

# Barcode process ---------------------------------------------------------

barcode_process <- function(.x) {
  tibble::tibble(barcode = .x) %>%
    dplyr::mutate(
      sample = stringr::str_sub(string = barcode, start = 1, end = 12),
      type = stringr::str_sub(string = barcode, start = 14, end = 15)
    )
}

filter_tumor_normal <- function(.x) {
  .x %>%
    dplyr::filter(type %in% c("01", "11")) %>%
    dplyr::mutate(type = ifelse(type == "01", "Tumor", "Normal"))
}

paired_sample <- function(.x) {
  .x %>%
    dplyr::group_by(sample) %>%
    dplyr::filter(n() >= 2, length(unique(type)) == 2) %>%
    dplyr::ungroup()
}

# Filter symbol -----------------------------------------------------------

filter_symbol <- function(.x, .gs) {
  .x %>%
    dplyr::filter(symbol %in% .gs)
}


# Filter sig and get rank ----------------------------------------------

filter_fc_pval <- function(.x) {
  .x %>%
    dplyr::filter(abs(log2(fc)) >= log2(3 / 2), fdr <= 0.05) %>%
    dplyr::mutate(p.value = -log10(p.value)) %>%
    dplyr::mutate(p.value = ifelse(p.value > 15, 15, p.value)) %>%
    dplyr::mutate(fdr = -log10(fdr)) %>%
    dplyr::mutate(fdr = ifelse(fdr > 15, 15, fdr)) %>%
    dplyr::mutate(fc = ifelse(fc < 1 / 8, 1 / 8, ifelse(fc > 8, 8, fc)))
}

filter_pattern <- function(fc, fdr) {
  if ((fc > 3 / 2) && (fdr < 0.05)) {
    return(1)
  } else if ((fc < 2 / 3) && (fdr < 0.05)) {
    return(-1)
  } else {
    return(0)
  }
}

get_pattern <- function(.x) {
  .x %>%
    dplyr::mutate(expr_pattern = purrr::map2_dbl(fc, fdr, filter_pattern)) %>%
    dplyr::select(cancer_types, symbol, expr_pattern) %>%
    tidyr::spread(key = cancer_types, value = expr_pattern) %>%
    dplyr::mutate_if(.predicate = is.numeric, .fun = dplyr::funs(ifelse(is.na(.), 0, .)))
}

get_gene_rank <- function(pattern) {
  pattern %>%
    dplyr::rowwise() %>%
    dplyr::do(
      symbol = .$symbol,
      rank = unlist(.[-1], use.names = F) %>% sum(),
      up = (unlist(.[-1], use.names = F) == 1) %>% sum(),
      down = (unlist(.[-1], use.names = F) == -1) %>% sum()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest() %>%
    dplyr::mutate(up_p = up / 14, down_p = down / 14, none = 1 - up_p - down_p) %>%
    dplyr::arrange(rank)
}

get_cancer_types_rank <- function(pattern) {
  pattern %>%
    dplyr::summarise_if(.predicate = is.numeric, dplyr::funs(sum(abs(.)))) %>%
    tidyr::gather(key = cancer_types, value = rank) %>%
    dplyr::arrange(dplyr::desc(rank))
}


# Clean expr ---------------------------------------------------------------

clean_expr <- function(.expr, .gs) {
  .expr %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = expr,
        .f = filter_symbol,
        .gs = .gs
      )
    ) %>%
    dplyr::mutate(
      fc_pval = purrr::map(
        .x = expr,
        .f = function(.x) {

          # tumor normal barcode paired
          colnames(.x)[-1] %>%
            barcode_process() %>%
            filter_tumor_normal() %>%
            paired_sample() -> .sample

          .x %>%
            dplyr::select(1, .sample$barcode) %>%
            tidyr::gather(key = barcode, value = expr, -1) %>%
            dplyr::left_join(.sample, by = "barcode") %>%
            tidyr::drop_na(expr) -> .d

          .d %>%
            dplyr::group_by(symbol) %>%
            dplyr::do(broom::tidy(t.test(expr ~ type, data = .))) %>%
            dplyr::ungroup() %>%
            dplyr::select(symbol, p.value) %>%
            dplyr::mutate(fdr = p.adjust(p.value, method = "fdr")) -> .d_pval

          .d %>%
            dplyr::group_by(symbol, type) %>%
            dplyr::summarise(mean = mean(expr)) %>%
            tidyr::spread(key = type, mean) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(fc = (Tumor + 0.1) / (Normal + 0.1)) -> .d_fc

          .d_fc %>%
            dplyr::inner_join(.d_pval, by = "symbol") %>%
            dplyr::mutate(n_normal = sum(.sample$type == "Normal"), n_tumor = sum(.sample$type == "Tumor"))
        }
      )
    ) %>%
    dplyr::select(cancer_types, fc_pval) %>%
    tidyr::unnest()
}


# Clean data datatable ----------------------------------------------------

expr_clean_datatable <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      order = list(list(5, "asc"), list(7, "desc"), list(6, "desc")),
      dom = "Bfrtip",
      buttons = c("copy", "csv", "print")
    ),
    rownames = FALSE,
    colnames = c("Cancer Types", "Symbol", "Normal expr.", "Tumor expr.", "Fold Change", "P-value", "FDR", "#Nomal", "#Tumor"),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>%
    DT::formatSignif(columns = c("Normal", "Tumor", "fc", "p.value", "fdr"), digits = 2) %>%
    DT::formatRound(columns = c("Normal", "Tumor", "fc", "p.value", "fdr"), 2)
}
# Expr bubble plot --------------------------------------------------------

expr_buble_plot <- function(.expr) {
  .expr %>% filter_fc_pval() -> expr_clean_filter
  .expr %>% get_pattern() -> expr_clean_pattern
  expr_clean_pattern %>% get_cancer_types_rank() -> cancer_rank
  expr_clean_pattern %>% get_gene_rank() -> gene_rank

  CPCOLS <- c("#000080", "#F8F8FF", "#CD0000")
  expr_clean_filter %>%
    ggplot(aes(x = cancer_types, y = symbol)) +
    geom_point(aes(size = fdr, col = log2(fc))) +
    scale_color_gradient2(
      low = CPCOLS[1],
      mid = CPCOLS[2],
      high = CPCOLS[3],
      midpoint = 0,
      na.value = "white",
      breaks = seq(-3, 3, length.out = 5),
      labels = c("<= -3", "-1.5", "0", "1.5", ">= 3"),
      name = "log2 FC"
    ) +
    scale_size_continuous(
      limit = c(-log10(0.05), 15),
      range = c(1, 6),
      breaks = c(-log10(0.05), 5, 10, 15),
      labels = c("0.05", latex2exp::TeX("$10^{-5}$"), latex2exp::TeX("$10^{-10}$"), latex2exp::TeX("$< 10^{-15}$")),
      name = "FDR"
    ) +
    scale_y_discrete(limit = gene_rank$symbol) +
    scale_x_discrete(limit = cancer_rank$cancer_types) +
    theme(
      panel.background = element_rect(colour = "black", fill = "white"),
      panel.grid = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.major = element_line(
        colour = "grey",
        linetype = "dashed",
        size = 0.2
      ),
      axis.title = element_blank(),
      axis.ticks = element_line(color = "black"),
      # axis.text.y = element_text(color = gene_rank$color),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),

      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.key = element_rect(fill = "white", colour = "black")
    )
}



# Survival bubble plot ----------------------------------------------------
fun_rank_cancer <- function(pattern){
  pattern %>% 
    dplyr::summarise_if(.predicate = is.numeric, dplyr::funs(sum(., na.rm = T))) %>%
    tidyr::gather(key = cancer_types, value = rank) %>%
    dplyr::arrange(dplyr::desc(rank))
} #get cancer rank
fun_rank_gene <- function(pattern){
  pattern %>% 
    dplyr::rowwise() %>%
    dplyr::do(
      symbol = .$symbol,
      rank =  unlist(.[-1], use.names = F) %>% sum(na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest() %>%
    dplyr::arrange(rank)
}


survival_bubble_plot <- function(.survival_clean) {
  print(.survival_clean)
  .survival_clean %>%
    dplyr::select(cancer_types, symbol) %>% 
    dplyr::mutate(n = 1) %>% 
    tidyr::spread(key = cancer_types, value = n) -> pattern
  
  cancer_rank <- pattern %>% fun_rank_cancer()
  gene_rank <- pattern %>% fun_rank_gene()
  .survival_clean %>% 
    ggplot(aes(x = cancer_types, y = symbol, color = status)) +
    geom_point(aes(size = -log10(p.value))) +
    scale_x_discrete(limit = cancer_rank$cancer_types) +
    scale_y_discrete(limit = gene_rank$symbol) +
    scale_size_continuous(name = "P-value") +
    theme(
      panel.background = element_rect(colour = "black", fill = "white"),
      panel.grid = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.major = element_line(
        colour = "grey",
        linetype = "dashed",
        size = 0.2
      ),
      
      axis.title = element_blank(),
      axis.ticks = element_line(color = "black"),
      
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.key = element_rect(fill = "white", colour = "black")
    ) +
    scale_color_manual(name = "Survival Worse", values = c("#e31a1c", "#1f78b4")) 
}

subtype_bubble_plot <- function(.subtype_clean){
  .subtype_clean %>% 
    dplyr::select(cancer_types, symbol) %>% 
    dplyr::mutate(n = 1) %>% 
    tidyr::spread(key = cancer_types, value = n) -> pattern
  cancer_rank <- pattern %>% fun_rank_cancer()
  gene_rank <- pattern %>% fun_rank_gene() 
  
  .subtype_clean %>% 
    ggplot(aes(x = cancer_types, y = symbol, color = cancer_types)) +
    geom_point(aes(size = -log10(p.value))) +
    scale_x_discrete(limit = cancer_rank$cancer_types) +
    scale_y_discrete(limit = gene_rank$symbol) +
    scale_size_continuous(name = "P-value") +
    theme(
      panel.background = element_rect(colour = "black", fill = "white"),
      panel.grid = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.major = element_line(
        colour = "grey",
        linetype = "dashed",
        size = 0.2
      ),
      
      axis.title = element_blank(),
      axis.ticks = element_line(color = "black"),
      
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.key = element_rect(fill = "white", colour = "black")
    ) 
}
