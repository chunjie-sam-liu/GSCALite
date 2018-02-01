
# Library -----------------------------------------------------------------

library(magrittr)

file_path <- "/data/TCGA/TCGA_data/pancan33_expr.rds.gz"
gs_path <- "/data/GSCALite/01_gene_symbol.rds.gz"

# Load data ---------------------------------------------------------------
gs <- readr::read_rds(path = gs_path)
expr <- readr::read_rds(path = file_path)

# Filter gene symbol ----


expr %>%
  dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) {
        .x %>%
          dplyr::filter(symbol %in% gs) %>%
          dplyr::select(-entrez_id) %>%
          dplyr::distinct(symbol, .keep_all = TRUE)
      }
    )
  ) -> expr_filter

source(file.path(here::here(), "functions", "data_function.R"))
expr_filter %>%
  dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) {
        colnames(.x)[-1] %>%
          barcode_process() %>%
          filter_tumor_normal() %>%
          paired_sample() -> .sample


        .sample_s <- table(.sample$type) %>% as.numeric()
        if (gtools::invalid(.sample_s) || any(.sample_s < 10)) return(NULL)

        .x
      }
    )
  ) %>%
  dplyr::filter(!purrr::map_lgl(expr, is.null)) %>%
  dplyr::pull(cancer_types) -> paired_cancer_types

paired_cancer_types %>% readr::write_rds(path = file.path("/data/GSCALite/TCGA/expr", "paired_cancer_types.rds.gz"), compress = "gz")
expr_filter %>% readr::write_rds(path = file.path("/data/GSCALite/TCGA/expr", "pancan33_expr_filtered.rds.gz"), compress = "gz")



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

fc_calc <- function(.x) {
  
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

expr %>% 
  dplyr::filter(cancer_types %in% paired_cancer_types) %>%
  head(1) %>% 
  dplyr::mutate(
    fc_pval = purrr::map(
      .x = expr,
      .f = fc_calc
    )
  )


cl <- 14
cluster <- multidplyr::create_cluster(14)


expr %>% 
  dplyr::filter(cancer_types %in% paired_cancer_types) %>% 
  head(14) %>% 
  multidplyr::partition(cluster = cluster) %>%
  multidplyr::cluster_library("magrittr") %>%
  multidplyr::cluster_assign_value("barcode_process", barcode_process) %>% 
  multidplyr::cluster_assign_value("filter_tumor_normal", filter_tumor_normal) %>% 
  multidplyr::cluster_assign_value("paired_sample", paired_sample) %>% 
  multidplyr::cluster_assign_value("fc_calc", fc_calc) %>% 
  dplyr::mutate(fc_pval = purrr::map(.x = expr, .f = fc_calc)) %>% 
  dplyr::collect() %>% 
  dplyr::select(-expr) %>% 
  dplyr::ungroup() %>%
  dplyr::select(-PARTITION_ID) -> expr_fc_pval

# expr_fc_pval %>% readr::write_rds("/data/GSCALite/TCGA/expr/pancan14_expr_fc_pval.rds.gz", compress = "gz")
expr_fc_pval %>% tidyr::unnest() %>% readr::write_rds("/data/GSCALite/TCGA/expr/pancan14_expr_fc_pval.rds.gz", compress = "gz") -> foo
foo


expr_clean <- readr::read_rds(path = "/home/liucj/github/GSCALite/expr_clean.rds.gz")
expr_clean -> .expr

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
  ) -> p

ggsave(filename = "expr_bubble.eps", plot = p, device = "eps", path = "/home/liucj/github/GSCALite", width = 4, height = 4, useDingbats = FALSE)


t_gdsc <- readr::read_rds(file.path(tcga_path, "Drug", "drug_target_gdsc.rds.gz")) %>%
  tidyr::unnest() %>%
  dplyr::select(drug_name, target_pathway) %>%
  dplyr::distinct() %>%
  dplyr::group_by(target_pathway) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::ungroup()

print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start Load GDSC @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
drug_gdsc <- readr::read_rds(file.path(tcga_path, "Drug", "gdsc_exp_spearman.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} End Load GDSC @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start GDSC Plot @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
gs <- c("A2M", "ACE", "ANGPT2", "BPI", "CD1B", "CDR1", "EGR2", "EGR3", "HBEGF", "HERPUD1", "MCM2", "MRE11A", "PCTP", "PODXL", "PPAP2B", "PPY", "PTGS2", "RCAN1", "SLC4A7", "THBD")
drug_gdsc %>%
  dplyr::filter(symbol %in% gs) %>%
  dplyr::mutate(cor_drug = purrr::map(.x = drug, .f = fn_filter_drug)) %>%
  tidyr::unnest(cor_drug) -> gdsc_gene_list_sig_drug

gdsc_gene_list_sig_drug %>%
  dplyr::mutate(
    fdr = ifelse(-log10(fdr) > 40, 40, -log10(fdr)),
    cor_sprm = ifelse(cor_sprm > 0.4, 0.4, cor_sprm),
    cor_sprm = ifelse(cor_sprm < -0.4, -0.4, cor_sprm)
  ) %>%
  dplyr::left_join(t_gdsc, by = "drug_name") -> gdsc_plot_ready


gdsc_plot_ready %>%
  dplyr::group_by(symbol) %>%
  dplyr::summarise(cor_sum = sum(cor_sprm)) %>%
  dplyr::arrange(cor_sum) -> gdsc_gene_rank

gdsc_plot_ready %>%
  dplyr::distinct(drug_name, target_pathway, count) %>%
  dplyr::group_by(target_pathway) %>%
  dplyr::mutate(per = n() / count) %>%
  dplyr::arrange(per) %>%
  dplyr::ungroup() %>%
  dplyr::select(drug_name, target_pathway, per) -> gdsc_drug_per

gdsc_plot_ready %>%
  dplyr::left_join(gdsc_drug_per, by = c("drug_name", "target_pathway")) %>%
  dplyr::group_by(drug_name) %>%
  dplyr::mutate(drug_count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(per, target_pathway, drug_count) %>%
  dplyr::select(drug_name, target_pathway, drug_count, per) %>%
  dplyr::distinct() %>%
  dplyr::mutate(target_pathway = stringr::str_to_title(target_pathway)) -> gdsc_drug_rank_pre

gdsc_drug_rank_pre %>%
  dplyr::distinct(target_pathway, per) %>%
  dplyr::arrange(per, target_pathway) -> .foo

pathway_color <-
  .foo %>%
  dplyr::mutate(color = ggthemes::gdocs_pal()(nrow(.foo)))

gdsc_drug_rank_pre %>%
  dplyr::select(-per) %>%
  dplyr::left_join(pathway_color, by = "target_pathway") -> drug_rank

p <-
  gdsc_plot_ready %>%
  ggplot(aes(x = symbol, y = drug_name, color = cor_sprm)) +
  geom_point(aes(size = fdr)) +
  scale_x_discrete(limits = gdsc_gene_rank$symbol, expand = c(0.025, 0.025)) +
  scale_y_discrete(limits = drug_rank$drug_name, expand = c(0.012, 0.012), position = "right") +
  scale_color_gradient2(
    name = "Spearman Correlation",
    high = "red",
    mid = "white",
    low = "blue"
  ) +
  scale_size_continuous(
    name = "FDR"
  ) +
  # ggthemes::theme_gdocs() +
  theme(
    panel.background = element_rect(color = "black", fill = "white", size = 0.1),
    panel.grid = element_line(colour = "grey", linetype = "dashed"),
    panel.grid.major = element_line(colour = "grey", linetype = "dashed", size = 0.2),
    
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 10, color = drug_rank$color),
    
    axis.ticks = element_line(color = "black"),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    # legend.key.width = unit(1,"cm"),
    # legend.key.heigh = unit(0.3,"cm"),
    legend.key = element_rect(fill = "white", colour = "black")
  ) + guides(
    color = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = 0.5,
      barwidth = 10
    )
  )
print(glue::glue("{paste0(rep('-', 10), collapse = '')} End GDSC Plot @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
p


ggsave(filename = "gdsc.svg", plot = p, device = "svg", path = "/home/liucj/github/GSCALite", width = 6, height = 17)
