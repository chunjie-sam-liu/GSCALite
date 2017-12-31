
# Load library ------------------------------------------------------------
library(magrittr)



# Load data ---------------------------------------------------------------


expr <- readr::read_rds(path = "/data/TCGA/TCGA_data/pancan33_expr.rds.gz")

clinical <- readr::read_rds(path = "/data/TCGA/TCGA_data/pancan34_clinical.rds.gz")

# Analysis ----------------------------------------------------------------

expr_clinical <-
  expr %>%
  dplyr::inner_join(clinical, by = "cancer_types")

fun_barcode <- function(.b) {
  stringr::str_sub(
    string = .b,
    start = 1,
    end = 12
  )
} # get short barcode from long barcode
fun_tn_type <- function(.b) {
  type <- .b %>%
    stringr::str_split(pattern = "-", simplify = T) %>%
    .[, 4] %>%
    stringr::str_sub(1, 2)
} # get tumor and normal info
fun_expr_survival_merge <- function(filter_expr, clinical) {
  # merge clinical and expr data
  filter_expr %>%
    dplyr::select(-entrez_id) %>%
    tidyr::gather(key = barcode, value = expr, -symbol) %>%
    dplyr::mutate(type = fun_tn_type(barcode)) %>%
    dplyr::filter(type == "01") %>%
    dplyr::mutate(barcode = fun_barcode(barcode)) %>%
    dplyr::select(symbol, barcode, expr) %>%
    dplyr::inner_join(clinical, by = "barcode") %>%
    dplyr::select(symbol, barcode, expr, gender, race, time = os_days, status = os_status) %>%
    dplyr::filter(!is.na(time), time > 0, !is.na(status)) %>%
    dplyr::mutate(status = plyr::revalue(status, replace = c("Alive" = 0, "Dead" = 1))) %>%
    dplyr::mutate(status = as.numeric(status)) %>%
    dplyr::mutate(expr = log2(expr + 1)) %>%
    tidyr::drop_na(expr) %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(group = as.factor(ifelse(expr <= median(expr), "Low", "High"))) %>%
    dplyr::ungroup() -> expr_clinical_ready
}


fun_draw_survival <- function(symbol, p.value, cancer_types, expr_clinical_ready) {
  gene <- symbol
  p_val <- signif(-log10(p.value), digits = 3)
  fig_name <- paste(cancer_types, gene, p_val, "pdf", sep = ".")
  # print(fig_name)
  .d <-
    expr_clinical_ready %>%
    dplyr::filter(symbol == gene)

  .d_diff <- survival::survdiff(survival::Surv(time, status) ~ group, data = .d)

  kmp <- 1 - pchisq(.d_diff$chisq, df = length(levels(as.factor(.d$group))) - 1)

  if (kmp > 0.05) {
    return(NA)
  } else {
    fit_x <- survival::survfit(survival::Surv(time, status) ~ group, data = .d, na.action = na.exclude)
    survminer::ggsurvplot(
      fit_x, data = .d, pval = T, pval.method = T,
      title = paste(paste(cancer_types, gene, sep = "-"), "Coxph =", signif(p.value, 2)),
      xlab = "Survival in days",
      ylab = "Probability of survival"
    )
    ggsave(filename = fig_name, device = "pdf", path = file.path(survival_path, "boxplot"), width = 6, height = 6)
  }
}

fun_clinical_test <- function(expr_clinical_ready, cancer_types) {
  if (nrow(expr_clinical_ready) < 1) {
    return(tibble::tibble())
  }
  print(cancer_types)
  expr_clinical_ready %>%
    dplyr::group_by(symbol) %>%
    dplyr::do(
      broom::tidy(
        tryCatch(
          survival::coxph(survival::Surv(time, status) ~ expr, data = ., na.action = na.exclude),
          error = function(e) {
            1
          },
          warning = function(e) {
            1
          }
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(p.value < 0.05) %>%
    dplyr::select(symbol, estimate, p.value) %>%
    dplyr::mutate(status = ifelse(estimate > 0, "H", "L")) -> d

  # d %>%
  #   dplyr::select(symbol, p.value) %>%
  #   purrr::pwalk(fun_draw_survival, cancer_types = cancer_types, expr_clinical_ready = expr_clinical_ready)

  return(d)
}


expr_clinical %>%
  dplyr::rename(filter_expr = expr) %>%
  head(1) %>%
  dplyr::mutate(merged_clean = purrr::map2(filter_expr, clinical, fun_expr_survival_merge)) %>%
  dplyr::select(-filter_expr, -clinical) %>%
  dplyr::mutate(diff_pval = purrr::map2(merged_clean, cancer_types, fun_clinical_test)) %>%
  dplyr::select(-merged_clean)

cl <- parallel::detectCores()
cluster <- multidplyr::create_cluster(floor(cl * 5 / 6))
expr_clinical %>%
  dplyr::rename(filter_expr = expr) %>%
  multidplyr::partition(cluster = cluster) %>%
  multidplyr::cluster_library("magrittr") %>%
  multidplyr::cluster_library("ggplot2") %>%
  multidplyr::cluster_assign_value("fun_barcode", fun_barcode) %>%
  multidplyr::cluster_assign_value("fun_tn_type", fun_tn_type) %>%
  multidplyr::cluster_assign_value("fun_expr_survival_merge", fun_expr_survival_merge) %>%
  multidplyr::cluster_assign_value("fun_clinical_test", fun_clinical_test) %>%
  # multidplyr::cluster_assign_value("fun_draw_survival", fun_draw_survival) %>%
  # multidplyr::cluster_assign_value("survival_path", survival_path) %>%
  dplyr::mutate(merged_clean = purrr::map2(filter_expr, clinical, fun_expr_survival_merge)) %>%
  dplyr::select(-filter_expr, -clinical) %>%
  dplyr::mutate(diff_pval = purrr::map2(merged_clean, cancer_types, fun_clinical_test)) %>%
  dplyr::select(-merged_clean) %>%
  dplyr::collect() %>%
  dplyr::as_tibble() %>%
  dplyr::ungroup() %>%
  dplyr::select(-PARTITION_ID) %>%
  tidyr::unnest(diff_pval) -> expr_clinical_sig_pval
on.exit(parallel::stopCluster(cluster))

expr_clinical_sig_pval %>% readr::write_rds(path = "/data/GSCALite/TCGA/expr/expr_survival.rds.gz", compress = "gz")
expr_clinical_sig_pval <- readr::read_rds(path = "/data/GSCALite/TCGA/expr/expr_survival.rds.gz")

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
} # get gene rank

expr_clinical_sig_pval %>% 
  dplyr::select(cancer_types, symbol) %>% 
  dplyr::mutate(n = 1) %>% 
  tidyr::spread(key = cancer_types, value = n) -> pattern
cancer_rank <- pattern %>% fun_rank_cancer()
gene_rank <- pattern %>% fun_rank_gene() 

expr_clinical_sig_pval %>% 
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
    axis.text.y = element_text(color = gene_rank$color),
    
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.key = element_rect(fill = "white", colour = "black")
  ) +
  ggthemes::scale_color_gdocs(name = "Surivival Worse")-> p




# subtype -----------------------------------------------------------------
tcga_path <- "/data/TCGA/TCGA_data"
clinical_subtype <-
  readr::read_rds(path = file.path(tcga_path, "pancan34_clinical_subtype.rds.gz")) %>%
  dplyr::select(-n)

expr_subtype <-
  expr %>%
  dplyr::inner_join(clinical_subtype, by = "cancer_types")

fun_subtype_test <- function(expr_subtype_ready) {
  expr_subtype_ready %>%
    tidyr::drop_na(expr) %>%
    dplyr::group_by(symbol) -> d

  d %>%
    dplyr::ungroup() %>%
    dplyr::group_by(symbol, subtype) %>%
    dplyr::mutate(l = n() > 10) -> tl

  if (!all(tl$l)) {
    return(tibble::tibble())
  } else {
    d %>%
      dplyr::ungroup() %>%
      dplyr::distinct(subtype) %>%
      .$subtype %>%
      length() -> n_subtype
    # n_subtype == 2 t.test
    # n_subtype >3 anova
    if (n_subtype == 2) {
      d %>%
        dplyr::do(broom::tidy(t.test(log2(expr + 1) ~ subtype, data = .))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(fdr = p.adjust(p.value, method = "fdr")) %>%
        dplyr::select(symbol, p.value, fdr) %>%
        dplyr::filter(p.value < 0.01, fdr < 0.1)
    } else {
      d %>%
        dplyr::do(broom::tidy(oneway.test(log2(expr + 1) ~ subtype, data = .))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(fdr = p.adjust(p.value, method = "fdr")) %>%
        dplyr::select(symbol, p.value, fdr) %>%
        dplyr::filter(p.value < 0.01, fdr < 0.1)
    }
  }
}

fun_expr_subtype_merge <- function(filter_expr, subtype) {
  # merge clinical and expr data
  filter_expr %>%
    dplyr::select(-entrez_id) %>%
    tidyr::gather(key = barcode, value = expr, -symbol) %>%
    dplyr::mutate(type = fun_tn_type(barcode)) %>%
    dplyr::filter(type == "01") %>%
    dplyr::mutate(barcode = fun_barcode(barcode)) %>%
    dplyr::select(symbol, barcode, expr) %>%
    dplyr::inner_join(subtype, by = "barcode") -> expr_subtype_ready
}

expr_subtype %>%
  head(1) %>%
  dplyr::rename(filter_expr = expr) %>%
  dplyr::mutate(merged_clean = purrr::map2(filter_expr, subtype, fun_expr_subtype_merge)) %>%
  dplyr::select(-filter_expr, -subtype) %>%
  dplyr::mutate(diff_pval = purrr::map(merged_clean, fun_subtype_test))


cl <- parallel::detectCores()
cluster <- multidplyr::create_cluster(floor(cl * 5 / 6))
expr_subtype %>%
  dplyr::rename(filter_expr = expr) %>%
  multidplyr::partition(cluster = cluster) %>%
  multidplyr::cluster_library("magrittr") %>%
  multidplyr::cluster_assign_value("fun_barcode", fun_barcode) %>%
  multidplyr::cluster_assign_value("fun_tn_type", fun_tn_type) %>%
  multidplyr::cluster_assign_value("fun_expr_subtype_merge", fun_expr_subtype_merge) %>%
  multidplyr::cluster_assign_value("fun_subtype_test", fun_subtype_test) %>%
  dplyr::mutate(merged_clean = purrr::map2(filter_expr, subtype, fun_expr_subtype_merge)) %>%
  dplyr::select(-filter_expr, -subtype) %>%
  dplyr::mutate(diff_pval = purrr::map(merged_clean, fun_subtype_test)) %>%
  dplyr::select(-merged_clean) %>%
  dplyr::collect() %>%
  dplyr::as_tibble() %>%
  dplyr::ungroup() %>%
  dplyr::select(-PARTITION_ID) %>%
  tidyr::unnest(diff_pval, .drop = F) -> expr_subtype_sig_pval
parallel::stopCluster(cluster)
expr_subtype_sig_pval %>% readr::write_rds(path = "/data/GSCALite/TCGA/expr/expr_subtype.rds.gz", compress = "gz")
expr_subtype_sig_pval <- readr::read_rds(path = "/data/GSCALite/TCGA/expr/expr_subtype.rds.gz")

# Stage -------------------------------------------------------------------

clinical_stage <-
  readr::read_rds(path = file.path(tcga_path, "pancan34_clinical_stage.rds.gz")) %>%
  dplyr::filter(n >= 40) %>%
  dplyr::select(-n)


expr_stage <-
  expr %>%
  dplyr::inner_join(clinical_stage, by = "cancer_types")

fun_expr_stage_merge <- function(filter_expr, stage) {
  # merge clinical and expr data
  filter_expr %>%
    dplyr::select(-entrez_id) %>%
    tidyr::gather(key = barcode, value = expr, -symbol) %>%
    dplyr::mutate(type = fun_tn_type(barcode)) %>%
    dplyr::filter(type == "01") %>%
    dplyr::mutate(barcode = fun_barcode(barcode)) %>%
    dplyr::select(symbol, barcode, expr) -> expr_clean
  expr_clean %>% dplyr::inner_join(stage, by = "barcode") -> expr_stage_ready
} #
fn_get_order <- function(.d) {
  .d %>%
    dplyr::group_by(stage) %>%
    dplyr::summarise(me = mean(expr)) %>%
    .$me %>%
    rank() -> .d_m

  if (identical(.d_m, c(1, 2, 3, 4))) {
    return(1)
  } else if (identical(.d_m, c(4, 3, 2, 1))) {
    return(2)
  } else {
    return(3)
  }
}
fun_stage_test <- function(expr_stage_ready) {
  expr_stage_ready %>%
    tidyr::drop_na(expr) %>%
    dplyr::group_by(symbol, stage) %>%
    dplyr::mutate(l = n() > 10) -> tl

  if (!all(tl$l)) {
    return(tibble::tibble())
  } else {
    expr_stage_ready %>%
      tidyr::drop_na(expr) %>%
      dplyr::group_by(symbol) %>%
      dplyr::do(broom::tidy(oneway.test(log2(expr + 1) ~ stage, data = .))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(fdr = p.adjust(p.value, method = "fdr")) %>%
      dplyr::select(symbol, p.value, fdr) -> diff_pval

    expr_stage_ready %>%
      tidyr::drop_na(expr) %>%
      tidyr::nest(-symbol) %>%
      dplyr::mutate(order = purrr::map_dbl(data, .f = fn_get_order)) %>%
      dplyr::select(-data) -> symbol_order

    diff_pval %>%
      dplyr::inner_join(symbol_order, by = "symbol")
  }
}



expr_stage %>%
  dplyr::rename(filter_expr = expr) %>%
  dplyr::filter(cancer_types == "BRCA") %>%
  dplyr::mutate(merged_clean = purrr::map2(filter_expr, stage, fun_expr_stage_merge)) %>%
  dplyr::select(-filter_expr, -stage) %>%
  dplyr::mutate(diff_pval = purrr::map(merged_clean, fun_stage_test)) %>%
  tidyr::unnest(diff_pval, .drop = F) -> te
