
# Load library ------------------------------------------------------------
library(magrittr)



# Load data ---------------------------------------------------------------


expr <- readr::read_rds(path = "/data/TCGA/TCGA_data/pancan33_expr.rds.gz")

clinical <- readr::read_rds(path = "/data/TCGA/TCGA_data/pancan34_clinical.rds.gz")

# Analysis ----------------------------------------------------------------

expr_clinical <- 
  expr %>%
  dplyr::inner_join(clinical, by = "cancer_types")

fun_barcode <- function(.b){
  stringr::str_sub(
    string = .b,
    start = 1,
    end = 12
  )
} #get short barcode from long barcode
fun_tn_type <- function(.b){
  type <- .b %>% 
    stringr::str_split(pattern = "-", simplify = T) %>% 
    .[, 4] %>% 
    stringr::str_sub(1, 2)
} # get tumor and normal info
fun_expr_survival_merge <- function(filter_expr, clinical){
  # merge clinical and expr data
  filter_expr %>% 
    dplyr::select(-entrez_id) %>% 
    tidyr::gather(key = barcode, value = expr, -symbol) %>% 
    dplyr::mutate(type = fun_tn_type(barcode)) %>% 
    dplyr::filter(type == "01") %>% 
    dplyr::mutate(barcode = fun_barcode(barcode)) %>% 
    dplyr::select(symbol, barcode, expr)  %>% 
    dplyr::inner_join(clinical, by = "barcode") %>% 
    dplyr::select(symbol, barcode, expr, gender, race,time = os_days, status = os_status) %>% 
    dplyr::filter(!is.na(time), time > 0, !is.na(status)) %>% 
    dplyr::mutate(status = plyr::revalue(status, replace = c("Alive" = 0, "Dead" = 1))) %>%
    dplyr::mutate(status = as.numeric(status)) %>% 
    dplyr::mutate(expr = log2(expr + 1)) %>% 
    tidyr::drop_na(expr) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::mutate(group = as.factor(ifelse(expr <= median(expr),"Low", "High"))) %>% 
    dplyr::ungroup() -> expr_clinical_ready
} 


fun_draw_survival <- function(symbol, p.value, cancer_types, expr_clinical_ready){
  gene <- symbol
  p_val <- signif(-log10(p.value), digits = 3)
  fig_name <- paste(cancer_types, gene, p_val, "pdf", sep = ".")
  # print(fig_name)
  .d <- 
    expr_clinical_ready %>% 
    dplyr::filter(symbol == gene)
  
  .d_diff <- survival::survdiff(survival::Surv(time, status) ~ group, data = .d)
  
  kmp <- 1 - pchisq(.d_diff$chisq, df = length(levels(as.factor(.d$group))) - 1)
  
  if(kmp > 0.05) {return(NA)} else{
    fit_x <- survival::survfit(survival::Surv(time, status) ~ group, data = .d , na.action = na.exclude)
    survminer::ggsurvplot(fit_x, data = .d, pval=T, pval.method = T,
                          title = paste(paste(cancer_types, gene, sep = "-"), "Coxph =", signif(p.value, 2)),
                          xlab = "Survival in days",
                          ylab = 'Probability of survival')
    ggsave(filename = fig_name, device = "pdf", path = file.path(survival_path, "boxplot"), width = 6, height = 6)
  }
}

fun_clinical_test <- function(expr_clinical_ready, cancer_types){
  if(nrow(expr_clinical_ready) < 1){return(tibble::tibble())}
  print(cancer_types)
  expr_clinical_ready %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::do(
      broom::tidy(
        tryCatch(
          survival::coxph(survival::Surv(time, status) ~ expr, data = ., na.action = na.exclude),
          error = function(e){1},
          warning = function(e){1})
      )
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(p.value < 0.05) %>% 
    dplyr::select(symbol, estimate, p.value) %>% 
    dplyr::mutate(status = ifelse(estimate > 0, "H", "L"))-> d
  
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
  multidplyr::cluster_assign_value("fun_barcode", fun_barcode)  %>%
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

