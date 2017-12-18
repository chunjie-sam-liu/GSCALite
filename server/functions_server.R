# source by server.R
# saved as functions_server.R



# Check input gene set ----------------------------------------------------

check_gene_set <- function(.s){
  .err <- character()
  .war <- character()
  
  if (!stringr::str_detect(string = .s, pattern = ",")) {
    .err <- c(.err, "Error: Please input the gene set with comma separate!")
  }
  
  .s %>%
    stringr::str_replace_all(pattern = "\\n", replacement = "") %>% 
    stringr::str_split(pattern = ",", simplify = T) %>% 
    .[1,] %>% 
    stringr::str_trim(side = "both") -> .ss
  
  if (!dplyr::between(length(.ss), 5, 200)) {
    .err <- c(.err, "Error: The number of genes should be more than 5 and less than 200.")
  }
  
  return(list(errors = .err, warnings = .war, gene_set = .ss))
}


# Validate gene with TCGA gene symbol -------------------------------------


validate_gene_set <- function(.v, user_dir = user_dir, user_logs = user_logs) {
  .err <- character()
  .war <- character()
  .log_file <- file.path(user_dir, user_logs$gene_set)
  
  # raw input gene set length
  .v_d <- .v[.v != ""] %>% unique()
  .v_d %in% gene_symbol -> .inter
  .v_d[.inter] -> .m
  .v_d[!.inter] -> .n_m
  if (length(.m) == 0) {
    .err = "Error: Please input valid gene symbol."
  }
  
  .log <- c(
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Input total gene set number is {length(.v)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Unique gene set number is {length(.m)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("#Total input gene set: {paste0(.v, collapse = ', ')}"),
    glue::glue("#Validated genes: {paste0(.m, collapse = ', ')}"),
    glue::glue("#Invalidated genes: {paste0(.n_m, collapse = ', ')}")
  )
  write(x = .log, file = .log_file, append = TRUE)
  list(gene_set = .m, errors = .err, warnings = .war)
}


# cnv bar data prepare ----------------------------------------------------

fn_gen_combined_core_atg <- function(cancer_types, filter_cnv, g_list, n) {
  # cancer_types <- "KIRC"
  # filter_cnv <- gene_list_cancer_cnv$filter_cnv[[1]]
  # g_list <-gene_list
  # n=1
  filter_cnv %>%
    dplyr::semi_join(g_list, by = "symbol") %>%
    tidyr::drop_na() %>%
    tidyr::gather(key = barcode, value = gistic, -symbol) %>%
    tidyr::spread(key = symbol, value = gistic) %>%
    dplyr::select(-barcode) -> .d
  
  n_sample <- nrow(.d)
  
  .d %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. == -n)) %>%
    nrow() -> .del
  .d %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. == -n)) %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. == n)) %>%
    nrow() -> .sub_d
  
  .d %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. == n)) %>%
    nrow() -> .amp
  .d %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. == n)) %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. == -n)) %>%
    nrow() -> .sub_a
  
  tibble::tibble(del_a = .del / n_sample, del_s = (.del - .sub_d) / n_sample, amp_a = .amp / n_sample, amp_s = (.amp - .sub_a) / n_sample)
}
# extract gene set from TCGA data -----------------------------------------

filter_gene_list <- function(.x, gene_list) {
  gene_list %>%
    dplyr::select(symbol) %>%
    dplyr::left_join(.x, by = "symbol")
}

