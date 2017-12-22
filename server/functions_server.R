# source by server.R
# saved as functions_server.R



# Check input gene set ----------------------------------------------------

check_gene_set <- function(.s, status = status, error = error) {
  
  .s %>%
    stringr::str_split(pattern = "[^[:alnum:]]+", simplify = TRUE) %>% 
    .[1, ] %>%
    stringr::str_trim(side = "both") -> .ss
  
  if (length(.ss) == 1 && .ss == "") {
    error$gene_set <- c(error$gene_set, "Error: Input at least One symbol.")
    status$gene_set <- TRUE
  }
  
  if (!dplyr::between(length(.ss), 1, 200)) {
    error$gene_set <- c(error$gene_set, "Error: The number of genes should be less than 200.")
    status$gene_set <- TRUE
  }
  
  return(list(gene_set = .ss))
}


# Validate gene with TCGA gene symbol -------------------------------------


validate_gene_set <- function(.v, user_dir = user_dir, user_logs = user_logs, total_gene_symbol = total_gene_symbol) {
  .err <- character()
  .war <- character()
  .log_file <- file.path(user_dir, user_logs$gene_set)
  # raw input gene set length
  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = tolower, USE.NAMES = FALSE) 
  .v_dedup %in% names(total_gene_symbol) -> .inter
  
  .l <- list(match = total_gene_symbol[.v_dedup[.inter]], non_match = .v_dedup[!.inter])
  
  if (length(.l$match) == 0) {
    .err <- "Error: Please input valid gene symbol."
  }

  .log <- c(
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Input total gene set number is {length(.v)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Unique gene set number is {length(.v_dedup)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("#Total input gene set: {paste0(.v, collapse = ', ')}"),
    glue::glue("#Validated genes: {paste0(.l$match, collapse = ', ')}"),
    glue::glue("#Invalidated genes: {paste0(.l$non_match, collapse = ', ')}")
  )
  write(x = .log, file = .log_file, append = TRUE)
  list(gene_set = .l$match, errors = .err, warnings = .war)
}


# cnv bar data prepare ----------------------------------------------------


# threshold cnv -----------------------------------------------------------

fn_get_amplitue_threshold <- function(.x) {
  tibble::tibble(
    a_total = sum(.x > 0) / length(.x),
    d_total = sum(.x < 0) / length(.x),
    a_homo = sum(.x == 2) / length(.x),
    d_homo = sum(.x == -2) / length(.x),
    a_hete = sum(.x == 1) / length(.x),
    d_hete = sum(.x == -1) / length(.x)
  )
}

# get cnv percent ---------------------------------------------------------

fn_get_ad <- function(.d) {
  .d %>%
    unlist(use.name = F) %>%
    fn_get_amplitue_threshold()
}
fn_get_percent <- function(cancer_types, filter_cnv) {
  filter_cnv %>%
    tidyr::nest(-symbol) %>%
    dplyr::mutate(ad = purrr::map(data, .f = fn_get_ad)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(ad) %>%
    tibble::add_column(cancer_types = cancer_types, .before = 1)
}
fn_cnv_percecnt <- function(data) {
  data %>%
    dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_get_percent)) %>%
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-cancer_types, -filter_cnv) %>%
    tidyr::unnest(rs) -> gene_list_cnv_per
}


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


# get exclusive cnv -------------------------------------------------------
fn_cnv_exclusive <- function(V1, V2, .data, cancer_types) {
  # .data <- filter_cnv
  # V1 <- 'TP53'
  # V2 <- 'EZH2'
  .data %>%
    dplyr::filter(symbol %in% c(V1, V2)) %>%
    tidyr::gather(key = barcode, value = gistic, -symbol) %>%
    tidyr::spread(key = symbol, value = gistic) %>%
    dplyr::select(-barcode) -> .d
  .g_name <- colnames(.d)
  # colnames(.d) <- c("A", "B")
  name <- paste(c(cancer_types, .g_name), collapse = "_")
  .d %>%
    dplyr::filter_all(.vars_predicate = dplyr::all_vars(. == 0)) %>%
    nrow() -> nn
  .d %>%
    dplyr::filter_all(.vars_predicate = dplyr::all_vars(. != 0)) %>%
    nrow() -> aa
  .d %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. != 0)) %>%
    dplyr::filter_all(.vars_predicate = dplyr::any_vars(. == 0)) -> .d_an

  sum(.d_an %>% dplyr::pull(1) != 0) -> an
  sum(.d_an %>% dplyr::pull(2) != 0) -> na
  c(nn = nn, an = an, na = na, aa = aa) %>%
    cometExactTest::comet_exact_test(mutmatplot = F) -> p_val

  tibble::tibble(te = name, nn = nn, an = an, na = na, aa = aa, p_val = p_val)
}
fn_cnv_mutal_exclusive <- function(cancer_types, filter_cnv, cluster) {
  # cancer_types <- te$cancer_types
  # filter_cnv <- te$filter_cnv[[1]]
  filter_cnv %>%
    dplyr::pull(symbol) %>%
    combn(m = 2) %>%
    t() %>%
    dplyr::as_data_frame() -> .gene_pairs

  .gene_pairs %>%
    multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    multidplyr::cluster_assign_value("fn_cnv_exclusive", fn_cnv_exclusive) %>%
    multidplyr::cluster_assign_value("filter_cnv", filter_cnv) %>%
    multidplyr::cluster_assign_value("cancer_types", cancer_types) %>%
    dplyr::mutate(rs = purrr::map2(V1, V2, .f = fn_cnv_exclusive, .data = filter_cnv, cancer_types = cancer_types)) %>%
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) %>%
    dplyr::select(rs) %>%
    tidyr::unnest() %>%
    tidyr::separate(col = te, into = c("cancer_types", "g1", "g2")) -> .gene_pairs_pval

  .gene_pairs_pval %>%
    dplyr::mutate(fdr = p.adjust(p_val, method = "fdr"))
}

