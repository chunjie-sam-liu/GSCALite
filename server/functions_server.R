# source by server.R
# saved as functions_server.R

# Check input gene set ----------------------------------------------------
check_gene_set <- function(.s, status = status, error = error) {
  .s %>%
    stringr::str_split(pattern = "[^[:alnum:]]+", simplify = TRUE) %>%
    .[1, ] %>%
    stringr::str_trim(side = "both") -> .ss

  if (!dplyr::between(length(.ss), 1, 200)) {
    error$gene_set <- "Error: The number of genes should be less than 200."
    status$trigger <- if (status$trigger == TRUE) FALSE else TRUE
    status$gene_set <- FALSE
  }

  .ss
}


# Validate gene with TCGA gene symbol -------------------------------------
validate_gene_set <- function(.v, user_dir = user_dir, user_logs = user_logs, total_gene_symbol = total_gene_symbol, status = status, error = error, gene_set = gene_set) {
  .log_file <- file.path(user_dir, user_logs$gene_set)


  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = tolower, USE.NAMES = FALSE)
  .v_dedup %in% names(total_gene_symbol) -> .inter


  gene_set$match <- total_gene_symbol[.v_dedup[.inter]]
  gene_set$non_match <- .v_dedup[!.inter]
  gene_set$n_match <- length(total_gene_symbol[.v_dedup[.inter]])
  gene_set$n_non_match <- length(.v_dedup[!.inter])
  gene_set$n_total <- length(total_gene_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])

  if (length(gene_set$match) == 0) {
    error$gene_set <- "Error: Please input at least one valid gene symbol."
    status$trigger <- if (status$trigger == TRUE) FALSE else TRUE
    status$gene_set <- FALSE
  }

  .log <- c(
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Input total gene set number is {length(gene_set$n_total)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Unique gene set number is {length(.v_dedup)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("#Total input gene set: {paste0(.v, collapse = ', ')}"),
    glue::glue("#Validated genes: {paste0(gene_set$match, collapse = ', ')}"),
    glue::glue("#Invalidated genes: {paste0(gene_set$non_match, collapse = ', ')}")
  )
  write(x = .log, file = .log_file, append = TRUE)
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
    dplyr::filter(symbol %in% g_list) %>%
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
  .x %>%
    dplyr::filter(symbol %in% gene_list)
  # gene_list %>%
  #   dplyr::select(symbol) %>%
  #   dplyr::left_join(.x, by = "symbol")
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


# rppa line contact faction -----------------------------------------------

get_rppa_text <- function(data) {
  data %>%
    dplyr::pull(symbol) %>%
    unique() -> gene.text
  data %>%
    dplyr::pull(cancer_types) %>%
    unique() -> cancer.text
  data %>%
    dplyr::pull(pathway) %>%
    unique() -> pathway.text

  c.text <- data.frame(x = 1, y = 1, text = "test", type = "test")
  g.l <- data$symbol %>% unique() %>% length()
  c.l <- data$cancer_types %>% unique() %>% length()
  p.l <- data$pathway %>% unique() %>% length()

  # condition 1: gene is more -----------------------------------------------

  if (g.l >= c.l & g.l >= p.l) {
    for (i in 1:length(gene.text)) {
      data.frame(x = 4, y = 2 * i - 1, text = gene.text[i], type = "gene") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }
    c.text %>%
      dplyr::filter(type == "gene") %>%
      dplyr::select(y) %>%
      max() -> g.m

    g.m / c.l -> c.i
    for (i in 1:length(cancer.text)) {
      data.frame(x = 1, y = i * c.i - 1, text = cancer.text[i], type = "cancer") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }

    g.m / 10 -> p.i
    for (i in 1:length(pathway.text)) {
      data.frame(x = 7, y = i * p.i - 1, text = pathway.text[i], type = "pathway") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }
  }

  # condition 2: cancer is more ---------------------------------------------

  if (c.l >= g.l & c.l >= p.l) {
    for (i in 1:length(cancer.text)) {
      data.frame(x = 1, y = 2 * i - 1, text = cancer.text[i], type = "cancer") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }
    c.text %>%
      dplyr::filter(type == "cancer") %>%
      dplyr::select(y) %>%
      max() -> c.m

    c.m / g.l -> g.i
    for (i in 1:length(gene.text)) {
      data.frame(x = 4, y = i * g.i - 1, text = gene.text[i], type = "gene") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }

    c.m / 10 -> p.i
    for (i in 1:length(pathway.text)) {
      data.frame(x = 7, y = i * p.i - 1, text = pathway.text[i], type = "pathway") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }
  }

  # condition 3: pathway is more --------------------------------------------

  if (p.l >= c.l & p.l >= g.l) {
    for (i in 1:length(pathway.text)) {
      data.frame(x = 7, y = i * 2 - 1, text = pathway.text[i], type = "pathway") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }

    c.text %>%
      dplyr::filter(type == "pathway") %>%
      dplyr::select(y) %>%
      max() -> p.m

    p.m / c.l -> c.i
    for (i in 1:length(cancer.text)) {
      data.frame(x = 1, y = i * c.i - 1, text = cancer.text[i], type = "cancer") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }

    p.m / g.l -> g.i
    for (i in 1:length(gene.text)) {
      data.frame(x = 4, y = i * g.i - 1, text = gene.text[i], type = "gene") -> tmp.text
      rbind(c.text, tmp.text) -> c.text
    }
  }
  return(c.text[-1, ])
}

get_rppa_seg <- function(data,cancer_text) {
  # name <- c("x1","y1","x2","y2","Cancer","Regulation")
  # print(n)
  data[1,1] %>% as.character() -> cancer
  data[1,2] %>% as.character() -> gene
  data[1,3] %>% as.character() -> pathway
  data[1,4] %>% as.numeric() -> diff
  if (diff > 0) {
    line_type <- "Activate"
  } else {
    line_type <- "Inhibit"
  }
  cancer_text %>%
    dplyr::filter(text %in% gene) %>%
    dplyr::select(x, y) %>%
    dplyr::mutate(x = x - 0.5) -> g1.pos
  cancer_text %>%
    dplyr::filter(text %in% gene) %>%
    dplyr::select(x, y) %>%
    dplyr::mutate(x = x + 0.5) -> g2.pos

  cancer_text %>%
    dplyr::filter(text %in% cancer) %>%
    dplyr::select(x, y) -> c.pos
  cancer_text %>%
    dplyr::filter(text %in% pathway) %>%
    dplyr::select(x, y) -> p.pos
  .d_seq_tmp1 <- data.frame(x1 = c.pos$x, y1 = c.pos$y, x2 = g1.pos$x, y2 = g1.pos$y, Cancer = cancer, Regulation = "Activate")
  .d_seq_tmp2 <- data.frame(x1 = g2.pos$x, y1 = g2.pos$y, x2 = p.pos$x, y2 = p.pos$y, Cancer = cancer, Regulation = line_type)
  rbind(.d_seq_tmp1,.d_seq_tmp2) -> .d_seg
  .d_seg$Cancer <- .d_seg$Cancer %>% as.character()
  .d_seg$Regulation <- .d_seg$Regulation %>% as.character()
  tibble::as_tibble(.d_seg)
}

get_rppa_seg1 <- function(cancer_text, data) {
  .d_seg <- data.frame(x1 = 0, y1 = 0, x2 = 0, y2 = 0, Cancer = "test", Regulation = "test")
  nrow(data) -> n
  
  for (i in 1:n) {
    data[i, 1] -> cancer
    data[i, 2] -> gene
    data[i, 3] -> pathway
    data[i, 4] -> diff
    if (diff > 0) {
      line_type <- "Activate"
    } else {
      line_type <- "Inhibit"
    }
    cancer_text %>%
      dplyr::filter(text %in% gene) %>%
      dplyr::select(x, y) %>%
      dplyr::mutate(x = x - 0.5) -> g1.pos
    cancer_text %>%
      dplyr::filter(text %in% gene) %>%
      dplyr::select(x, y) %>%
      dplyr::mutate(x = x + 0.5) -> g2.pos
    
    cancer_text %>%
      dplyr::filter(text %in% cancer) %>%
      dplyr::select(x, y) -> c.pos
    cancer_text %>%
      dplyr::filter(text %in% pathway) %>%
      dplyr::select(x, y) -> p.pos
    .d_seq_tmp1 <- data.frame(x1 = c.pos$x, y1 = c.pos$y, x2 = g1.pos$x, y2 = g1.pos$y, Cancer = cancer$cancer_types, Regulation = "Activate")
    .d_seq_tmp2 <- data.frame(x1 = g2.pos$x, y1 = g2.pos$y, x2 = p.pos$x, y2 = p.pos$y, Cancer = cancer$cancer_types, Regulation = line_type)
    rbind(.d_seg, .d_seq_tmp1) %>% rbind(.d_seq_tmp2) -> .d_seg
  }
  .d_seg[-1, ] %>%
    unique() -> .d_seg
  return(.d_seg)
}
