# source by server.R
# saved as functions_server.R

# Check input gene set ----------------------------------------------------
check_gene_set <- function(.s, status = status, error = error) {
  .s %>%
    stringr::str_split(pattern = "[^[:alnum:]]+", simplify = TRUE) %>%
    .[1, ] %>%
    stringr::str_trim(side = "both") -> .ss

  if (!dplyr::between(length(.ss), 5, 100)) {
    error$gene_set <- "The number of genes should be between 5 and 100."
    status$trigger <- if (status$trigger == TRUE) FALSE else TRUE
    status$gene_set <- FALSE
  }

  .ss
}


# Validate gene with TCGA gene symbol -------------------------------------
validate_gene_set <- function(.v, user_dir = user_dir, user_logs = user_logs, total_gene_symbol = total_gene_symbol, status = status, error = error, gene_set = gene_set) {
  .log_file <- user_logs$gene_set
  
  
  .v_dedup <- tibble::tibble(input = .v[.v != ""]) %>% unique() %>% 
    dplyr::mutate(Up = toupper(input))
  total_gene_symbol %>%
    dplyr::filter(TCGA_sym %in% .v_dedup$Up) %>%
    .$TCGA_sym -> .v_tcga
  total_gene_symbol %>%
    dplyr::filter(NCBI_sym %in% .v_dedup$Up)  %>%
    .$NCBI_sym -> .v_ncbi
  total_gene_symbol %>%
    dplyr::filter(alias %in% .v_dedup$Up) -> .v_alias
  
  total_gene_symbol %>%
    dplyr::filter(alias %in% setdiff(.v_dedup$Up,.v_ncbi)) -> .v_alias.ncbi
  
  total_gene_symbol %>%
    dplyr::filter(alias %in% setdiff(.v_dedup$Up,.v_tcga)) -> .v_alias.tcga
  
  gene_set$match <- c(.v_tcga,.v_alias.tcga$TCGA_sym) %>% unique()
  gene_set$match.gtex <- c(.v_ncbi,.v_alias.ncbi$NCBI_sym) %>% unique()
  
  .non_match <- tibble::tibble(Up = c(.v_tcga,.v_ncbi,.v_alias.tcga$alias, .v_alias.ncbi$alias)) %>%
    unique() %>%
    dplyr::mutate(match = "TRUE") %>%
    dplyr::right_join(.v_dedup,by = "Up") %>%
    dplyr::filter(is.na(match)) %>%
    .$input
  gene_set$non_match <- .non_match
  gene_set$n_match <- length(unique(c(.v_tcga,.v_ncbi,.v_alias.tcga$alias, .v_alias.ncbi$alias)))
  gene_set$n_non_match <- length(.non_match)
  gene_set$n_total <- length(unique(c(.v_tcga,.v_ncbi,.v_alias.tcga$alias, .v_alias.ncbi$alias))) + length(.non_match)
  
  if (length(gene_set$match) < 5) {
    error$gene_set <- "Please input at least five valid gene symbol."
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
# older version of match gene set
# validate_gene_set <- function(.v, user_dir = user_dir, user_logs = user_logs, total_gene_symbol = total_gene_symbol, status = status, error = error, gene_set = gene_set) {
#   .log_file <- user_logs$gene_set
# 
# 
#   .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = tolower, USE.NAMES = FALSE)
#   .v_dedup %in% names(total_gene_symbol) -> .inter
# 
# 
#   gene_set$match <- total_gene_symbol[.v_dedup[.inter]]
#   gene_set$non_match <- .v[!.inter]    #.v_dedup[!.inter]
#   gene_set$n_match <- length(total_gene_symbol[.v_dedup[.inter]])
#   gene_set$n_non_match <- length(.v_dedup[!.inter])
#   gene_set$n_total <- length(total_gene_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])
# 
#   if (length(gene_set$match) < 5) {
#     error$gene_set <- "Please input at least five valid gene symbol."
#     status$trigger <- if (status$trigger == TRUE) FALSE else TRUE
#     status$gene_set <- FALSE
#   }
# 
#   .log <- c(
#     glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Input total gene set number is {length(gene_set$n_total)} {paste0(rep('-', 10), collapse = '')}"),
#     glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Unique gene set number is {length(.v_dedup)} {paste0(rep('-', 10), collapse = '')}"),
#     glue::glue("#Total input gene set: {paste0(.v, collapse = ', ')}"),
#     glue::glue("#Validated genes: {paste0(gene_set$match, collapse = ', ')}"),
#     glue::glue("#Invalidated genes: {paste0(gene_set$non_match, collapse = ', ')}")
#   )
#   write(x = .log, file = .log_file, append = TRUE)
# }


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

  c.text <- data.frame(x = 0.5, y = 1, text = "test", type = "test")
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


# maftools subsetMaf edit to suit our addition. ---------------------------
my_subsetMaf <- function (maf, tsb = NULL, genes = NULL, fields = NULL, cancer = NULL, 
                          mafObj = FALSE, includeSyn = TRUE, isTCGA = FALSE) 
{
  maf.silent <- maf@maf.silent
  maf.dat <- maf@data
  maf.anno <- maf@clinical.data
  if (!is.null(tsb)) {
    if (isTCGA) {
      tsb = substr(x = tsb, start = 1, stop = 12)
    }
    maf.dat = maf.dat[Tumor_Sample_Barcode %in% tsb, ]
    maf.silent = maf.silent[Tumor_Sample_Barcode %in% tsb, 
                            ]
  }
  if (!is.null(genes)) {
    maf.dat = maf.dat[Hugo_Symbol %in% genes, ]
    maf.silent = maf.silent[Hugo_Symbol %in% genes, ]
  }
  if (!is.null(cancer)) {
    # maf.dat = maf.dat[eval(parse(text = query))]
    maf.dat = maf.dat[Cancer_Types %in% cancer,]
    # maf.silent = maf.silent[eval(parse(text = query))]
    maf.silent = maf.silent[Cancer_Types %in% cancer,]
  }
  default.fields = c("Hugo_Symbol", "Chromosome", "Start_Position", 
                     "End_Position", "Reference_Allele", "Tumor_Seq_Allele2", 
                     "Variant_Classification", "Variant_Type", "Tumor_Sample_Barcode")
  if (!is.null(fields)) {
    default.fields = unique(c(default.fields, fields))
    if (length(default.fields[!default.fields %in% colnames(maf.dat)]) > 
        0) {
      message("Missing fields. Ignoring them.. ")
      print(default.fields[!default.fields %in% colnames(maf.dat)])
      default.fields = default.fields[default.fields %in% 
                                        colnames(maf.dat)]
    }
    maf.dat = maf.dat[, default.fields, with = FALSE]
    maf.silent = maf.silent[, default.fields, with = FALSE]
  }
  if (mafObj) {
    maf.silent = droplevels.data.frame(maf.silent)
    maf.dat = droplevels.data.frame(maf.dat)
    maf.anno = droplevels.data.frame(maf.anno)
    mafSummary = my_summarizeMaf(maf.dat, chatty = FALSE, anno = maf.anno)
    m = my_MAF(data = maf.dat, variants.per.sample = mafSummary$variants.per.sample, 
            variant.type.summary = mafSummary$variant.type.summary, 
            variant.classification.summary = mafSummary$variant.classification.summary, 
            gene.summary = mafSummary$gene.summary, summary = mafSummary$summary, 
            maf.silent = maf.silent, clinical.data = mafSummary$sample.anno)
    return(m)
  }
  else {
    if (includeSyn) {
      return(rbind(maf.dat, maf.silent, use.names = TRUE, 
                   fill = TRUE))
    }
    else {
      return(maf.dat)
    }
  }
}

my_summarizeMaf = function(maf, anno = NULL, chatty = TRUE){
  
  if('NCBI_Build' %in% colnames(maf)){
    NCBI_Build = unique(maf[!Variant_Type %in% 'CNV', NCBI_Build])
    NCBI_Build = NCBI_Build[!is.na(NCBI_Build)]
    
    if(chatty){
      if(length(NCBI_Build) > 1){
        message('NOTE: Mutiple reference builds found!')
        NCBI_Build = do.call(paste, c(as.list(NCBI_Build), sep=";"))
        message(NCBI_Build)
      }
    }
  }else{
    NCBI_Build = NA
  }
  
  if('Center' %in% colnames(maf)){
    Center = unique(maf[!Variant_Type %in% 'CNV', Center])
    #Center = Center[is.na(Center)]
    if(length(Center) > 1){
      Center = do.call(paste, c(as.list(Center), sep=";"))
      if(chatty){
        message('Mutiple centers found.')
        print(Center)
      }
    }
  }else{
    Center = NA
  }
  
  #nGenes
  nGenes = length(unique(maf[,Hugo_Symbol]))
  
  #Top 20 FLAGS - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4267152/
  flags = c("TTN", "MUC16", "OBSCN", "AHNAK2", "SYNE1", "FLG", "MUC5B",
            "DNAH17", "PLEC", "DST", "SYNE2", "NEB", "HSPG2", "LAMA5", "AHNAK",
            "HMCN1", "USH2A", "DNAH11", "MACF1", "MUC17")
  
  #Variants per TSB
  tsb = maf[,.N, Tumor_Sample_Barcode]
  colnames(tsb)[2] = 'Variants'
  tsb = tsb[order(tsb$Variants, decreasing = TRUE),]
  
  #summarise and casting by 'Variant_Classification'
  vc = maf[,.N, .(Tumor_Sample_Barcode, Variant_Classification )]
  vc.cast = data.table::dcast(data = vc, formula = Tumor_Sample_Barcode ~ Variant_Classification, fill = 0, value.var = 'N')
  
  if(any(colnames(vc.cast) %in% c('Amp', 'Del'))){
    vc.cast.cnv = vc.cast[,c('Tumor_Sample_Barcode', colnames(vc.cast)[colnames(vc.cast) %in% c('Amp', 'Del')]), with =FALSE]
    vc.cast.cnv$CNV_total = rowSums(vc.cast.cnv[,2:ncol(vc.cast.cnv)], na.rm = TRUE)
    
    vc.cast = vc.cast[,!colnames(vc.cast)[colnames(vc.cast) %in% c('Amp', 'Del')], with =FALSE]
    vc.cast[,total:=rowSums(vc.cast[,2:ncol(vc.cast), with = FALSE])]
    
    vc.cast = merge(vc.cast, vc.cast.cnv, by = 'Tumor_Sample_Barcode', all = TRUE)[order(total, CNV_total, decreasing = TRUE)]
    
    vc.mean = as.numeric(as.character(c(NA, NA, NA, NA, apply(vc.cast[,2:ncol(vc.cast), with = FALSE], 2, mean))))
    vc.median = as.numeric(as.character(c(NA, NA, NA, NA, apply(vc.cast[,2:ncol(vc.cast), with = FALSE], 2, median))))
    
  }else{
    vc.cast = vc.cast[,total:=rowSums(vc.cast[,2:ncol(vc.cast), with = FALSE])][order(total, decreasing = TRUE)]
    
    vc.mean = round(as.numeric(as.character(c(NA, NA, NA, NA, apply(vc.cast[,2:ncol(vc.cast), with = FALSE], 2, mean)))), 3)
    vc.median = round(as.numeric(as.character(c(NA, NA, NA, NA, apply(vc.cast[,2:ncol(vc.cast), with = FALSE], 2, median)))), 3)
  }
  
  #summarise and casting by 'Variant_Type'
  vt = maf[,.N, .(Tumor_Sample_Barcode, Variant_Type )]
  vt.cast = data.table::dcast(data = vt, formula = Tumor_Sample_Barcode ~ Variant_Type, value.var = 'N', fill = 0)
  
  if(any(colnames(vt.cast) %in% c('CNV'))){
    vt.cast.cnv = vt.cast[,c('Tumor_Sample_Barcode', colnames(vt.cast)[colnames(vt.cast) %in% c('CNV')]), with =FALSE]
    
    vt.cast = vt.cast[,!colnames(vt.cast)[colnames(vt.cast) %in% c('CNV')], with =FALSE]
    vt.cast = vt.cast[,total:=rowSums(vt.cast[,2:ncol(vt.cast), with = FALSE])]
    
    vt.cast = merge(vt.cast, vt.cast.cnv, by = 'Tumor_Sample_Barcode', all = TRUE)[order(total, CNV, decreasing = TRUE)]
  }else{
    vt.cast = vt.cast[,total:=rowSums(vt.cast[,2:ncol(vt.cast), with = FALSE])][order(total, decreasing = TRUE)]
  }
  
  #summarise and casting by 'Hugo_Symbol'
  hs = maf[,.N, .(Hugo_Symbol, Variant_Classification)]
  hs.cast = data.table::dcast(data = hs, formula = Hugo_Symbol ~Variant_Classification, fill = 0, value.var = 'N')
  #----
  if(any(colnames(hs.cast) %in% c('Amp', 'Del'))){
    hs.cast.cnv = hs.cast[,c('Hugo_Symbol', colnames(hs.cast)[colnames(hs.cast) %in% c('Amp', 'Del')]), with = FALSE]
    hs.cast.cnv$CNV_total = rowSums(x = hs.cast.cnv[,2:ncol(hs.cast.cnv), with = FALSE], na.rm = TRUE)
    
    hs.cast = hs.cast[,!colnames(hs.cast)[colnames(hs.cast) %in% c('Amp', 'Del')], with = FALSE]
    hs.cast[,total:=rowSums(hs.cast[,2:ncol(hs.cast), with = FALSE], na.rm = TRUE)]
    
    hs.cast = merge(hs.cast, hs.cast.cnv, by = 'Hugo_Symbol', all = TRUE)[order(total, CNV_total, decreasing = TRUE)]
  }else{
    hs.cast[,total:=rowSums(hs.cast[,2:ncol(hs.cast), with = FALSE])]
    hs.cast = hs.cast[order(total, decreasing = TRUE)]
  }
  #----
  
  #Get in how many samples a gene ismutated
  numMutatedSamples = maf[!Variant_Type %in% 'CNV', .(MutatedSamples = length(unique(Tumor_Sample_Barcode))), by = Hugo_Symbol]
  numAlteredSamples = maf[, .(AlteredSamples = length(unique(Tumor_Sample_Barcode))), by = Hugo_Symbol]
  numAlteredSamples = merge(numMutatedSamples, numAlteredSamples, by = 'Hugo_Symbol', all = TRUE)
  #Merge and sort
  hs.cast = merge(hs.cast, numAlteredSamples, by = 'Hugo_Symbol', all = TRUE)[order(MutatedSamples, total, decreasing = TRUE)]
  #Replace NAs with 0
  hs.cast$AlteredSamples = ifelse(test = is.na(x = hs.cast$AlteredSamples), yes = 0, no = hs.cast$AlteredSamples)
  hs.cast$MutatedSamples = ifelse(test = is.na(x = hs.cast$MutatedSamples), yes = 0, no = hs.cast$MutatedSamples)
  #Make a summarized table
  summary = data.table::data.table(ID = c('NCBI_Build', 'Center','Samples', 'nGenes',colnames(vc.cast)[2:ncol(vc.cast)]),
                                   summary = c(NCBI_Build, Center, nrow(vc.cast), nGenes, colSums(vc.cast[,2:ncol(vc.cast), with =FALSE])))
  summary[,Mean := vc.mean]
  summary[,Median := vc.median]
  
  if(chatty){
    print(summary)
    
    message("Gene Summary..")
    print(hs.cast)
  }
  
  #Check for flags.
  if(nrow(hs.cast) > 10){
    topten = hs.cast[1:10, Hugo_Symbol]
    topten = topten[topten %in% flags]
    if(chatty){
      if(length(topten) > 0){
        message('NOTE: Possible FLAGS among top ten genes:')
        print(topten)
      }
    }
  }
  
  
  if(chatty){
    message("Checking clinical data..")
  }
  
  if(is.null(anno)){
    if(chatty){
      message("NOTE: Missing clinical data! It is strongly recommended to provide clinical data associated with samples if available.")
    }
    sample.anno = tsb[,.(Tumor_Sample_Barcode)]
  }else if(is.data.frame(x = anno)){
    sample.anno  = data.table::setDT(anno)
    if(!'Tumor_Sample_Barcode' %in% colnames(sample.anno)){
      message(paste0('Available fields in provided annotations..'))
      print(colnames(sample.anno))
      stop(paste0('Tumor_Sample_Barcode column not found in provided clinical data. Rename column name containing sample names to Tumor_Sample_Barcode if necessary.'))
    }
  }else{
    if(file.exists(anno)){
      sample.anno = data.table::fread(anno, stringsAsFactors = FALSE)
      if(!'Tumor_Sample_Barcode' %in% colnames(sample.anno)){
        message(paste0('Available fields in ', basename(anno), '..'))
        print(colnames(sample.anno))
        stop(paste0('Tumor_Sample_Barcode column not found in provided clinical data. Rename column name containing sample names to Tumor_Sample_Barcode if necessary.'))
      }
    }
  }
  
  #clean up annotation data
  colnames(sample.anno) = gsub(pattern = ' ', replacement = '_', x = colnames(sample.anno), fixed = TRUE) #replace spaces in column names for annotation data
  sample.anno = as.data.frame(apply(sample.anno, 2, function(y) trimws(y))) #remove trailing whitespaces
  sample.anno[sample.anno == ""] = NA #Replace blanks with NA
  sample.anno = as.data.frame(apply(sample.anno, 2, function(y) gsub(pattern = " ", replacement = "_", x = y))) #replace spaces with _
  data.table::setDT(x = sample.anno)
  colnames(sample.anno)[1] = c("Tumor_Sample_Barcode")
  
  maf.tsbs = levels(tsb[,Tumor_Sample_Barcode])
  sample.anno = sample.anno[Tumor_Sample_Barcode %in% maf.tsbs][!duplicated(Tumor_Sample_Barcode)]
  anno.tsbs = sample.anno[,Tumor_Sample_Barcode]
  
  if(!length(maf.tsbs[!maf.tsbs %in% anno.tsbs]) == 0){
    if(chatty){
      message('Annotation missing for below samples in MAF')
      print(maf.tsbs[!maf.tsbs %in% anno.tsbs])
    }
  }
  
  return(list(variants.per.sample = tsb, variant.type.summary = vt.cast, variant.classification.summary = vc.cast,
              gene.summary = hs.cast, summary = summary, sample.anno = sample.anno))
}

## MAF object
my_MAF <- setClass(Class = 'MAF', slots =  c(data = 'data.table', variants.per.sample = 'data.table', variant.type.summary = 'data.table',
                                          variant.classification.summary = 'data.table', gene.summary = 'data.table',
                                          summary = 'data.table', maf.silent = 'data.table', clinical.data = 'data.table'))



# Loading screen ----------------------------------------------------------

loading_screen <- function(){
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}
