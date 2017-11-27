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
    .err <- c(.err, "Error: The number of genes shuold be less than 200.")
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
