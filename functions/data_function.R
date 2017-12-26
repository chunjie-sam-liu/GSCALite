

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