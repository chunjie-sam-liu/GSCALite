# sourced by 'server.R'
# save as 'GTEx_exp_server.R'
# server elements 'GTEx_exp' sub tab of 'GTEx' tab

source(file.path(config$wd, "functions", "data_function.R"))
source(file.path(config$wd, "functions", "gtex_gexp_function.R"))

# check box-------------------------
callModule(module = cancerTypesSelect, id = "GTEx_exp", .sctps = input$select_ctps)
callModule(module = selectAndAnalysis, id = "GTEx_exp", .id = "GTEx_exp")

# generate eqtl result out ui -------------------------------------------------------

output$ui_gexp_result <- shiny::renderUI({
  fn_gexp_result(selected_analysis$gtex_exp)
})

#  get tissue type --------------------------------------------------------
# 
# observeEvent(input$gtex_expr_submit, {
#   status$gtex_expr_submit <- TRUE
#   shinyjs::disable(id = "GTEx_tissue_submit")
#   shinyjs::enable(id = "analysis_stop")
# })

# stop analysis -----------------------------------------------------------
# observeEvent(input$analysis_stop, {
#   status$GTEx_tissue_submit <- FALSE
#   shinyjs::enable(id = "GTEx_tissue_submit")
#   GTEx_hide <- c("GTEx_exp", "GTEx_gsva")
#   hidePic(GTEx_hide) # hide pic when stop clicked!
# })


# get gene set-----------------------
# GTEx_expr_gene_list <- eventReactive(
#   eventExpr = status$analysis,
#   ignoreNULL = TRUE,
#   valueExpr = {
#     # be sure the following code run after start analysis
#     if (status$analysis == TRUE) {
#       status$gtex_expr_submit <- TRUE
#       shinyjs::disable(id = "GTEx_tissue_submit")
#       shinyjs::enable(id = "analysis_stop")
#       as.character(gene_set$match)
#     }
#   }
# )

# Selected tissue types ---------------------------------------------------
# GTEx_tissue_type <- callModule(GTEx_normal_Tissue, "GTEx_exp")
# output$GTEx_selected_tissues <- renderText(GTEx_tissue_type())



####### reset tissue type selection when click-----------
# observeEvent(input$GTEx_tissue_reset, {
#   GTEx_tissue_type <- callModule(resetGTExTissueType, "GTEx_exp")
# })
# observeEvent(input$GTEx_tissue_submit, heatmap_gsva_4_geneset(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set))
# analysis core ----------------------------

##### get gene expression profiles in GTEx dataset######

get_gene_exp_profile <- function(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set, filter_gene = 0) {
  gtex_expr <- gtex_expr[gtex_expr$SMTS %in% tissue_set, ]
  if (filter_gene) {
    gtex_expr %>%
      dplyr::mutate(
        expr = purrr::map(
          .x = expr,
          .f = function(.x) {
            .x %>%
              dplyr::filter(symbol %in% gene_set)
          }
        )
      ) -> gtex_gene_list_expr
  } else {
    gtex_gene_list_expr <- gtex_expr
  }
  return(gtex_gene_list_expr)
}


get_gene_mean_profile <- function(gene_set = gene_set, gtex_expr_mean = gtex_expr_mean, tissue_set = tissue_set, filter_gene = 0) {
  gtex_expr_mean <- gtex_expr_mean[gtex_expr_mean$SMTS %in% tissue_set, ]
  if (filter_gene) {
    gtex_expr_mean %>%
      dplyr::mutate(
        Mean = purrr::map(
          .x = Mean,
          .f = function(.x) {
            .x %>%
              dplyr::filter(symbol %in% gene_set)
          }
        )
      ) -> gtex_gene_list_mean
  } else {
    gtex_gene_list_mean <- gtex_expr_mean
  }
  return(gtex_gene_list_mean)
}



###### do heatmap and gsva for gene set ####
heatmap_gsva_4_geneset <- eventReactive(
  {
    status$analysis == TRUE
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      if (selected_analysis$gtex_exp == TRUE) {
        load_data_gexp()
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} start: expression profiles of gene set on GTEx dataset processing@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        # gene_set <- GTEx_expr_gene_list()
        # tissue_set <- c("Heart", "Ovary", "Lung","Muscle","Blood","Uterus","Vagina","Breast","Skin","Testis","Colon","Stomach","Pancreas")
        # print(tissue_set)
        # print(gene_set)
        
        ##### start: draw heatmap for the gene set in GTEx dataset######
        print("start: draw heatmap for the gene set in GTEx dataset")
        gtex_gene_list_expr.mean <- get_gene_mean_profile(gene_set = gene_set$match, gtex_expr_mean = gtex_expr_mean, tissue_set = input$select_ctps, filter_gene = 1)
        gene_n <- length(gene_set$match)
        display_matrix <- data.frame(round(matrix(unlist(lapply(gtex_gene_list_expr.mean$Mean,function(.x){.x[2]})), nrow = gene_n), 2))
        colnames(display_matrix) <- gtex_gene_list_expr.mean$SMTS
        display_matrix$GeneName <- gtex_gene_list_expr.mean$Mean[[1]]$symbol
        display_matrix %>% tidyr::gather(Tissue, RPKM, -GeneName) -> hm_4_p
        callModule(heatmap_GTEX_Plot, "GTEx_exp", data = hm_4_p,status=status)
        print("end: draw heatmap for the gene set in GTEx dataset")
        ######## calculate and draw GSVA profiles for gene set in selected tissues in GTEx dataset############
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} start: calculating gene set on GTEx dataset@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      }
      # print(GTEx_expr_gene_list(), "2")
      # print("2")
      ## can not get the tissue info!!!!!#
      # print(GTEx_tissue_type())

      
      #   gtex_expr <- get_gene_exp_profile(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set,filter_gene = 0)
      #
      #   gene_set.lst <- list(atg_lys = gene_set)
      #   fn_gsva <- function(.y, gene_set.lst = gene_set.lst){
      #     .y %>%
      #       tidyr::drop_na() %>%
      #       dplyr::select( -ensembl_gene_id) -> .d
      #
      #     .d_mat <- as.matrix(.d[,-1])
      #     rownames(.d_mat) <- .d$symbol
      #     .es_dif <- gsva(.d_mat, gset.idx.list = gene_set.lst, method = "gsva", mx.diff = TRUE, verbose = FALSE, parallel.sz = 1)
      #     .es_dif %>%
      #       as.data.frame() %>%
      #       tibble::as_tibble() %>%
      #       tibble::add_column(set = "atg_lys", .before = 1) -> .d_es
      #   }
      #   print("run_gsva")
      #   gtex_expr %>%
      #     dplyr::mutate(
      #       gsva = purrr::map(
      #         .x = expr,
      #         .f = function(.x) {
      #           fn_gsva(.x, gene_set.lst = gene_set.lst)
      #         }
      #       )
      #     ) -> gtex_expr_gsva
      #   print("end_gsva")
      #   gtex_expr_gsva %>%
      #     dplyr::select(SMTS, gsva) %>%
      #     dplyr::mutate(
      #       gsva = purrr::map(
      #         .x = gsva,
      #         .f = function(.x) {
      #           .x %>%
      #             dplyr::select(-set) %>%
      #             tidyr::gather(key = barcode, value = gsva)
      #         }
      #       )
      #     ) %>% tidyr::unnest() -> plot_ready
      #
      # 	print("call box plot for gsva")
      # callModule(box_GTEx_GSVA_Plot, "GTEx_gsva", data=plot_ready)
    }
  }
)

# observe(GTEx_expr_gene_list())
observe(heatmap_gsva_4_geneset())