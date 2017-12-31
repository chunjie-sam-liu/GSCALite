# sourced by 'server.R'
# save as 'GTEx_exp_server.R'
# server elements 'GTEx_exp' sub tab of 'GTEx' tab

source(file.path(config$wd, "functions", "data_function.R"))


# check box-------------------------
callModule(module = selectAndAnalysis, id = "gtex_eqtl", .id = "gtex_eqtl")

GTEx_eqtl_tissue <- callModule(GTEx_eqtl_Tissue, "gtex_eqtl")
output$eqtl_selected_tissues <- renderText(GTEx_eqtl_tissue())

callModule(module = cancerTypesSelect, id = "gtex_eqtl", .sctps = GTEx_eqtl_tissue)
callModule(module = selectAndAnalysis, id = "gtex_eqtl", .id = "gtex_eqtl")


#  get tissue type --------------------------------------------------------
gtex_eqtl_submit_analysis <- function(input, output, session) {
  observeEvent(input$submit, {
    status$gtex_eqtl_submit <- TRUE
    print(status$gtex_eqtl_submit)
  })
}

callModule(gtex_eqtl_submit_analysis, "gtex_eqtl")



# get gene set-----------------------
GTEx_eqtl_gene_list <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      status$gtex_eqtl_submit <- TRUE
      shinyjs::disable(id = "GTEx_tissue_submit")
      as.character(gene_set$match)
    }
  }
)


# analysis core ----------------------------
###### do heatmap and gsva for gene set ####
filter_eqtl_4_geneset <- eventReactive(
  status$gtex_eqtl_submit == TRUE,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$gtex_eqtl_submit == TRUE) {
      print(GTEx_eqtl_gene_list(), "2")
      print("2")
      ## can not get the tissue info!!!!!#
      print(GTEx_eqtl_tissue())
      output$gtex_eqtl_dt <- DT::renderDataTable({
        NULL
      })
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} start: eqtl selection on GTEx dataset processing@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      # load GTEx eqtl data ---------------------------------------------------------
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading GTEx eqtl data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      GTEx_egene <- readr::read_rds(file.path(config$database, "GTEx", "eqtl", "GTEx_egene.merged.tissue.rds.gz"))
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} end loading GTEx eqtl data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      gene_set <- GTEx_eqtl_gene_list()
      tissue_set <- c("Heart", "Ovary", "Lung")
      print(tissue_set)
      print(gene_set)

      ##### start: select egenes ######
      print("start: select egenes in GTEx tissues")
      GTEx_egene %>% dplyr::filter(GTEx_egene$gene_name %in% gene_set & GTEx_egene$tissue %in% tissue_set) -> selected_eqtl_result
      print(selected_eqtl_result)
      if (nrow(selected_eqtl_result) > 0) {
        .msg <- glue::glue("complete")
        output$`gtex_eqtl-gtex_eqtl_dt` <- DT::renderDataTable({
          selected_eqtl_result
        })
        print(.msg)
      } else {
        .msg <- glue::glue("No eqtl in gene set for your selected tissue.")
      }
      shinyBS::createAlert(
        session = session, anchorId = "expr-no_gene_set", title = "Information", style = "info",
        content = .msg, append = FALSE
      )
      print("end: select egenes in GTEx tissues")
    }
  }
)

observe(GTEx_eqtl_gene_list())
observe(filter_eqtl_4_geneset())
