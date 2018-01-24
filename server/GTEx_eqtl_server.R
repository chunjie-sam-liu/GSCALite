# sourced by 'server.R'
# save as 'GTEx_exp_server.R'
# server elements 'GTEx_exp' sub tab of 'GTEx' tab

source(file.path(config$wd, "functions", "data_function.R"))
source(file.path(config$wd, "functions", "gtex_eqtl_function.R"))

# check box-------------------------

# GTEx_eqtl_tissue <- callModule(GTEx_eqtl_Tissue, "gtex_eqtl")
# output$eqtl_selected_tissues <- renderText(GTEx_eqtl_tissue())

callModule(module = cancerTypesSelect, id = "gtex_eqtl", .sctps = GTEx_eqtl_tissue)
callModule(module = selectAndAnalysis, id = "gtex_eqtl", .id = "gtex_eqtl")

# generate eqtl result out ui -------------------------------------------------------

output$ui_eqtl_result <- shiny::renderUI({
  fn_eqtl_result(selected_analysis$eqtl)
})
#  get tissue type --------------------------------------------------------
# gtex_eqtl_submit_analysis <- function(input, output, session) {
#   observeEvent(input$submit, {
#     status$gtex_eqtl_submit <- TRUE
#     print(status$gtex_eqtl_submit)
#   })
# }
#
# callModule(gtex_eqtl_submit_analysis, "gtex_eqtl")



# get gene set-----------------------
# GTEx_eqtl_gene_list <- eventReactive(
#   eventExpr = status$analysis,
#   ignoreNULL = TRUE,
#   valueExpr = {
#     # be sure the following code run after start analysis
#     if (status$analysis == TRUE) {
#       status$gtex_eqtl_submit <- TRUE
#       shinyjs::disable(id = "GTEx_tissue_submit")
#       as.character(gene_set$match)
#     }
#   }
# )


# analysis core ----------------------------
###### do heatmap and gsva for gene set ####
filter_eqtl_4_geneset <- eventReactive(
  status$analysis == TRUE,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      if (selected_analysis$eqtl == TRUE) {
        # load GTEx eqtl data ---------------------------------------------------------
        load_data_eqtl()
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} start: eqtl selection on GTEx dataset processing@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

        tissue_set <- c("Heart", "Ovary", "Lung")
        tissue_set <- selected_ctyps()

        ##### start: select egenes ######
        print("start: select egenes in GTEx tissues")
        GTEx_egene %>% dplyr::filter(GTEx_egene$gene_name %in% gene_set$match & GTEx_egene$tissue %in% tissue_set) -> selected_eqtl_result
        print(selected_eqtl_result)
        if (nrow(selected_eqtl_result) > 0) {
          .msg <- glue::glue("Complete")
          output$`gtex_eqtl-gtex_eqtl_dt` <- DT::renderDataTable({
            selected_eqtl_result
          })
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
  }
)

observe(filter_eqtl_4_geneset())