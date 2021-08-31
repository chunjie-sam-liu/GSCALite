# sourced by 'server.R'
# save as 'tcga_cnv_server.R'
# server elements 'tcga_cnv' sub tab of 'tcga' tab

source(file.path(config$wd, "functions", "tcga_cnv_function.R"))

# Selected cancer types ---------------------------------------------------
# cnv_cancer_type <- callModule(cancerType, "cnv")



# button control --------------------------------------------------------

# submit cancer type -------------------------------------------------------

# cnv_submit_analysis <- function(input, output, session) {
#   observeEvent(input$submit, {
#     status$cnv_submit <- TRUE
#     print(status$cnv_submit)
#     # shinyjs::disable(id = "cnv_submit")
#     # shinyjs::enable(id = "cnv_stop")
#   })
# }
# callModule(cnv_submit_analysis, "cnv")

# analysis core -----------------------------------------------------------
# monitor for gene list change--------------------------------------------
# cnv_gene_list <- eventReactive(
#   eventExpr = status$analysis,
#   ignoreNULL = TRUE,
#   valueExpr = {
#     # be sure the following code run after start analysis
#     if (status$analysis == TRUE) {
#       status$cnv_submit <- TRUE
#       shinyjs::disable(id = "cnv-submit")
#       shinyjs::disable(id = "cnv-switch")
#       as.character(gene_set$match)
#     }
#   }
# )
# generate cnv result out ui -------------------------------------------------------

output$ui_cnv_result <- shiny::renderUI({
  fn_cnv_result(selected_analysis$cnv)
})



# analysis start ----------------------------------------------------------
cnv_analysis <- eventReactive(
  {
    status$analysis
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      if (selected_analysis$cnv == TRUE) {
        # Cancer types value box selection ----------------------------------------

        callModule(module = cancerTypesSelect, id = "cnv", .sctps = intersect(selected_ctyps(), tcga_data))

        # Check box ---------------------------------------------------------------

        callModule(module = selectAndAnalysis, id = "cnv", .id = "cnv")
        .msg <- c("NOTICE: ")
        
        # load data----
        load_data_cnv()
        cnv_gene_new <- gene_set$match
        cnv_cancer_new <- selected_ctyps()

        # cancer overlap ----
        cancer_in_tcga_data_cnv <- intersect(selected_ctyps(),tcga_data)
        # print(cnv_cancer_diff)
        # print(cnv_gene_diff)

        if (length(gene_set$match) != 0) {
          if (!setequal(cnv_gene_new, cnv_gene_old) | !setequal(cnv_cancer_new, cnv_cancer_old)) {
            cnv_gene_old <- gene_set$match
            cnv_cancer_old <- selected_ctyps()

            # cnv percent plot ------------------------------------------------------------

            print(glue::glue("{paste0(rep('-', 10), collapse = '')} start cnv pie percent data processing@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            # get cancer type cnv ----

            cnv %>%
              dplyr::mutate(filter_cnv = purrr::map(cnv, filter_gene_list, gene_list = gene_set$match)) %>%
              dplyr::select(-cnv) %>%
              dplyr::filter(cancer_types %in% selected_ctyps()) -> gene_list_cancer_cnv

            # get data for plot ----
            gene_list_cancer_cnv %>%
              tidyr::unnest() %>%
              tidyr::drop_na() -> cnv_plot_ready
            if (nrow(cnv_plot_ready) > 0) {

              # cancer rank ----
              cnv_plot_ready %>%
                dplyr::group_by(cancer_types) %>%
                dplyr::summarise(v = sum(a_total - d_total)) %>%
                dplyr::arrange(dplyr::desc(v)) -> cnv_cancer_rank

              # gene rank ----
              cnv_plot_ready %>%
                dplyr::group_by(symbol) %>%
                dplyr::summarise(v = sum(a_total - d_total)) %>%
                dplyr::arrange(v) -> cnv_gene_rank

              # plot generate ----

              # pie plot ----
              cnv_plot_ready %>%
                dplyr::select(-a_total, -d_total) %>%
                tidyr::gather(key = type, value = per, -c(cancer_types, symbol)) %>%
                dplyr::mutate(
                  symbol = factor(x = symbol, levels = cnv_gene_rank$symbol),
                  cancer_types = factor(x = cancer_types, levels = cnv_cancer_rank$cancer_types)
                ) -> pie_plot_ready

              # cnvpie_getheight <- function(cn) {
              #   if (cn <= 5) {
              #     return(0.27)
              #   }
              #   if (cn > 5 && cn <= 20) {
              #     return(0.25 - (cn - 5) * 0.01)
              #   } else {
              #     return(0.15)
              #   }
              # }
              cnv_pie_gn <- pie_plot_ready$symbol %>% unique() %>% length()
              cnv_pie_cn <- pie_plot_ready$cancer_types %>% unique() %>% length()
              if(cnv_pie_cn < 7){
                cnv_pie_width <- 2
                cnv_pie_height <- 0.2*cnv_pie_gn} 
              if(cnv_pie_cn >= 7 && cnv_pie_cn<15){
                cnv_pie_width <- cnv_pie_cn * 0.27
                cnv_pie_height <- cnv_pie_gn * 0.27
              }
              if(cnv_pie_cn >= 15){
                cnv_pie_width <- cnv_pie_cn * 0.27
                cnv_pie_height <- cnv_pie_gn * 0.27
              }
              print(paste0("cnv_pie_width:",cnv_pie_width))
              print(paste0("cnv_pie_height:",cnv_pie_height))
              # cnv_pie_h <- cnvpie_getheight(cn = pie_plot_ready$cancer_types %>% unique() %>% length())
              # cnv_pie_height <- pie_plot_ready$symbol %>% unique() %>% length() * 0.27 #cnv_pie_h
              
              # if (cnv_pie_height > 15) {
              #   cnv_pie_height <- 15
              # }
              # if (cnv_pie_height < 3) {
              #   cnv_pie_height <- 3
              # }
              print(glue::glue("{paste0(rep('-', 10), collapse = '')} End cnv percent data processing@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

              print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start gernerate cnv pie profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

              callModule(
                piePlot, "cnv_pie", data = pie_plot_ready, y = "per",
                fill = "type", facet_grid = "symbol ~ cancer_types",
                outfile = file.path(user_dir, "pngs", paste(user_id, "-CNV_pie_profile.png", sep = "")), height = cnv_pie_height,
                width = cnv_pie_width,
                status_monitor = "analysis", status, downloadname = "cnv_percent_profile_figure"
              )
              .msg_cnv_pie <- NULL
            } else {
              .msg_cnv_pie <- paste(glue::glue("No significant [CNV Pie distribution] result of gene: {paste0(gene_set$match, collapse = ',')} in your selected cancer types {paste0(cancer_in_tcga_data_cnv,collapse=', ')}. Please try more cancers or more genes."), sep = " ")
              output[["cnv_pie-plot"]] <- callModule(white_plot, "cnv_pie", status_monitor = "analysis", status = status, outfile = file.path(user_dir, "pngs", paste(user_id, "-white_1.png", sep = "")))
            }

            print(glue::glue("{paste0(rep('-', 10), collapse = '')} End gernerate cnv pie profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            # homo cnv plot ----
            print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start gernerate homo cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            cnv_homo_plot_ready <- cnv_plot_ready %>%
              dplyr::select(cancer_types, symbol, a_homo, d_homo) %>%
              tidyr::gather(key = type, value = per, -cancer_types, -symbol) %>%
              dplyr::mutate(effect = plyr::revalue(type, replace = c("a_homo" = "Homozygous Amplification", "d_homo" = "Homozygous Deletion"))) %>%
              dplyr::mutate(color = plyr::revalue(type, replace = c("a_homo" = "brown4", "d_homo" = "aquamarine4")))
            if (nrow(cnv_homo_plot_ready) > 0) {
              callModule(
                cnv_pointPlot, "cnv_homo", data = cnv_homo_plot_ready, cancer = "cancer_types",
                gene = "symbol", size = "per", color = "color", sizename = "Homo CNV%",
                colorname = "SCNA Type", wrap = "~ effect", status_monitor = "analysis", status,
                downloadname = "cnv_homo_figure"
              )
              .msg_cnv_homo <- NULL
            } else {
              .msg_cnv_homo <- paste(glue::glue("No significant [Homo CNV profile] result of gene: {paste0(gene_set$match, collapse = ',')} in your selected cancer types: {paste0(cancer_in_tcga_data_cnv,collapse=', ')}. Please try more cancers or more genes."), sep = " ")
              output[["cnv_homo-plot"]] <- renderPlot({
                NULL
              })
            }


            print(glue::glue("{paste0(rep('-', 10), collapse = '')} End gernerate homo cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            # hete cnv plot ----
            print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start gernerate hete cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            cnv_hete_plot_ready <- cnv_plot_ready %>%
              dplyr::select(cancer_types, symbol, a_hete, d_hete) %>%
              tidyr::gather(key = type, value = per, -cancer_types, -symbol) %>%
              dplyr::mutate(effect = plyr::revalue(type, replace = c("a_hete" = "Heterozygous Amplification", "d_hete" = "Heterozygous Deletion"))) %>%
              dplyr::mutate(color = plyr::revalue(type, replace = c("a_hete" = "brown1", "d_hete" = "aquamarine3")))

            if (nrow(cnv_hete_plot_ready) > 0) {
              callModule(
                cnv_pointPlot, "cnv_hete", data = cnv_hete_plot_ready, cancer = "cancer_types",
                gene = "symbol", size = "per", color = "color", sizename = "Hete CNV%",
                colorname = "SCNA Type", wrap = "~ effect", status_monitor = "analysis", status,
                downloadname = "cnv_hete_figure"
              )
              .msg_cnv_hete <- NULL
            } else {
              .msg_cnv_hete <- paste(glue::glue("No significant [Hete CNV profile] result of gene: {paste0(gene_set$match, collapse = ',')} in your selected cancer types: {paste0(cancer_in_tcga_data_cnv,collapse=', ')}. Please try more cancers or more genes."), sep = " ")
              output[["cnv_hete-plot"]] <- renderPlot({
                NULL
              })
            }

            print(glue::glue("{paste0(rep('-', 10), collapse = '')} End gernerate hete cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            # cnv bar plot ------------------------------------------------------------

            # print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start processing cnv overall percent data@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
            # cnv_raw %>%
            #   dplyr::mutate(filter_cnv = purrr::map(cnv, filter_gene_list, gene_list = gene_set$match)) %>%
            #   dplyr::select(-cnv) %>%
            #   dplyr::filter(cancer_types %in% selected_ctyps()) -> gene_list_cancer_cnv_raw

            # bar stack plot ----
            # gene_list_cancer_cnv_raw %>%
            #   dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_gen_combined_core_atg, g_list = gene_set$match, n = 1)) %>%
            #   dplyr::select(-filter_cnv) %>%
            #   tidyr::unnest(rs) %>%
            #   dplyr::mutate(del_a = -del_a) %>%
            #   dplyr::mutate(del_s = -del_s) %>%
            #   tidyr::gather(key = type, value = per, -cancer_types) %>%
            #   dplyr::mutate(cnv_type = "Hete CNV") -> cnv_hete_bar_plot_ready
            # 
            # gene_list_cancer_cnv_raw %>%
            #   dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_gen_combined_core_atg, g_list = gene_set$match, n = 2)) %>%
            #   dplyr::select(-filter_cnv) %>%
            #   tidyr::unnest(rs) %>%
            #   dplyr::mutate(del_a = -del_a) %>%
            #   dplyr::mutate(del_s = -del_s) %>%
            #   tidyr::gather(key = type, value = per, -cancer_types) %>%
            #   dplyr::mutate(cnv_type = "Homo CNV") -> cnv_homo_bar_plot_ready
            # 
            # rbind(cnv_hete_bar_plot_ready, cnv_homo_bar_plot_ready) -> cnv_bar_plot_ready
            # 
            # print(glue::glue("{paste0(rep('-', 10), collapse = '')} End processing cnv overall percent data@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
            # if (nrow(cnv_bar_plot_ready) > 0) {
            #   callModule(cnvbarPlot, "cnv_bar", data = cnv_bar_plot_ready, x = "cancer_types", y = "per", fill = "type", status_monitor = "analysis", status, downloadname = "cnv_hete_figure")
            #   .msg_cnv_bar <- NULL
            # } else {
            #   .msg_cnv_bar <- paste(glue::glue("No significant [Overall CNV frenquency] result of gene: {paste0(gene_set$match, collapse = ',')} in your selected cancer types: {paste0(cancer_in_tcga_data_cnv,collapse=', ')}. Please try more cancers or more genes."), sep = " ")
            #   output[["cnv_bar-plot"]] <- renderPlot({
            #     NULL
            #   })
            # }

            # cnv cor to expressin ----------------------------------------------------

            print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start cnv cor to expression ploting@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
            cnv_cor %>%
              dplyr::mutate(spm = purrr::map(spm, filter_gene_list, gene_list = gene_set$match)) %>%
              dplyr::filter(cancer_types %in% selected_ctyps()) %>%
              tidyr::unnest() -> gene_list_cancer_cnv_cor

            if (nrow(gene_list_cancer_cnv_cor) > 0) {
              # cnv to expression plot  ----
              gene_list_cancer_cnv_cor %>%
                dplyr::group_by(symbol) %>%
                dplyr::summarise(rank = sum(spm)) %>%
                dplyr::arrange(rank) -> gene_rank.cnvcor

              gene_list_cancer_cnv_cor %>%
                dplyr::group_by(cancer_types) %>%
                dplyr::summarise(rank = sum(spm)) %>%
                dplyr::arrange(rank) -> cancer_rank.cnvcor

              callModule(methy_diff_pointPlot, "cnv_exp", data = gene_list_cancer_cnv_cor, cancer = "cancer_types", gene = "symbol", size = "logfdr", color = "spm", cancer_rank = cancer_rank.cnvcor, gene_rank = gene_rank.cnvcor, sizename = "-Log10(FDR)", colorname = "Pearson Correlation", title = "Pearson Correlation between CNV and mRNA RSEM.", status_monitor = "analysis", status, downloadname = "cnv_correlate_to_expr")
              .msg_cnv_exp <- NULL
            } else {
              .msg_cnv_exp <- paste(glue::glue("No significant [CNV to Expression] result of gene: {paste0(gene_set$match, collapse = ',')} in your selected cancer types: {paste0(cancer_in_tcga_data_cnv,collapse=', ')}. Please try more cancers or more genes."), sep = " ")

              output[["cnv_exp-plot"]] <- renderPlot({
                NULL
              })
            }

            # infomation UI for each part
            output[["cnv_pie-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_cnv_pie, style = "color:#CD3700")
              )
            })
            output[["cnv_hete-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_cnv_hete, style = "color:#CD3700")
              )
            })
            output[["cnv_homo-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_cnv_homo, style = "color:#CD3700")
              )
            })
            output[["cnv_bar-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_cnv_bar, style = "color:#CD3700")
              )
            })
            output[["cnv_exp-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_cnv_exp, style = "color:#CD3700")
              )
            })

            print(glue::glue("{paste0(rep('-', 10), collapse = '')} End cnv cor to expression ploting@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            .msg <- paste(.msg, glue::glue("Since we just show significant results, so a small size of gene and cancer set may cause no significant result in some plots. If it happens, try more genes and cancer types."), sep = " ")

            # alert for information
            shinyBS::createAlert(
              session = session, anchorId = "cnv-no_gene_set", title = "Information", style = "info",
              content = .msg, append = FALSE
            )
            status$cnv_submit <- FALSE
            shinyjs::enable("cnv-submit")
          } else {
            print("not do")
            cnv_gene_old <- gene_set$match
            cnv_cancer_old <- selected_ctyps()
          }
          print(cnv_gene_old)
          print(cnv_cancer_old)
        } else {
          shinyBS::createAlert(
            session = session, anchorId = "snv-no_gene_set", title = "Oops",
            content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
          )
        }
      }
    }
  }
)
# monitor ---------------------------------------------------------------------
observe(cnv_analysis())