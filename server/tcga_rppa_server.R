# sourced by 'server.R'
# save as 'tcga_rppa_server.R'
# server elements 'tcga_rppa' sub tab of 'tcga' tab


source(file.path(config$wd, "functions", "tcga_rppa_function.R"))


# generate rppa result out ui -------------------------------------------------------

output$ui_rppa_result <- shiny::renderUI({
  fn_rppa_result(selected_analysis$rppa)
})

# analysis core -----------------------------------------------------------

rppa_analysis <- eventReactive(
  {
    status$analysis == TRUE
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      if (selected_analysis$rppa == TRUE) {
        # Cancer types value box selection ----------------------------------------
        
        callModule(module = cancerTypesSelect, id = "rppa", .sctps = intersect(selected_ctyps(), tcga_data))
        # Check box ---------------------------------------------------------------
        
        callModule(module = selectAndAnalysis, id = "rppa", .id = "rppa")
        load_data_rppa()
      if (length(gene_set$match) != 0) {
        shinyBS::createAlert(
          session = session, anchorId = "rppa-no_gene_set", title = "Information", style = "info",
          content = "Need a few minutes to draw picture, please wait.", append = FALSE
        )
        .msg <- c("NOTICE: Too much cancers and genes will make [Relation network] complicated, it will hard to see, try less genes or less cancers. [Global percentage] and [Heatmap percentage] will not change when cancer selection changes, cause it's a global percentage included all cancer types (see help page).")

        # remove pic result generate before ----
        # output$rppa_rela_plot <- renderImage({})

        print("-------------------------------RPPA part------------------------------------")
        print(gene_set$match)
        print(selected_ctyps())
        
        # ploting -----------------------------------------------------------------
        # global plot -----------------------------------------------
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start rppa global analysis part@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        
        # get gene set /cancer type data ----
        rppa_per %>%
          dplyr::filter(symbol %in% gene_set$match) -> gene_list_rppa_per
        
        # rppa global pie plot----
        gene_list_rppa_per %>%
          tidyr::unnest() %>%
          tidyr::gather(-symbol, -pathway, key = "class", value = "per") %>%
          dplyr::mutate(class = plyr::revalue(class, replace = c("a" = "Activation", "i" = "Inhibition", "n" = "None"))) -> rppa_pie_plot_ready
        
        # arugument for plot
        rppa_pie_height <- gene_set$match %>% length() * 0.25
        if (rppa_pie_height > 15) {
          rppa_pie_height <- 15
        }
        if (rppa_pie_height < 3) {
          rppa_pie_height <- 3
        }
        rppa_pie_outfile <- file.path(user_dir, "pngs", paste(user_id, "-", "TCGA_rppa_pie_profile.png", sep = ""))
        
        # draw ----
        callModule(rppaPiePlot, "rppa_pie", data = rppa_pie_plot_ready, y = "per", fill = "class", facet_grid = " symbol~pathway", height = rppa_pie_height, outfile = rppa_pie_outfile, status, downloadname = "Pathway_activity_pie_percentage")
        
        # rppa global percentage ----
        # data process
        gene_list_rppa_per %>%
          tidyr::unnest() %>%
          dplyr::filter(a + i > 5 / 32) %>%
          dplyr::select(-n) %>%
          tidyr::gather(-symbol, -pathway, key = "class", value = "per") %>%
          dplyr::mutate(per = ifelse(class == "i", -per * 100, per * 100)) %>%
          tidyr::unite(pathway, c(pathway, class)) -> rppa_per_ready
        
        # pic draw
        rppa_heat_height <- gene_set$match %>% length() * 0.1
        if (rppa_heat_height > 15) {
          rppa_heat_height <- 15
        }
        if (rppa_heat_height < 3) {
          rppa_heat_height <- 3
        }
        rppa_heat_outfile <- file.path(user_dir, "pngs", paste(user_id, "-", "TCGA_rppa_heatmap_percentage.png", sep = ""))
        callModule(rppa_heat_per, "rppa_per", rppa_per_ready = rppa_per_ready, pathway = "pathway", symbol = "symbol", per = "per", height = rppa_heat_height, outfile = rppa_heat_outfile, status, downloadname = "Pathway_activity_global_percentage")
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start rppa global analysis part@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        
        # rppa relation plot ---------------------------------------------------
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} start rppa analysis part@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        
        # data processing
                rppa_relation %>%
          dplyr::filter(cancer_types %in% selected_ctyps()) %>%
          dplyr::mutate(data = purrr::map(data, filter_gene_list, gene_list = gene_set$match)) %>%
          tidyr::unnest() -> gene_list_cancer_rppa_rela
                
        # rppa line contact ----
        # get data
        cancer_text <- get_rppa_text(gene_list_cancer_rppa_rela)
        gene_list_cancer_rppa_rela %>%
          dplyr::mutate(n=1:nrow(gene_list_cancer_rppa_rela)) %>%
          tidyr::nest(-n) %>%
          dplyr::group_by(n) %>%
          dplyr::mutate(seg=purrr::map(data,.f=get_rppa_seg,cancer_text=cancer_text)) %>%
          dplyr::ungroup() %>%
          dplyr::select(-n,-data) %>%
          tidyr::unnest() ->plot_seg
        # get_rppa_seg1(gene_list_cancer_rppa_rela,cancer_text = cancer_text) -> plot_seg
        cancer_text %>%
          dplyr::filter(type == "cancer") -> cancer.text
        cancer_text %>%
          dplyr::filter(type == "gene") -> gene.text
        cancer_text %>%
          dplyr::filter(type == "pathway") -> path.text
        rppa_line_height <- gene_set$match %>% length() * 0.1
        if (rppa_line_height > 15) {
          rppa_line_height <- 15
        }
        if (rppa_line_height < 3) {
          rppa_line_height <- 3
        }
        # plot draw
        output$`rppa_line-plot` <- renderImage({
          status[["analysis"]]
          
          rppa_line_outfile <- file.path(user_dir, "pngs", paste(user_id, "-", "TCGA_rppa_network_profile.png", sep = ""))

          ggsave(rppa_line_outfile, rppa_line_contact(plot_seg, cancer.text, gene.text, path.text), device = "png", width = 4, height = rppa_line_height)
          list(
            src = rppa_line_outfile,
            contentType = "image/png",
            # width = "100%" ,
            # height = 900,
            alt = "This is alternate text"
          )
        }, deleteFile = FALSE)
        
        output$`rppa_line-picdownload` <- downloadHandler(
          filename = function() { paste("Pathway_relation_network", '.',input$`rppa_line-pictype`, sep='') },
          content = function(file) {
            ggsave(file,rppa_line_contact(plot_seg, cancer.text, gene.text, path.text),device = input$`rppa_line-pictype`,width = input$`rppa_line-d_width`,height = input$`rppa_line-d_height`)
          }
        )
        
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} End rppa relation analysis part@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        # alert for information
        shinyBS::createAlert(
          session = session, anchorId = "rppa-no_gene_set", title = "Information", style = "info",
          content = .msg, append = FALSE
        )
      } else {
        shinyBS::createAlert(
          session = session, anchorId = "rppa-no_gene_set", title = "Oops",
          content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
        )
      }
      }
    }
  }
)



# monitor -----------------------------------------------------------------

observe(rppa_analysis())
