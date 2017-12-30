# sourced by 'server.R'
# save as 'tcga_cnv_server.R'
# server elements 'tcga_cnv' sub tab of 'tcga' tab

# data input --------------------------------------------------------------
# get gene set ----
# input<-list(gene_set=c("TP53","EZH2","CD274","CD276","CD80",
#                        "CD86","VTCN1","CD40LG","TNFRSF14",
#                        "TNFSF9","TNFSF4","CD70",ICOS",
#                        "BTLA","LAG3","TNFRSF9","TNFRSF4"))
# cnv_gene_list <- reactive({
# c("TP53","EZH2","CD274","CD276","CD80",
#   "CD86","VTCN1","CD40LG","TNFRSF14",
#   "TNFSF9","TNFSF4","CD70","ICOS",
#   "BTLA","LAG3","TNFRSF9","TNFRSF4")
# })



# load cnv data ----
print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv percent data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
cnv <- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_cnv_percent.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} end loading cnv percent data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv raw data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
cnv_raw <- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_cnv_threshold.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv raw data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv cor data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
cnv_cor <- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_all_gene_exp-cor-cnv.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv cor data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

# Selected cancer types ---------------------------------------------------
cnv_cancer_type <- callModule(cancerType, "cnv")
output$cnv_selected_cancer <- renderText(
  cnv_cancer_type()
)
# Cancer types value box selection ----------------------------------------

callModule(module = cancerTypesSelect, id = "cnv", .sctps = cnv_cancer_type)
# Check box ---------------------------------------------------------------

callModule(module = selectAndAnalysis, id = "cnv", .id = "cnv")

# cnv_cancer_type <- reactive(c("KIRC","LGG","COAD","LUAD","LUSC","BRCA"))


# button control --------------------------------------------------------

# submit cancer type -------------------------------------------------------
cnv_submit_analysis <- function(input, output, session){
observeEvent(input$submit, {
  status$cnv_submit <- TRUE
  print(status$cnv_submit)
  # shinyjs::disable(id = "cnv_submit")
  # shinyjs::enable(id = "cnv_stop")
})
}
callModule(cnv_submit_analysis,"cnv")
# stop analysis -----------------------------------------------------------
# observeEvent(input$cnv_stop, {
#   status$cnv_submit <- FALSE
#   shinyjs::enable(id = "cnv_submit")
#   cnv_hide <- c("cnv_pie", "cnv_hete", "cnv_homo", "cnv_bar", "cnv_exp")
#   hidePic(cnv_hide) # hide pic when stop clicked!
# })


# analysis core -----------------------------------------------------------
# monitor for gene list change
cnv_gene_list <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      status$cnv_submit <- TRUE
      shinyjs::disable(id = "cnv_submit")
      as.character(gene_set$match)
    }
  }
)

# monitor fot cancer type change
# cnv_cancer_type <- eventReactive(
#   eventExpr = input$stop,
#   ignoreNULL = TRUE,
#   valueExpr = {
#     # be sure the following code run after start analysis
#     cnv_cancer_type<-callModule(resetcancerType,"cnv")
#   }
# )


# analysis start ----------------------------------------------------------


cnv_analysis <- eventReactive(
  {
    status$cnv_submit == TRUE
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$cnv_submit == TRUE) {
      print(cnv_gene_list(), "2")
      print("2")
      print(cnv_cancer_type())
      print(status$cnv_submit)
      # cnv percent plot ------------------------------------------------------------

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} start cnv pie percent data processing@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      # get cancer type cnv ----

      cnv %>%
        dplyr::mutate(filter_cnv = purrr::map(cnv, filter_gene_list, gene_list = cnv_gene_list())) %>%
        dplyr::select(-cnv) %>%
        dplyr::filter(cancer_types %in% cnv_cancer_type()) -> gene_list_cancer_cnv

      # get data for plot ----
      gene_list_cancer_cnv %>%
        tidyr::unnest() %>%
        tidyr::drop_na() -> cnv_plot_ready

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

      cnv_pie_height <- pie_plot_ready$symbol %>% unique() %>% length() * 0.25
      if (cnv_pie_height > 15) {
        cnv_pie_height <- 15
      }
      if (cnv_pie_height < 3) {
        cnv_pie_height <- 3
      }

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} End cnv percent data processing@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start gernerate cnv pie profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      callModule(
        piePlot, "cnv_pie", data = pie_plot_ready, y = "per",
        fill = "type", facet_grid = "symbol ~ cancer_types",
        outfile = file.path(user_dir, "pngs", paste(user_id, "-CNV_pie_profile.png", sep = "")), height = cnv_pie_height
      )

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} End gernerate cnv pie profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      # homo cnv plot ----
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start gernerate homo cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      cnv_homo_plot_ready <- cnv_plot_ready %>%
        dplyr::select(cancer_types, symbol, a_homo, d_homo) %>%
        tidyr::gather(key = type, value = per, -cancer_types, -symbol) %>%
        dplyr::mutate(effect = plyr::revalue(type, replace = c("a_homo" = "Homozygous Amplification", "d_homo" = "Homozygous Deletion"))) %>%
        dplyr::mutate(color = plyr::revalue(type, replace = c("a_homo" = "brown4", "d_homo" = "aquamarine4")))

      callModule(
        pointPlot, "cnv_homo", data = cnv_homo_plot_ready, cancer = "cancer_types",
        gene = "symbol", size = "per", color = "color", sizename = "Homo CNV%",
        colorname = "SCNA Type", wrap = "~ effect"
      )

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} End gernerate homo cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      # hete cnv plot ----
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start gernerate hete cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      cnv_hete_plot_ready <- cnv_plot_ready %>%
        dplyr::select(cancer_types, symbol, a_hete, d_hete) %>%
        tidyr::gather(key = type, value = per, -cancer_types, -symbol) %>%
        dplyr::mutate(effect = plyr::revalue(type, replace = c("a_hete" = "Heterozygous Amplification", "d_hete" = "Heterozygous Deletion"))) %>%
        dplyr::mutate(color = plyr::revalue(type, replace = c("a_hete" = "brown1", "d_hete" = "aquamarine3")))

      callModule(
        pointPlot, "cnv_hete", data = cnv_hete_plot_ready, cancer = "cancer_types",
        gene = "symbol", size = "per", color = "color", sizename = "Hete CNV%",
        colorname = "SCNA Type", wrap = "~ effect"
      )

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} End gernerate hete cnv profile plot@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      # cnv bar plot ------------------------------------------------------------

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start processing cnv overall percent data@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      cnv_raw %>%
        dplyr::mutate(filter_cnv = purrr::map(cnv, filter_gene_list, gene_list = cnv_gene_list())) %>%
        dplyr::select(-cnv) %>%
        dplyr::filter(cancer_types %in% cnv_cancer_type()) -> gene_list_cancer_cnv_raw

      # bar stack plot ----
      gene_list_cancer_cnv_raw %>%
        dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_gen_combined_core_atg, g_list = cnv_gene_list(), n = 1)) %>%
        dplyr::select(-filter_cnv) %>%
        tidyr::unnest(rs) %>%
        dplyr::mutate(del_a = -del_a) %>%
        dplyr::mutate(del_s = -del_s) %>%
        tidyr::gather(key = type, value = per, -cancer_types) %>%
        dplyr::mutate(cnv_type = "Hete CNV") -> cnv_hete_bar_plot_ready

      gene_list_cancer_cnv_raw %>%
        dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_gen_combined_core_atg, g_list = cnv_gene_list(), n = 2)) %>%
        dplyr::select(-filter_cnv) %>%
        tidyr::unnest(rs) %>%
        dplyr::mutate(del_a = -del_a) %>%
        dplyr::mutate(del_s = -del_s) %>%
        tidyr::gather(key = type, value = per, -cancer_types) %>%
        dplyr::mutate(cnv_type = "Homo CNV") -> cnv_homo_bar_plot_ready

      rbind(cnv_hete_bar_plot_ready, cnv_homo_bar_plot_ready) -> cnv_bar_plot_ready

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} End processing cnv overall percent data@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

      callModule(cnvbarPlot, "cnv_bar", data = cnv_bar_plot_ready, x = "cancer_types", y = "per", fill = "type")

      # cnv cor to expressin ----------------------------------------------------

      print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start cnv cor to expression ploting@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      cnv_cor %>%
        dplyr::mutate(spm = purrr::map(spm, filter_gene_list, gene_list = cnv_gene_list())) %>%
        dplyr::filter(cancer_types %in% cnv_cancer_type()) %>%
        tidyr::unnest() -> gene_list_cancer_cnv_cor

      # cnv to expression plot  ----
      gene_list_cancer_cnv_cor %>%
        dplyr::group_by(symbol) %>%
        dplyr::summarise(rank = sum(spm)) %>%
        dplyr::arrange(rank) -> gene_rank.cnvcor

      gene_list_cancer_cnv_cor %>%
        dplyr::group_by(cancer_types) %>%
        dplyr::summarise(rank = sum(spm)) %>%
        dplyr::arrange(rank) -> cancer_rank.cnvcor

      callModule(methy_diff_pointPlot, "cnv_exp", data = gene_list_cancer_cnv_cor, cancer = "cancer_types", gene = "symbol", size = "logfdr", color = "spm", cancer_rank = cancer_rank.cnvcor, gene_rank = gene_rank.cnvcor, sizename = "-Log10(P.value)", colorname = "Spearman Correlation Coefficient", title = "Spearman Correlation Coefficient of CNV and gene expression.")
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} End cnv cor to expression ploting@ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      # cnv_show <- c("cnv_pie", "cnv_hete", "cnv_homo", "cnv_bar", "cnv_exp")
      # showPic(cnv_show) # hide pic when stop clicked!
      status$cnv_submit <- FALSE
    }
  }
)

# monitor ---------------------------------------------------------------------
observe(cnv_gene_list())
observe(cnv_analysis())