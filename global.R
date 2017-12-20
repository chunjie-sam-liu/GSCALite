# source by server.R
# source by ui.R
# saved as functions_server.R

##########################################
# Cancer selection data get and comfim####
## 1. cancer select for each part ui ######
## 2. cancer type selection confirm  ######
## cancerTypeInput & cancerType############
##########################################
# cancer type selection ---------------------------------------------------

cancerTypeInput <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # cancer type selection----
      column(
        width = 10,
        offset = 1,
        shiny::tags$br(),
        shiny::tags$h3("Cancer Type Selection", class = "text-success"),
        shiny::tags$br(),

        shinydashboard::tabBox(
          width = 12, title = "Tissue",
          tabPanel(
            "Kidney",
            shiny::tags$h4("Kidney", class = "text-success"),
            checkboxGroupInput(
              inputId = ns("Kidney"), label = NULL, inline = TRUE,
              choices = list(
                "Kidney Chromophobe(KICH)" = "KICH",
                "Kidney Renal Clear Cell Carcinoma(KIRC)" = "KIRC",
                "Kidney Renal Papillary Cell Carcinoma(KIRP)" = "KIRP"
              )
            )
          ),
          tabPanel(
            "Adrenal Gland",
            checkboxGroupInput(
              inputId = ns("Adrenal_Gland"), label = NULL, inline = TRUE,
              choices = list(
                "Adrenocortical Carcinoma(ACC)" = "ACC",
                "Pheochromocytoma and Paraganglioma(PCPG)" = "PCPG"
              )
            )
          ),
          tabPanel(
            "Brain",
            checkboxGroupInput(
              inputId = ns("Brain"), label = NULL, inline = TRUE,
              choices = list(
                "Glioblastoma Multiforme(GBM)" = "GBM",
                "Brain Lower Grade Glioma(LGG)" = "LGG"
              )
            )
          ),
          tabPanel(
            "Colorectal",
            checkboxGroupInput(
              inputId = ns("Colorectal"), label = NULL, inline = TRUE,
              choices = list(
                "Colon Adenocarcinoma(COAD)" = "COAD",
                "Rectum Adenocarcinoma(READ)" = "READ"
              )
            )
          ),
          tabPanel(
            "Lung",
            checkboxGroupInput(
              inputId = ns("Lung"), label = NULL, inline = TRUE,
              choices = list(
                "Lung Adenocarcinoma(LUAD)" = "LUAD",
                "Lung Squamous Cell Carcinoma(LUSC)" = "LUSC"
              )
            )
          ),
          tabPanel(
            "Uterus",
            checkboxGroupInput(
              inputId = ns("Uterus"), label = NULL, inline = TRUE,
              choices = list(
                "Uterine Corpus Endometrial Carcinoma(UCEC)" = "UCEC",
                "Uterine Carcinosarcoma(UCS)" = "UCS"
              )
            )
          ),
          tabPanel(
            "Bile Duct",
            checkboxGroupInput(
              inputId = ns("Bile_Duct"), label = NULL, inline = TRUE,
              choices = list("Bladder Urothelial Carcinoma(BLCA)" = "BLCA")
            )
          ),
          tabPanel(
            "Bone Marrow",
            checkboxGroupInput(
              inputId = ns("Bone_Marrow"), label = NULL, inline = TRUE,
              choices = list("Acute Myeloid Leukemia(LAML)" = "LAML")
            )
          ),
          tabPanel(
            "Breast",
            checkboxGroupInput(
              inputId = ns("Breast"), label = NULL, inline = TRUE,
              choices = list("Breast Invasive Carcinoma(BRCA)" = "BRCA")
            )
          ),
          tabPanel(
            "Cervix",
            checkboxGroupInput(
              inputId = ns("Cervix"), label = NULL, inline = TRUE,
              choices = list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)" = "CESC")
            )
          ),
          tabPanel(
            "Other tissues",
            checkboxGroupInput(
              inputId = ns("other_tissue"), label = NULL, inline = TRUE,
              choices = list(
                "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma(DLBC)" = "DLBC",
                "Esophageal Carcinoma(ESCA)" = "ESCA",
                "Stomach Adenocarcinoma(STAD)" = "STAD",
                "Head and Neck Squamous Cell Carcinoma(HNSC)" = "HNSC",
                "Liver Hepatocellular Carcinoma(LIHC)" = "LIHC",
                "Mesothelioma(MESO)" = "MESO",
                "Ovarian Serous Cystadenocarcinoma(OV)" = "OV",
                "Pancreatic Adenocarcinoma(PAAD)" = "PAAD",
                "Prostate Adenocarcinoma(PRAD)" = "PRAD",
                "Sarcoma(SARC)" = "SARC",
                "Skin Cutaneous Melanoma(SKCM)" = "SKCM",
                "Testicular Germ Cell Tumors(TGCT)" = "TGCT",
                "Thyroid Carcinoma(THCA)" = "THCA",
                "Thymoma(THYM)" = "THYM",
                "Uveal Melanoma(UVM)" = "UVM"
              )
            )
          )
        )
      ),
      shiny::tags$hr(width = "85%")
    )
  )
}
# cancerType server function ----------------------------------------------
# pair with cancerTypeInput in functions_ui.R##
# Call by *_*_server.R by callModule(cancerType,"id pair with UI part")
cancerType <- function(input, output, session) {
  cancer_type <- reactive({
    c(
      input$Kidney, input$Adrenal_Gland, input$Brain, input$Colorectal,
      input$Lung, input$Uterus, input$Bile_Duct, input$Bone_Marrow, input$Breast,
      input$Cervix, input$other_tissue
    ) -> cancer_type
  })
  return(cancer_type)
}

resetcancerType <- function(input, output, session) {
  shinyjs::reset("Kidney")
  shinyjs::reset("Adrenal_Gland")
  shinyjs::reset("Brain")
  shinyjs::reset("Colorectal")
  shinyjs::reset("Lung")
  shinyjs::reset("Uterus")
  shinyjs::reset("Bile_Duct")
  shinyjs::reset("Bone_Marrow")
  shinyjs::reset("Breast")
  shinyjs::reset("Cervix")
  shinyjs::reset("other_tissue")
  cancer_type <- reactive({
    c("") -> cancer_type
  })
  return(cancer_type)
}


###############################################################
# Plot function to generate plot in ui#########################
## 1. plotoutout in ui and in server ###########################
## 2. draw specific pic type by calling different function  ######
## PlotInput & Plot#################################
###############################################################
# call in ui by PlotInput("cnv_pie",..) OR  PlotInput("cnv_bar",..)
# call in ser by callModule(Plot,"cnv_pie",...) OR ...
PlotInput <- function(id, width, height) {
  ns <- NS(id)

  tagList(
    plotOutput(ns("plot")),
    hr()
    # sliderInput(ns("num"),label = "Select size of number",min=10,max = 100,value = 50)
  )
}

Plot <- function(input, output, session) { # data(raw data, gene set, cancer types),plot type(decide function type)
  # size <- reactive(as.numeric(input$num))
  # x <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  #   })
  # y <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  # })
  x <- rnorm(50)
  y <- nrow(50)

  output$plot <- renderPlot({
    plot(x, y)
  }) # fun argument decide what function will be called.
}

# cnv Point plot --------------------------------------------------------------


pointPlot <- function(input, output, session, data, cancer, gene, size, color, sizename, colorname, wrap) {

  # Example: callModule(pointPlot,"cnv_pie",data=cnv_plot_ready_1,cancer="cancer_types",
  #                     gene="symbol",size="per",color="color",sizename="CNV%",
  #                     colorname="SCNA Type",wrap="~ effect")
  # data should include x/y, point size and point color.
  output$plot <- renderPlot({
    data %>%
      ggplot(aes_string(y = gene, x = cancer)) +
      geom_point(aes_string(size = size, color = color)) +
      xlab("Cancer type") +
      ylab("Symbol") +
      scale_size_continuous(
        name = sizename,
        breaks = c(0.05, 0.1, 0.2, 0.4, 0.6, 1),
        limits = c(0.05, 1),
        labels = c("5", "10", "20", "40", "60", "100")
      ) +
      ggthemes::scale_color_gdocs(
        name = colorname,
        labels = c("Deletion", "Amplification")
      ) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      facet_wrap(as.formula(wrap)) +
      theme(strip.text.x = element_text(size = 15)) -> p
    return(p)
  })
}


# cnv pie plot ----------------------------------------------------------------

piePlot <- function(input, output, session, data, y, fill, facet_grid) {
  # Example:
  # callModule(piePlot,"cnv_pie",data=pie_plot_ready,y="per",
  #            fill="type",facet_grid="cancer_types ~ symbol")
  # data should include ...
  output$plot <- renderPlot({
    data %>%
      ggplot(aes_string(x = factor(1), y = y, fill = fill)) +
      geom_bar(stat = "identity", position = "stack", color = NA) +
      # scale_y_continuous(limits = c(0,1))
      coord_polar("y") +
      facet_grid(as.formula(facet_grid)) + # cancer_types ~ symbol
      # scale_x_discrete(limits = cnv_gene_rank$symbol) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),

        strip.text.y = element_text(angle = 0, hjust = 0, size = 11),
        strip.text.x = element_text(size = 11, angle = 90, vjust = 0),
        strip.background = element_blank(),

        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = "bottom",

        panel.background = element_blank(),
        panel.spacing = unit(0.02, "lines")
      ) +
      scale_fill_manual(
        limits = c("a_hete", "a_homo", "d_hete", "d_homo", "other"),
        label = c("Hete Amp", "Homo Amp", "Hete Del", "Homo Del", "None"),
        # Amp RColorBrewer name = "Spectral"
        # Del RColorBrewer name = "BrBG"
        values = c("brown1", "brown4", "aquamarine3", "aquamarine4", "grey")
      ) -> p
    return(p)
  })
}


# gene set CNV frenquencey in each cancer ---------------------------------
# bar stak plot
cnvbarPlot <- function(input, output, session, data, x, y, fill) {
  # Example:
  # callModule(piePlot,"cnv_pie",data=pie_plot_ready,y="per",
  #            fill="type",facet_grid="cancer_types ~ symbol")
  # data should include ...
  output$plot <- renderPlot({
    data %>%
      ggplot(aes_string(x = x, y = y, fill = fill)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~cnv_type, ncol = 2) +
      theme(strip.text.x = element_text(size = 15)) +
      ggsci::scale_fill_npg(
        name = "Type",
        limits = c("amp_a", "amp_s", "del_a", "del_s"),
        labels = c("Amp", "Amp Only", "Del", "Del Only")
      ) +
      ggthemes::theme_gdocs() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3)
      ) +
      labs(x = "Cancer Types", y = "CNV Frequency") -> p
    return(p)
  })
}


# snv percentage plot -----------------------------------------------------

snv_per_heatmap <- function(input, output, session, data, cancer, gene, fill, label, cancer_rank, gene_rank) {
  output$plot <- renderPlot({
    data %>%
      ggplot(aes_string(x = cancer, y = gene, fill = fill)) +
      geom_tile() +
      geom_text(aes_string(label = label)) +
      scale_x_discrete(position = "top", limits = cancer_rank$x_label) +
      scale_y_discrete(limits = gene_rank$symbol) +
      scale_fill_gradient2(
        name = "Mutation Frequency (%)",
        limit = c(0, 0.8),
        breaks = seq(0, 1, 0.1),
        label = c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"),
        high = "red",
        na.value = "white"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = -0.05, size = "15"),
        axis.title.y = element_text(size = "15")
      ) +
      guides(fill = guide_legend(
        title = "Mutation Frequency (%)",
        title.position = "left",
        title.theme = element_text(angle = 90, vjust = 2),
        reverse = T,
        keywidth = 0.6,
        keyheight = 0.8
      )) +
      labs(x = "", y = "") -> p
    return(p)
  })
}


# snv survival point plot -------------------------------------------------

snv_sur_pointPlot <- function(input, output, session, data, cancer, gene, size, color, cancer_rank,gene_rank,sizename, colorname) {
  # Example: callModule(pointPlot,"cnv_pie",data=cnv_plot_ready_1,cancer="cancer_types",
  #                     gene="symbol",size="per",color="color",sizename="CNV%",
  #                     colorname="SCNA Type",wrap="~ effect")
  # data should include x/y, point size and point color.
  output$plot <- renderPlot({
    data %>%
      ggplot(aes_string(y = gene, x = cancer)) +
      geom_point(aes_string(size = size, color = color)) +
      xlab("Cancer type") +
      ylab("Symbol") +
      scale_x_discrete(limit=cancer_rank$cancer_types) +
      scale_y_discrete(limit=gene_rank$symbol) +
      scale_size_continuous(
        name = sizename,
        breaks = c(-log10(0.05), 5, 10, 15),
        limits = c(-log10(0.05), 15),
        labels = c("0.05", latex2exp::TeX("$10^{-5}$"), latex2exp::TeX("$10^{-10}$"), latex2exp::TeX("$< 10^{-15}$"))
      ) +
      ggthemes::scale_color_gdocs(
        name = colorname,
        labels = c("High", "Low")
      ) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  -> p
    return(p)
  })
}


# snv maf summary ---------------------------------------------------------

# 1. ui part -----------------------------------------------------------------

snvPlotInput <- function(id, width, height) {
  ns <- NS(id)
  
  tagList(
    imageOutput(ns("plot")),
    hr()
  )
}

# 2. server part ----------------------------------------------------------

snv_maf_summaryPlot <- function(input, output, session, gene_list_maf, figname) {
  output$plot <-renderImage({
    outfile <- paste(user_dir,"/",figname,'.png',sep="")
    png(outfile, width = 400, heights= 300)
    maftools::plotmafSummary(gene_list_maf)
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
}

snv_maf_oncoPlot <-function(input, output, session, gene_list_maf, figname,cancer_type) {
  output$plot <-renderImage({
    outfile <- paste(user_dir,"/",figname,'.png',sep="")
    png(outfile, width = 400, heights= 300)
    col <- RColorBrewer::brewer.pal(n = 8, name = "Paired")
    names(col) <- c(
      "Frame_Shift_Del", "Missense_Mutation", "Nonsense_Mutation", "Multi_Hit", "Frame_Shift_Ins",
      "In_Frame_Ins", "Splice_Site", "In_Frame_Del"
    )
    fabcolors <- RColorBrewer::brewer.pal(n = length(cancer_type), name = "Spectral")
    names(fabcolors) <- cancer_type

    fabcolors <- list(cancer_types = fabcolors)
    maftools::oncoplot(
      maf = gene_list_maf, removeNonMutated = T, colors = col,
      clinicalFeatures = "cancer_types", sortByAnnotation = TRUE,
      annotationColor = fabcolors, top = 10
    )
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
}



