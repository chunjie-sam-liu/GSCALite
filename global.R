# source by server.R
# source by ui.R
# saved as functions_server.R

##########################################
# Cancer selection data get and comfim####
## 1. cancer select for each part ui ######
## 2. cancer type selection confirm  ######
## cancerTypeInput & cancerType############
##########################################

# GTEx normal tissue choice ##############
tabPannel_element_ten <- c()
for(i in gtex_expr.tissues[1:10]){
  tmp_XXXX <- list(i=i);names(tmp_XXXX <-i);
  assign(paste("GTEx_",i,"_choice",sep = ""),tmp_XXXX)
  tabPannel_element_ten <-c(tabPannel_element_ten,
                            paste("tabPanel(\"",i,"\",checkboxGroupInput(inputId = ns(\"",i,"\"), label = NULL, inline = TRUE,choices = ",paste("GTEx_",i,"_choice)),",sep=""),sep="")
  )  
}
GTEx_other_tissue_choice <- gtex_expr.tissues[11:tissue_n]
GTEx_input_choice <- c(paste("input$",gtex_expr.tissues[1:10],sep=""),"input$other_tissue")


GTExTissueTypeInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # tissue type selection----
      column(
        width = 10,
        offset = 1,
        shiny::tags$br(),
        shiny::tags$h3("GTEx normal tissue Selection", class = "text-success"),
        shiny::tags$br(),
        
        shinydashboard::tabBox(
          width = 12, title = "Tissue",
          eval(tabPannel_element_ten),
          tabPanel(
            "Other tissues",
            checkboxGroupInput(
              inputId = ns("other_tissue"), label = NULL, inline = TRUE,
              choices = GTEx_other_tissue_choice
            )
          )
        )
      ),
      shiny::tags$hr(width = "85%")
    )
  )
}

GTEx_normal_Tissue <- function(input, output, session) {
  GTEx_normal_tissue <- reactive({eval(parse(text = GTEx_input_choice)) -> GTEx_normal_tissue })

  return(GTEx_normal_tissue)
}

resetGTExTissueType <- function(input, output, session){
  for(i in c(tabPannel_element_ten,"other_tissue")){
    shinyjs::reset(i)
  }
  GTEx_normal_tissue <- reactive({
    c("") -> GTEx_normal_tissue
  })
  return(GTEx_normal_tissue)
}


# cancer type choice ------------------------------------------------------

Kidney_choice <- list(
  "Kidney Chromophobe(KICH)" = "KICH",
  "Kidney Renal Clear Cell Carcinoma(KIRC)" = "KIRC",
  "Kidney Renal Papillary Cell Carcinoma(KIRP)" = "KIRP"
)
Adrenal_Gland_choice <- list(
  "Adrenocortical Carcinoma(ACC)" = "ACC",
  "Pheochromocytoma and Paraganglioma(PCPG)" = "PCPG"
)
Brain_choice <- list(
  "Glioblastoma Multiforme(GBM)" = "GBM",
  "Brain Lower Grade Glioma(LGG)" = "LGG"
)
Colorectal_choice <- list(
  "Colon Adenocarcinoma(COAD)" = "COAD",
  "Rectum Adenocarcinoma(READ)" = "READ"
)
Lung_choice <- list(
  "Lung Adenocarcinoma(LUAD)" = "LUAD",
  "Lung Squamous Cell Carcinoma(LUSC)" = "LUSC"
)
Uterus_choice <- list(
  "Uterine Corpus Endometrial Carcinoma(UCEC)" = "UCEC",
  "Uterine Carcinosarcoma(UCS)" = "UCS"
)
Bile_Duct_choice <- list("Bladder Urothelial Carcinoma(BLCA)" = "BLCA")
Bone_Marrow_choice <- list("Acute Myeloid Leukemia(LAML)" = "LAML")
Breast_choice <- list("Breast Invasive Carcinoma(BRCA)" = "BRCA")
Cervix_choice <- list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)" = "CESC")
other_tissue_choice <- list(
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
            checkboxGroupButtons(
              inputId = ns("Kidney"), label = NULL,
              choices = Kidney_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Adrenal Gland",
            checkboxGroupButtons(
              inputId = ns("Adrenal_Gland"), label = NULL,
              choices = Adrenal_Gland_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Brain",
            checkboxGroupButtons(
              inputId = ns("Brain"), label = NULL,
              choices = Brain_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Colorectal",
            checkboxGroupButtons(
              inputId = ns("Colorectal"), label = NULL,
              choices = Colorectal_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Lung",
            checkboxGroupButtons(
              inputId = ns("Lung"), label = NULL,
              choices = Lung_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Uterus",
            checkboxGroupButtons(
              inputId = ns("Uterus"), label = NULL,
              choices = Uterus_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Bile Duct",
            checkboxGroupButtons(
              inputId = ns("Bile_Duct"), label = NULL,
              choices = Bile_Duct_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Bone Marrow",
            checkboxGroupButtons(
              inputId = ns("Bone_Marrow"), label = NULL,
              choices = Bone_Marrow_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Breast",
            checkboxGroupButtons(
              inputId = ns("Breast"), label = NULL,
              choices = Breast_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Cervix",
            checkboxGroupButtons(
              inputId = ns("Cervix"), label = NULL,
              choices = Cervix_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Other tissues",
            checkboxGroupButtons(
              inputId = ns("other_tissue"), label = NULL,
              choices = other_tissue_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
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

resetcancerType <- function(input, output, session){
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

# selectallCancer <- funtion(input, output, session){
#   # updateCheckboxGroupInput(session,"Kidney",choices=Kidney_choice,selected=Kidney_choice)
#   
#   # cancer_type <- reactive({
#   #   c("KICH","KIRC","KIRP") -> cancer_type
#   # })
#   # return(cancer_type)
# }

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

#####################GTEx expression heatmap plot by zhangq#########################

heatmap_GTEX_Plot <- function(input, output, session, data){
  output$plot <- renderPlot({
  ggplot(data, aes(Tissue, GeneName)) +
    geom_tile(aes(fill = RPKM)) + 
    geom_text(aes(label = RPKM)) +
    scale_fill_gradient(low = "white", high = "red")+
    labs(title = "Expression value of query genes in GTEx dataset")+
    theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(angle = 45, hjust = 1)) -> p
    return(p)
  })
}

box_GTEx_GSVA_Plot <- function(input, output, session, data){
  output$plot <- renderPlot({
  data %>% 
    dplyr::group_by(SMTS) %>% 
    dplyr::summarise(m = median(gsva)) %>% 
    dplyr::arrange(m) %>% dplyr::pull(SMTS) -> lev
  tcc <- tibble(SMTS=lev,color=rainbow(length(lev)))
  data %>% 
    dplyr::mutate(SMTS = factor(SMTS, levels = lev)) %>% 
      ggplot(aes(x = SMTS, y = gsva)) +
      stat_boxplot(geom = 'errorbar', width = 0.3) +
      geom_boxplot(outlier.colour = NA) +
      geom_point(aes(color = SMTS), position = position_jitter(width = 0.05), alpha = 0.4,size = 0.8) +
      scale_color_manual(name = "Tissues",values = dplyr::slice(tcc, match(lev, SMTS)) %>% dplyr::pull(color)) +
      theme(axis.line = element_line(color = "black"),axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90),
	panel.grid = element_blank(),panel.background = element_rect(fill = "white", color = NA),panel.spacing.x = unit(0, "lines")) +
      guides(color = F) +labs(x = "Tissue", y = "GSVA Score", title = "") -> p
  return(p)
  })
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

piePlot <- function(input, output, session, data, y, fill, facet_grid,outfile,height) {
  # Example:
  # callModule(piePlot,"cnv_pie",data=pie_plot_ready,y="per",
  #            fill="type",facet_grid="cancer_types ~ symbol")
  # data should include ...
  output$plot <- renderImage({
    
    data %>%
      ggplot(aes_string(x = factor(1), y = y, fill = fill)) +
      geom_bar(stat = "identity", position = "stack", color = NA) +
      # scale_y_continuous(limits = c(0,1))
      coord_polar("y") +
      facet_grid(as.formula(facet_grid)) + # cancer_types ~ symbol
      # scale_x_discrete(limits = cnv_gene_rank$symbol) +
      # scale_x_discrete(expand=c(0,0)) +
      # scale_y_discrete(expand=c(0,0)) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),

        strip.text.y = element_text(angle = 0, hjust = 0, size = 4),
        strip.text.x = element_text(size = 4, angle = 90, vjust = 0),
        strip.background = element_blank(),

        legend.title = element_blank(),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.key.size = unit(0.25,"cm"),

        panel.background = element_blank(),
        panel.spacing = unit(0, "null"),#unit(0.01, "lines"),
        panel.spacing.x  = unit(0,"null"),
        
        plot.margin = rep(unit(0,"null"),4),
        axis.ticks.length = unit(0,"cm")
      ) +
      scale_fill_manual(
        limits = c("a_hete", "a_homo", "d_hete", "d_homo", "other"),
        label = c("Hete Amp", "Homo Amp", "Hete Del", "Homo Del", "None"),
        # Amp RColorBrewer name = "Spectral"
        # Del RColorBrewer name = "BrBG"
        values = c("brown1", "brown4", "aquamarine3", "aquamarine4", "grey")
      ) ->p
    
    # outfile <- paste("/project/huff/huff/github/GSCALite/userdata","/","TCGA_cnv_pie_rellation_network",'.png',sep="")
    ggsave(outfile,p,device ="png",width =4,height = height)
    list(src = outfile,
         contentType = 'image/png',
         # width = 400,
         # height = "900px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
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

snv_sur_pointPlot <- function(input, output, session, data, cancer, gene, size, color, cancer_rank,gene_rank,sizename, colorname,title) {
  # Example: callModule(pointPlot,"cnv_pie",data=cnv_plot_ready_1,cancer="cancer_types",
  #                     gene="symbol",size="per",color="color",sizename="CNV%",
  #                     colorname="SCNA Type",wrap="~ effect")
  # data should include x/y, point size and point color.
  output$plot <- renderPlot({
    data %>%
      ggplot(aes_string(y = gene, x = cancer)) +
      geom_point(aes_string(size = size, color = color)) +
      labs(title = title) +
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
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 10),
            axis.text.y = element_text(size = 10),
            legend.position = "bottom",
            panel.background = element_rect(colour = "black", fill = "white"),
            panel.grid = element_line(colour = "grey", linetype = "dashed"),
            panel.grid.major = element_line(
              colour = "grey",
              linetype = "dashed",
              size = 0.2) ,
            plot.title = element_text(size = 20),
            plot.margin=grid::unit(c(0,0,0,0), "mm")
            )  -> p
    return(p)
  })
}



# snv maf summary ---------------------------------------------------------

# 1. ui part -----------------------------------------------------------------

imagePlotInput <- function(id, width=400, height=300) {
  ns <- NS(id)
  
  tagList(
    imageOutput(ns("plot"),width = width, height = height),
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


# methylation plot --------------------------------------------------------


# 1. methy diff -----------------------------------------------------------
methy_diff_pointPlot <- function(input, output, session, data, cancer, gene, size, color, cancer_rank,gene_rank,sizename, colorname,title) {
  
  output$plot <- renderPlot({
    CPCOLS <- c("red", "white", "blue")
    data %>%
      ggplot(aes_string(x=cancer,y=gene)) +
      geom_point(aes_string(size = size,color = color)) +
      scale_y_discrete(limit = gene_rank$symbol) +
      scale_x_discrete(limit = cancer_rank$cancer_types) +
      labs(title = title) +
      ylab("Symbol") +
      xlab("Cancer types") +
      scale_size_continuous(
        name = sizename #"-Log10(FDR)"
      ) +
      scale_color_gradient2(
        name = colorname, #"Methylation diff (T - N)",
        low = CPCOLS[3],
        mid = CPCOLS[2],
        high = CPCOLS[1]
      ) +
      theme(legend.position = "bottom",
            panel.background = element_rect(colour = "black", fill = "white"),
            panel.grid = element_line(colour = "grey", linetype = "dashed"),
            panel.grid.major = element_line(
              colour = "grey",
              linetype = "dashed",
              size = 0.2),
            axis.text.x = element_text(angle = 40,vjust=1,hjust = 1,size = 10),
            axis.text.y = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.key = element_rect(fill = "white", colour = "black") ,
            plot.title = element_text(size=20)
      )-> p
    return(p)
    })
}




# rppa --------------------------------------------------------------------
# line contact ----
rppa_line_contact <- function(input, output, session, seg, cancer, gene, pathway,title){
  output$plot <- renderImage({
    
    ggplot() +
      geom_segment(data = seg, mapping = aes_string(
        x = x1, 
        y = y1,
        xend = x2,
        yend = y2,
        colour = Cancer,
        linetype = Regulation
      )) +
      guides(color=FALSE) +
      geom_text(
        data = cancer, 
        mapping = aes_string(x = x, y = y, label = text,color=text),
        hjust = 1
      ) +
      geom_text(
        data = gene, 
        mapping = aes_string(x = x, y = y-0.15, label = text)) +
      geom_text(
        data = path, 
        mapping = aes_string(x = x, y = y, label = text),
        hjust=0) +
      expand_limits(x=c(0,7)) +
      theme(
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) + 
      labs(title = title) ->p 
    return(p)
    # outfile <- paste("/project/huff/huff/github/GSCALite/userdata","/","TCGA_RPPA_rellation_network",'.png',sep="")
    # ggsave(outfile,p,device ="png",dpi = 300)
    # list(src = outfile,
    #      contentType = 'image/png',
    #      width=1200,
    #      height= "100%" ,
    #      alt = "This is alternate text")
  })
}

# rppa pie ----
rppaPiePlot <- function(input, output, session, data, y, fill, facet_grid,height,outfile) {
  # Example:
  # callModule(piePlot,"cnv_pie",data=pie_plot_ready,y="per",
  #            fill="type",facet_grid="cancer_types ~ symbol")
  # data should include ...
  output$plot <- renderImage({
    
    data %>%
      ggplot(aes_string(x = factor(1), y = y, fill = fill)) +
      geom_bar(stat = "identity", position = "stack", color = NA) +
      # scale_y_continuous(limits = c(0,1))
      coord_polar("y") +
      facet_grid(as.formula(facet_grid)) + #  symbol~ cancer_types
      # scale_x_discrete(limits = cnv_gene_rank$symbol) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        
        strip.text.y = element_text(angle = 0, hjust = 0, size = 5),
        strip.text.x = element_text(size = 5, angle = 90, vjust = 0),
        strip.background = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.25, "cm"),
        legend.position = "bottom",
        
        panel.background = element_blank(),
        panel.spacing = unit(0.02, "lines"),
        plot.margin = rep(unit(0,"null"),4),
        axis.ticks.length = unit(0,"cm")
      ) +
      scale_fill_manual(
        limits = c("Activation", "Inhibition", "None"),
        label = c("Activation", "Inhibition", "None"),
        # Amp RColorBrewer name = "Spectral"
        # Del RColorBrewer name = "BrBG"
        values = c("brown1", "aquamarine3", "grey")
      ) ->p
    
    
    ggsave(outfile,p,device ="png",width =4, height = height)
    list(src = outfile,
         contentType = 'image/png',
         # width = "100%" ,
         # height = 900,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
}

# rppa heatmap percent ----
rppa_heat_per <- function(input, output, session, rppa_per_ready, pathway,symbol, per, height,outfile){
  output$plot <- renderImage({
    rppa_per_ready %>%
      ggplot(aes(x = pathway, y = symbol))+
      xlab("Pathway")+ylab("Symbol") +
      guides(fill=guide_colorbar("Percent")) +
      geom_tile(aes(fill = per), col = "white") +
      geom_text(label=ceiling(rppa_per_ready$per),
                size = 1) +
      scale_fill_gradient2(
        high = "red",
        mid = "white",
        low = "blue"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1,size=5),
        axis.text.y = element_text(size=5),
        legend.key.size = unit(0.25, "cm"),
        legend.position = "bottom",
        plot.margin = rep(unit(0,"null"),4),
        axis.ticks.length = unit(0,"cm"),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size=6),
        axis.title.y = element_text(size=6),
        legend.title = element_text(size=6)
      ) + 
      xlab("Pathway (a:activate; i:inhibit)") ->p
    ggsave(outfile,p,device ="png",width =4, height = height)
    list(src = outfile,
         contentType = 'image/png',
         # width = "100%" ,
         # height = 900,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
}

