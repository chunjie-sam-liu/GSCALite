# sourced by 'tcga_meth_ui.R'
# save as 'tcga_meth_help.R'
# help file for tcga_meth ui part

fluidRow(
  style = "width:85%",
  column(
    width = 12,offset=0,
    # collapsable panel
    shiny::tags$div(
      class = "panel panel-primary",
      
      # panel head ----
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#sqTCGA_meth",
            shiny::icon(name = "info-circle"),
            "Click here for the detailed description of methods and results"
          )
        )
      ),
      
      # panel body ----
      shiny::tags$div(
        id = "sqTCGA_meth", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          # here comes the content
          ###############
          column(
            width = 12,
            # overall description----
            # shiny::tags$div(
            #   class = "bs-callout bs-callout-primary",
            #   shiny::tags$h3("Overall description"),
            # 
            #   shiny::tags$dl(
            #     class="dl-horizontal",
            #     shiny::tags$dt("What is Methylation"),
            #     shiny::tags$dd(
            #       "DNA methylation is a key epigenetic modification to cytosines, often in CpG dinucleotides. This modification has been frequently associated with gene silencing, but the precise role of DNA methylation in development and disease remains a mystery."
            #     ),
            #     
            #     shiny::tags$dt("What you can get"),
            #     shiny::tags$dd(
            #       shiny::tags$p("1. Genes methylated differentially between tumor and normal samples."),
            #       shiny::tags$p("2. Genes whose overall survival significantly different from hypermethylation to hypomethylation."),
            #       shiny::tags$p("3. Genes whose expression significantly influenced by methylation level.")
            #       )
            #     )
            #   ),
            
            shiny::tags$div(
              class = "bs-callout bs-callout-primary",
              shiny::tags$h3("Methods"),
              
              shiny::tags$dl(
                class="dl-horizontal",
                
                shiny::tags$dt("Data:"),
                shiny::tags$dd(
                  "We collected 10129 methy data from",
                  shiny::tags$a("href" = "https://gdc.cancer.gov/", "NCI Genomic Data Commons,"),
                  "including 33 cancer types, but only 14 cancer types have paired tumor vs. normal data, so differential methylation analysis was based on these 14 cancer types."
                ),
                
                shiny::tags$dt(shiny::tags$p("Differential"),
                               shiny::tags$p("Methylation")),
                shiny::tags$dd(
                  "Cancers with more than 10 tumor-normal pairs will be have a calculation between tumor and normal, but not only paired samples were included. And a student T test were performed to define the methylation difference between tumor and normal samples, p value was adjusted by FDR, FDR <= 0.05 was considered as significant."
                ),
                
                shiny::tags$dt(shiny::tags$p("Methylation"),
                               shiny::tags$p("Survival")),
                shiny::tags$dd(
                  "Methylation data and clinical overall survival data was combined, and methylation level of gene was divided into 2 groups by middle methylation. Cox regression was performed to estimate the hazards(risk of death), if Cox coef > 0, the high methylation group shows a worse survival, the Hyper_worse defined as High, otherwise defined as Low. And a log rank test was also performed to compare the distributions of two groups, p value <0.05 was considered as significant."
                ),
                
                shiny::tags$dt(shiny::tags$p("Methylation"),
                               shiny::tags$p("correlate to mRNA RPKM")),
                shiny::tags$dd(
                  "Methylation can influence the expression of gene in theory. The mRNA expression and methylation data were merged by TCGA barcode. We test the association between paired mRNA expression and methylation, based on Person's product moment correlation coefficient, and follows a t distribution. P-value was adjusted by FDR and genes with FDR<=0.05 will be remained. From this, we may get genes whose expression is significantly influenced by genome methylation."
                )
              )
            ),
            
            shiny::tags$div(
              class="bs-callout bs-callout-danger",
              shiny::tags$h3("Results"),
              
              shiny::tags$dl(
                class="dl-horizontal",
                shiny::tags$dt(shiny::tags$p("Differential"),
                               shiny::tags$p("Methylation")),
                shiny::tags$dd(
                  "Differential Methylation bubble plot show you genes' methylation change between tumor and normal samples in each cancers. Blue points represent a methylation upregulation in tumors, red points represent a methylation downregulation in tumors, the deeper of color, the higher the difference. And size of the point represents statistic significance, the bigger of size, the more significantly."
                ),
                
                shiny::tags$dt(shiny::tags$p("Methylation"),
                               shiny::tags$p("Survival")),
                shiny::tags$dd(
                  "Gives you a survival difference between samples with a specific genes' high and low-methylation, only logrank p value significant(<=0.05) genes will be displayed on the figure. Red point represents low worse of high methylation group, blue point is just the opposite. Size of the point represents statistic significance, the bigger of size, the more significantly."
                ),
                
                
                shiny::tags$dt(shiny::tags$p("Methylation"),
                               shiny::tags$p("To Expression")),
                shiny::tags$dd(
                  "Give you a person correlation between methylation and mRNA gene expression. Blue points represent negative correlation (means when the level of gene's methylation upregulate, the gene expression downregulate in stead of upregulate, they have opposite trend), and red represent positive correlation (means when the level of gene's methylation upregulate, the gene expression upregulate too, they have consistent trend), the deeper of color, the higher the correlation. And size of the point represents statistic significance, the bigger of size, the more significantly."
                )
              )
            )
          ) # all description end
        )
      )
    )
  )
)