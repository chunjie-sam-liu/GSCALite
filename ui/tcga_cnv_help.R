# sourced by 'tcga_cnv_ui.R'
# save as 'tcga_cnv_help.R'
# help file for tcga_cnv ui part

fluidRow(
  style = "width:85%",
  column(
    width = 12,
    # # use primary panel
    
    shiny::tags$div(
      class = "panel panel-primary",
      
      # panel head
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#sqTCGA_cnv",
            shiny::icon(name = "info-circle"),
            "Click here for the detailed description of methods and results"
          )
        )
      ),
      
      # panel body -----
      shiny::tags$div(
        id = "sqTCGA_cnv", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          # here comes the content
          ###############
          column(
            width = 12,
            # methods ----
            shiny::tags$div(
              class = "bs-callout bs-callout-primary",
              shiny::tags$h3("Methods"),
              
              shiny::tags$dl(
                class="dl-horizontal",
                shiny::tags$dt("Overall description:"),
                shiny::tags$dd(
                  "In this CNV module, we calculate the percentage of CNV, CNV correlation with mRNA of gene in each cancer type. The CNV was devided into 2 subtypes, heterozygous CNV and homozygous CNV, which represent the occurrence of CNV on only one chromosome or both two. Percentage statistic based on subtypes of CNV used GISTIC processed CNV data, and calculation of correlation used raw CNV data and mRNA RPKM data."
                ),
                shiny::tags$dt("Data:"),
                shiny::tags$dd(
                  "We collected 11495 CNV data from",
                  shiny::tags$a("href" = "https://gdc.cancer.gov/", "NCI Genomic Data Commons,"),
                  "and process them with GISTICS2.0",
                  shiny::tags$a("href" = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2011-12-4-r41", "(C. H. Mermel et al, 2011)"),
                  "The CNV statistic is based on GISTIC processed data, and the correlation between CNV and mRNA expression is based on CNV raw data."
                ),
                
                shiny::tags$dt("CNV Pie distribution:"),
                shiny::tags$dd(
                  "CNV pie plot gives you a global profile that shows the constitute of Heterozygous/Homozygous CNV of each gene in each cancer. A pie represents the proportion of different types of CNV of one gene in one cancer, and different color represent different types of CNV. "
                ),
                
                shiny::tags$dt("Hete CNV profile:"),
                shiny::tags$dd(
                  "Heterozygous CNV profile show you percentage of heterozygous cnv, including amplification and deletion percentage of heterozygous CNV about each gene in each cancer. Only genes with > 5% CNV in cancers will show corresponding point on the figure. The heterozygous CNV is generally happened so the plot will full of points, but most of researchs such as ",
                  shiny::tags$a(href="http://www.cbioportal.org/","cBioPortal"),
                  "mainly focus on homozygous CNV in the present."
                ),
                
                shiny::tags$dt("Homo CNV profile:"),
                shiny::tags$dd(
                  "Homozygous CNV profile show you percentage of homozygous CNV, including amplification and deletion percentage of homozygous CNV about each gene in each cancer. Only genes with > 5% CNV in cancers will show corresponding point on the figure. Most of researchs such as ",
                  shiny::tags$a(href="http://www.cbioportal.org/","cBioPortal"),
                  " mainly focus on homozygous CNV in the present."
                ),
                
                shiny::tags$dt("CNV correlate to gene expression:"),
                shiny::tags$dd(
                  "The mRNA expression and CNV data were merged by TCGA barcode. We test the association between paired mRNA expression and CNV percent samples, based on Person's product moment correlation coefficient, and follows a t distribution. P-value was adjusted by FDR. This method has been employed in ",
                  shiny::tags$a(href="https://genome.cshlp.org/content/21/12/2004.full","A. Schlattl et al,2011"),
                  "to relate copy-number to transcriptome sequencing data."
                )
              )
            ),
            
            # results part
            shiny::tags$div(
              class = "bs-callout bs-callout-danger",
              shiny::tags$h3("Results"),
              
              shiny::tags$dl(
                class = "dl-horizontal",
                
                shiny::tags$dt("CNV Pie distribution:"),
              shiny::tags$dd(
                "Hete Amp: heterozygous amplification; Hete Del: heterozygous deletion; Homo Amp: homozygous amplification; Homo Del: homozygous deletion; None: no CNV."
              ),
              
              shiny::tags$dt("Hete/Homo CNV profile:"),
              shiny::tags$dd(
                "Heterozygous/Homozygous CNV profile show you percentage of heterozygous/homozygous cnv, including amplification and deletion percentage of heterozygous/homozygous CNV about each gene in each cancer. Only genes with > 5% CNV in cancers will show corresponding points on the figure."
              ),
              
              shiny::tags$dt("CNV correlate to gene expression:"),
              shiny::tags$dd(
                "Genes whose mRNA expression significantly (FDR<=0.05) correlate with CNV percentage were shown on the figure. From this, we can get genes whose expression significantly regulated by CNV. Blue bubbles represent a negative correlation (means when gene having a high frequency of CNV, gene's expression downregulate, they have opposite trend), and red bubbles represent positive correlation (means when gene having a high frequency of CNV, the gene expression upregulate too, they have consistent trend), the deeper of color, the higher the correlation. And size of the point represents statistic significance, the bigger of size, the more statistic significant."
              )
              )
            )
          ) # all description end
        )
      )
    )
  )
)
