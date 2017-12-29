# sourced by 'tcga_meth_ui.R'
# save as 'tcga_meth_help.R'
# help file for tcga_meth ui part

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left",
                                                               HTML('<a data-toggle="collapse" href="#sqTCGA_meth">
                                                                    <i class="fa fa-question fa-fw"></i> 
                                                                    Click here for help</a>')
                                                               )
                                                               ),
                                shiny::tags$div(id="sqTCGA_meth", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       # overall description----
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What is Methylation ?", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","DNA methylation is a key epigenetic modification to cytosines, often in CpG dinucleotides. This modification has been frequently associated with gene silencing, but the precise role of DNA methylation in development and disease remains a mystery."),
                                                                              
                                                                              shiny::tags$br(),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                       ),
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What you can get.", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","1. Genes methylated differentially between tumor and normal samples."),
                                                                              shiny::tags$p(class="text-justify","2. Genes whose overall survival significantly different from hypermethylation to hypomethylation."),
                                                                              shiny::tags$p(class="text-justify","3. Genes whose expression significant correlate with methylation level."),
                                                                              shiny::tags$br(),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                       ),
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("Method", class="text-success"),
                                                                              
                                                                              shiny::tags$h4("Differential Methylation"),
                                                                              shiny::tags$p(class="text-justify","Cancers with more than 10 tumor-normal pairs will be have a calculation between tumor and normal, but not only paired samples were included. And a student T test were performed to define the methylation difference between tumor and normal samples, p value <0.05 was considered as significant."),
                                                                              
                                                                              shiny::tags$h4("Methylation survival"),
                                                                              shiny::tags$p(class="text-justify","Methylation data and clinical overall survival data was combined, and methylation level of gene was divided into 2 groups by middle methylation. Cox regression was performed to estimate the hazards of high methylation group, and a log rank test was also performed to compare the distributions of two groups, p value <0.05 was considered as significant."),
                                                                              
                                                                              shiny::tags$h4("Methylation correlate to expression"),
                                                                              shiny::tags$p(class="text-justify","Methylation can influence the expression of gene in theory. Use person correlation to get the relationship between gene expression and methylation level, gene with FDR<=0.05 will be  remained. From this, we can have genes whose expression is significant influenced by genome methylation.
"),
                                                                              shiny::tags$br(),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                       ),
                                                                       # result description----
                                                                       column(width=10, offset = 1,
                                                                              shiny::tags$h3("Results commentary", class="text-success"),
                                                                              shiny::tags$table(class="table table-striped",
                                                                                                
                                                                                                shiny::tags$thead(
                                                                                                  shiny::tags$th("Results type"),
                                                                                                  shiny::tags$th("Description")
                                                                                                ),
                                                                                                shiny::tags$tbody(
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Differential Methylation"),
                                                                                                    shiny::tags$td("Differential Methylation bubble plot show you genes' methylation change between tumor and normal samples in each cancers. Blue points represent a methylation upregulation in tumors, red points represent a methylation downregulation in tumors, the deeper of color, the higher the difference. And size of the point represents statistic significance, the bigger of size, the more significantly.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Methylation Survival"),
                                                                                                    shiny::tags$td("Gives you a survival difference between samples with a specific genes' high and low-methylation, only p value significant(<=0.05) genes will be displayed here. Red point represents low worse of high methylation group, blue point is just the opposite. Size of the point represents statistic significance, the bigger of size, the more significantly.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Methylation to Expression"),
                                                                                                    shiny::tags$td("Give you a person correlation between methylation and gene expression. Blue represent a negative correlation (means when methylation upregulate, the gene expression downregulate in stead of upregulate, they have opposite trend), and red represent positive correlation (means when methtlation upregulate, the gene expression upregulate too, they have consistent trend), the deeper of color, the higher the correlation. And size of the point represents statistic significance, the bigger of size, the more significantly.")
                                                                                                  )
                                                                                                )
                                                                              )
                                                                       )# result description end
                                                                       
                                                                       
                                                                )# all description end
                                                )
                                )
                                                )
                
         )
         )