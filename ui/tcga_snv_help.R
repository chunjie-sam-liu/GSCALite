# sourced by 'tcga_snv_ui.R'
# save as 'tcga_snv_help.R'
# help file for tcga_snv ui part


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left",
                                                               HTML('<a data-toggle="collapse" href="#sqTCGA_snv">
                                                                    <i class="fa fa-question fa-fw"></i> 
                                                                    Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="sqTCGA_snv", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What is Single Nucleotide Mutations", class="text-success"),
                                                                              
                                                                              
                                                                              
                                                                              shiny::tags$p(class="text-justify",
                                                                                            
                                                                                            "Single Nucleotide Mutation (SNV) is a variation in a single nucleotide that is the most common form of intra-species variation. It can play an important role in human traits and complex diseases. Here we give you a global profile as well as details status of SNV of your gene set for the cancers you interested in."),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                              
                                                                       ),
                                                                       column(width=10, offset = 1,
                                                                              shiny::tags$h3("Results commentary", class="text-success"),
                                                                              shiny::tags$table(class="table table-striped",
                                                                                                
                                                                                                shiny::tags$thead(
                                                                                                  shiny::tags$th("Results type"),
                                                                                                  shiny::tags$th("Description")
                                                                                                ),
                                                                                                shiny::tags$tbody(
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Summay plot"),
                                                                                                    shiny::tags$td("A summary plot displays number of variants in each sample as a stacked barplot and variant types as a boxplot summarized.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("SNV Heatmap"),
                                                                                                    shiny::tags$td("A heatmap gives a global SNV percentage of each genes in each cancer types.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Oncoplot"),
                                                                                                    shiny::tags$td("A oncoplot also known as waterfall plots, gives a mutation distribution of top mutated genes in your gene set and a SNV classification of SNV types (include missense mutation, frame shift deletion, nonsense mutation etc.) . All cancers' sample will be show together. Side barplot and top barplots show number of variants in each sample or each gene.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Oncostrip"),
                                                                                                    shiny::tags$td("Similar to OncoPrinter tool on cBioPortal and oncoplot above, oncostrip draws mutations in each sample. Less samples is recommended for a clearer plot.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Lollipop graph"),
                                                                                                    shiny::tags$td("Lollipop plots are simple and most effective way showing mutation spots on protein structure. Many oncogenes have a preferential sites which are mutated more often than any other locus. These spots are considered to be mutational hot-spots and lollipop plots can be used to display them along with rest of the mutations.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("SNV survival"),
                                                                                                    shiny::tags$td("A survival plot will give you a survival difference between mutate and non-mutate gene, only a p value significant(<=0.05) gene will be displayed here.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Mutation load"),
                                                                                                    shiny::tags$td("Mutation load gives gene's mutation against TCGA median mutation, which act us a background. TCGA contains over 30 different cancer cohorts and median mutation load across them varies from as low as 7 per exome (Pheochromocytoma and Paraganglioma arising from Adrenal Gland) to as high as 315 per exome (Skin Cutaneoys Melanoma). It is informative to see how mutation load in given maf stands against TCGA cohorts.")
                                                                                                  )
                                                                                                )
                                                                                            )
                                                                              )
                                                                       
                                                                       
                                                                )
                                                )
                                )
                )
         )
)