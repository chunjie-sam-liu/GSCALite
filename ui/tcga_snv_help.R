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
                                                                              
                                                                              shiny::tags$h3("What is Single Nucleotide Mutation", class="text-success"),
                                                                              
                                                                              
                                                                              
                                                                              shiny::tags$p(class="text-justify",
                                                                                            
                                                                                            "Single Nucleotide Mutation(SNV) is a variation in a single nucleotide that is the most common form of intra-species variation. "),
                                                                              
                                                                              shiny::tags$strong("Please check out the following article, in which PCA on genomic data is nicely explained."),
                                                                              
                                                                              HTML("<a href='http://www.nature.com/nbt/journal/v26/n3/full/nbt0308-303.html' target='_blank'><i class='fa fa-external-link  fa-fw'></i>Nature Computational Biology</a>")
                                                                              
                                                                       ),
                                                                       column(width=10, offset = 1,
                                                                              shiny::tags$h3("Results commentary", class="text-success"),
                                                                              shiny::tags$table(class="text-justify",
                                                                                            "")
                                                                              )
                                                                       
                                                                       
                                                                )
                                                )
                                )
                )
         )
)