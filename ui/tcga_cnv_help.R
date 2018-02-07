# sourced by 'tcga_cnv_ui.R'
# save as 'tcga_cnv_help.R'
# help file for tcga_cnv ui part

fluidRow(
  style = "width:85%",
  column(
    width = 12,
    # collapsable panel
    shiny::tags$div(
      class = "panel panel-default",
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          HTML('<a data-toggle="collapse" href="#sqTCGA_cnv">
                                                                    <i class="fa fa-question fa-fw"></i> 
                                                                    Click here for help</a>')
        )
      ),
      shiny::tags$div(
        id = "sqTCGA_cnv", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          # here comes the content
          ###############
          column(
            width = 12,
            # overall description----
            column(
              width = 10, offset = 1,

              shiny::tags$h3("What is Copy number variation", class = "text-success"),

              shiny::tags$p(
                class = "text-justify", "Copy number variation is a type of structural variation: specifically, it is a type of duplication or deletion event that affects a considerable number of base pairs. See details at: ",
                shiny::tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1226196/", 
                              shiny::tags$span("A. J. Sharp et al.",style="color:#008176"),target = "_blank")

              ),
              # shiny::tags$p(class="text-justify",),

              shiny::tags$hr(width = "100%")
            ),
            # result description----
            column(
              width = 10, offset = 1,
              shiny::tags$h3("Results commentary", class = "text-success"),
              shiny::tags$table(
                class = "table table-striped",

                shiny::tags$thead(
                  shiny::tags$th("Results type",width=4),
                  shiny::tags$th("Description",width=8)
                ),
                shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("CNV Pie distribution"),
                    shiny::tags$td("CNV pie plot gives you a global profile that shows the constitute of Heterozygous and Homozygous CNV about each gene in each cancer. A pie represents the proportion of different types CNV of a gene in a cancer, and different color represent different types of CNV. Hete Amp: heterozygous amplification; Hete Del: heterozygous deletion; Homo Amp: homozygous amplification; Homo Del: homozygous deletion; None: no CNV.")
                  ),
                  shiny::tags$tr(
                    shiny::tags$td("Hete CNV profile"),
                    shiny::tags$td("Heterozygous CNV profile show you percentage of heterozygous cnv, including amplification and deletion percentage of heterozygous CNV about each gene in each cancer.")
                  ),
                  shiny::tags$tr(
                    shiny::tags$td("Homo CNV"),
                    shiny::tags$td("Homozygous CNV profile show you percentage of homozygous CNV, include the amplification and deletion percentage of homozygous CNV about each gene in each cancer.")
                  ),
                  # shiny::tags$tr(
                  #   shiny::tags$td("Overall CNV Frequency"),
                  #   shiny::tags$td("Overall CNV frenquency gives you a bar plot, which shows the sum frequency of all genes' CNV in each cancer type. For example, you searched 10 genes, red bar in Hete CNV: [Type Amp]=number(samples with any one of these genes have hete CNV amplification)/number(all samples), means this part of samples have at least one gene contains heterozygous amplification, but also may have genes contain hetero/homozygous deletion; wathet blue bar in Hete CNV: [Type Amp Only]=number(samples with any one of these genes only have hete CNV amplification)/number(all samples), means this part of samples have at least one gene contains only hete CNV amplification. So the sum of them may greater than 1.")
                  # ),
                  shiny::tags$tr(
                    shiny::tags$td("CNV correlate to gene expression"),
                    shiny::tags$td("CNV can influence the expression of gene in theory. Use person correlation to get the relationship between gene expression and cnv, gene with FDR<=0.05 will be remained. From this, we can get genes whose expression significantly regulated by CNV. Blue bubbles represent a negative correlation (means when gene having a high frequency of CNV, gene's expression downregulate, they have opposite trend), and red bubbles represent positive correlation (means when gene having a high frequency of CNV, the gene expression upregulate too, they have consistent trend), the deeper of color, the higher the correlation. And size of the point represents statistic significance, the bigger of size, the more statistic significant.")
                  ) # ,
                  # shiny::tags$tr(
                  #   shiny::tags$td("Exclude CNV"),
                  #   shiny::tags$td("Exclude CNV is a phenomenon that two genes' CNV happened different in a same sample all the time, this difference should meet a statistic difference at least.")
                  # )
                )
              )
            ) # result description end
          ) # all description end
        )
      )
    )
  )
)
