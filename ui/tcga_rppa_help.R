# sourced by 'tcga_rppa_ui.R'
# save as 'tcga_rppa_help.R'
# help file for tcga_rppa ui part

fluidRow(
  style = "width:85%",
  column(
    width = 12,
    # collapsable panel
    shiny::tags$div(
      class = "panel panel-primary",
      
      
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#help_tcga_rppa",
            shiny::icon(name = "info-circle"),
            "Click here for the detailed description of methods and results"
          )
        )
      ),

      shiny::tags$div(
        id = "help_tcga_rppa", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          # here comes the content
          ###############
          column(
            width = 12,
          shiny::div(
            class = "bs-callout bs-callout-primary",
            shiny::tags$h3("Method"),
            # overall description----
            shiny::tags$dl(
              class = "dl-horizontal",
              shiny::tags$dt("Data"),

              shiny::tags$dd(
                "RPPA data from TCPA are used to calculate score for 7876 samples, 10 cancer related pathways and 32 cancer types.",
                "Reverse phase protein array (RPPA) is a high-throughput antibody-based technique with the procedures similar to that of Western blots. Proteins are extracted from tumor tissue or cultured cells, denatured by SDS, printed on nitrocellulose-coated slides followed by antibody probe (",
                shiny::tags$a(
                  href = "http://bioinformatics.mdanderson.org/main/TCPA:Overview",
                  shiny::tags$span("TCPA database"), target = "_blank"
                ),
                "), TCPA RPPA data are all from TCGA samples."
              ),

              shiny::tags$dt("Pathways"),
              shiny::tags$dd(
                "The pathway we included in are: TSC/mTOR, RTK, RAS/MAPK, PI3K/AKT, Hormone ER, Hormone AR, EMT, DNA Damage Response, Cell Cycle, Apoptosis pathways. They are all famous cancer related pathway."
              ),

              shiny::tags$dt("Pathway score"),
              shiny::tags$dd(
                "RBN RPPA data were median-centered and normalized by standard deviation across all samples for each component to obtain the relative protein level. The pathway score is then the sum of the relative protein level of all positive regulatory components minus that of negative regulatory components in a particular pathway ",
                shiny::tags$a(
                  href = "https://doi.org/10.1038/ncomms4887",
                  shiny::tags$span("(R. Akbani et al.)."), target = "_blank"
                )
              ),

              shiny::tags$dt(
                shiny::tags$p("Gene with"),
                shiny::tags$p("Pathway activity")
              ),
              shiny::tags$dd(
                "Gene expression was divided into 2 groups(groupHigh and groupLow) by median expression, the difference of pathway activity score(PAS) between groups is defined by student T test, p value was adjusted by FDR, FDR<=0.05 is considered as significant. When PAS(Gene A groupHigh) > PAS(Gene A groupLow), we consider gene A may have a activate effect to a pathway, otherwise have a inhibit effect to a pathway. A similar method has been applied in ",
                shiny::tags$a(href = "https://www.ncbi.nlm.nih.gov/pubmed/29525205", "Y. Ye et al.")
              )
            )
          ),

          shiny::div(
            class = "bs-callout bs-callout-danger",
            shiny::tags$h3("Result"),
            # overall description----
            shiny::tags$dl(
              class = "dl-horizontal",
              shiny::tags$dt("Global percentage"),

              shiny::tags$dd(
                "Global percentage of cancers in which a gene have effect on the pathway among 32 cancers types, shows percentage(number of activate or inhibit cancer types/32 *100%)."
              ),

              shiny::tags$dt("Hetmap percentage"),
              shiny::tags$dd(
                "Heatmap show you genes that have function (inhibit or activate) in at least 5 cancer types. Pathway_a (red) represent percentage of cancers in which pathway may activated by given genes, inhibition in a similar way showed as pathway_i (blue)."
              ),

              shiny::tags$dt("Relation network"),
              shiny::tags$dd(
                "This network show you the relationship between genes and pathways by a line connection. Solid line means activation, dashed lines means inhibition. Color of line represent different cancer types."
              )
              )
            )
          )
        )
      )
    ) # all description end
  )
)