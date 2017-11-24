tabItem(tabName = "tcga_snv", align = "center",
        shinyjs::useShinyjs(),
        
        ## SNV message --------------------------------------------
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>SNV
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Single Nucleotide Mutation</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>Single Nucleotide Mutation(SNV) is a variation in a single nucleotide that occurs at a specific position in the genome. 
The TCGA data is used to give you a visualization about SNV of you gene set for seleted cancer types.
                <br>GSAC offers different types of graphic layout
(heatmap, oncoplot, lollipop, survival, and mutation load, see details in <code>help page</code> below.) 
for you to visualize the SNV of your gene set for your seleted cancer types.</p>
                </div>
                </div>
                </div>
                </div>")
        ),
        
        # HELP as including of tcga_snv_help.R ---------------------
        source(file = file.path(config$wd, "ui", "tcga_snv_help.R"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="100%")
        
)