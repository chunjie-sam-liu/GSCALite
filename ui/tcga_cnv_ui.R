tabItem(tabName = "tcga_cnv", align = "center",
        shinyjs::useShinyjs(),
        
        ## SNV message
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>CNV
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Copy Number variation</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>TCGA copy number variation(CNV) data will be used to give you a visualization of you gene set for seleted cancer types.
                <br>GSAC offers different types of graphic layout for you to visualize the CNV of your gene set for your seleted cancer types.</p>
                </div>
                </div>
                </div>
                </div>")
        )
        
        )