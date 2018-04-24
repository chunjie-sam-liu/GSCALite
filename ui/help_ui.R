# sourced by 'ui.R'
# save as 'help_ui.R'
# ui elements for help


tabItem(
  tabName = "help",
  
  # help information ----
  fluidRow(
    style = "width:100%;",
    
    column(
      width = 12, offset = 0,
      shiny::tags$h1("Simple usage for GSCALite", align = "center"),
      shiny::tags$br(),
      shiny::tags$video(src = "./videos/guide_video.mp4", controls = "controls", width = "100%", height = "100%"),
      shiny::tags$br(),
      shiny::tags$img(
        src = "./imgs/help_main.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ), 
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::tags$h1("Results presentation", align = "center"),      
      shiny::tags$h1("1: TCGA anaysis result in GSCALite"),
      shiny::tags$h2("1.1: Gene Set Expression analysis result"),
      shiny::tags$img(
        src = "./imgs/1.mrna_expression/tumorVSnormal.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("This picture here displayed the expression ratio of gene set in the tumor-VS-normal compairson. 
                    The dot denotes FDR from multiple t-test.", align="center"),
      shiny::tags$img(
        src = "./imgs/1.mrna_expression/FC-table.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("The table above indicates the detailed information for the gene set queried. 
                    User could click the arrow besides column names to change the rank. 
                    The boxes under column names help user to search target results.",align="center"),
      shiny::tags$h2("1.2: SNV analysis results"),
      shiny::tags$h3("1.2.1: SNV percentage profile"),
      shiny::tags$img(
        src = "./imgs/2.snv/percent.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("The number in the main picture denotes the mututaion frequency of the gene in a specific cancer type.
                    each row represents a gene.",align="center"),
      shiny::tags$h3("1.2.2: SNV summary plot"),
      shiny::tags$img(
        src = "./imgs/2.snv/summary.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("Summary of SNV analysis results. This picture here displays the Variant Type (SNP or DEL), Variant Classification, SNV Class, etc.",align="center"),
      shiny::tags$h3("1.2.3: SNV oncoplot"),
      shiny::tags$img(
        src = "./imgs/2.snv/oncoplot.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("Detailed SNV informations of the gene set in the selected TCGA cancer type.", align="center"),
      shiny::tags$h3("1.2.4: SNV survival plot"),
      shiny::tags$img(
        src = "./imgs/2.snv/survival.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("Kaplan-Meier Survival Estimates for samples with SNV or not in selected cancer types. 
                    The null hypothesis is that the SNV did not change survival. 
                    Here, the dot with blue color indicates patientes with SNV in the correspond gene have worse survival in TCGA dataset.",align="center"),
      shiny::tags$h2("1.3: CNV analysis results"),
      shiny::tags$h3("1.3.1: CNV results in a nut"),
      shiny::tags$img(
        src = "./imgs/3.cnv/pie.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$img(
        src = "./imgs/3.cnv/bar.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("The pie and bar plots give a survey for CNV categories (heterozygous and homozygous amplification/deletion) of queried gene set in selected cancers."
                    ,align="center"),
      shiny::tags$h3("1.3.2: The distribution of CNV category in selected cancer types"),      
      shiny::tags$img(
        src = "./imgs/3.cnv/heteCNV.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$img(
        src = "./imgs/3.cnv/homoCNV.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("The two plots above demonstrate the somatic CNV information (gene loci locates in the region) in selected cancer types.",align="center"),
      shiny::tags$h3("1.3.3: The relationship between CNV and gene expression"),      
      shiny::tags$img(
        src = "./imgs/3.cnv/corTOexpression.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("This plot demonstrates the relationship between CNV and gene expression. 
                    The negtive value of spearman correlation coefficence indicates the gene has a opposite expression profile with CNV.", align = "center"),
      shiny::tags$h2("1.4: methylation analysis results"),
      shiny::tags$h3("1.4.1: The methylation difference between tumor and normal samples"),      
      shiny::tags$img(
        src = "./imgs/4.methy/diff_meth.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("This picture here displays the methylation difference between tumor and normal samples.
                    The range of methylation value varies from 0 to 1, 0 indicates NON-methy while 1 means totally methylated loci.
                    Here we used the average methylation level to calculated the difference bewteen tumor and normal samples.
                    FDR was calculated after multiple t-test.", align = "center"),
      shiny::tags$h3("1.4.2: The relationship between gene expression and methylation"),      
      shiny::tags$img(
        src = "./imgs/4.methy/meth_exp_relation.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("This plot demonstrates the relationship between gene expression and methylation. 
                    The blue/red dot here indicates the gene expression is negtively/positively related to the methylation.", align = "center"),      
      shiny::tags$h3("1.4.3: The relationship between gene methylation and patient survival"),      
      shiny::tags$img(
        src = "./imgs/4.methy/meth_os.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("This picture displays the relationship between gene expression and patients survival. 
                    The blue/red dot here indicates the high/low methylation level of corresponse gene is related to the wrose survival of patients.", align = "center"),
      shiny::tags$h2("1.5: Pathway activity in cancers"),
      shiny::tags$h3("1.5.1: The global activity of genes in cancer related pathways."),      
      shiny::tags$img(
        src = "./imgs/5.protein/glb_percent.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("This picture here displays the global activity of genes in selected cancer types. 
                    The pie of gene in corresponse pathways means the activity/inhibition/non-significant effect contribute to the pathways in specific cancer.
                    The blank means the gene is not in the pathways.", align = "center"),
      shiny::tags$h3("1.5.2: The detail percentage of genes with activity/inhibition/non-significant profiles in given cancers."),      
      shiny::tags$img(
        src = "./imgs/5.protein/heatmap_percent.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("Red/blue color indicates genes were actived/inhibited in given pathways across TCGA samples of given cancers(percentage, negative number means inhibited).", align = "center"), 
      shiny::tags$h3("1.5.3: The gene profiles in cancer related pathways across cancer types."),      
      shiny::tags$img(
        src = "./imgs/5.protein/network_cancer.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$p("The link between left and middle column indicates the genes is expressed in the cancer type. 
                    And the link between middle and right column shows the gene is activated/inhibited in given cancer type.", align = "center"),
      shiny::tags$h2("1.6: MiRNA-mRNA regulation network"),
      shiny::tags$img(
        src = "./imgs/6.miRNA/D3network.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$img(
        src = "./imgs/6.miRNA/visnetwork.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),      
      shiny::tags$p("The pictures above displays the miRNA-mRNA regulatory network in selected cancer type. ", align = "center"),
      shiny::tags$h2("1.8: Gene profiles in GTEx dataset"),
      shiny::tags$h3("1.8.1: The gene expression profiles in selected tissues."),
      shiny::tags$img(
        src = "./imgs/9.GTEx/GTEx_expr.png",
        class = "center-block img-responsive",
        style = "height: 600px;" ),
      shiny::tags$h3("1.8.2: Table of eQTL related to quried gene set in selected tissues"),
      shiny::tags$img(
        src = "./imgs/9.GTEx/GTEx_eqtl.png",
        class = "center-block img-responsive",
        style = "height: 600px;" )     
      
      )
      ),
  fluidRow(
    shiny::tags$hr(),
    shiny::tags$p(
      "Copyright ©",
      shiny::tags$a("Guo Lab", href = "http://bioinfo.life.hust.edu.cn/home_page#!/", target = "_blank", style = "color:#008176"),
      ",",
      shiny::tags$a("College of Life Science and Technology", href = "http://life.hust.edu.cn/", target = "_blank", style = "color:#008176"),
      ",",
      shiny::tags$a("HUST", href = "http://www.hust.edu.cn/", target = "_blank", style = "color:#008176"),
      ", China"
      ,align = "center"),
    shiny::tags$p(
      shiny::tags$a("Han Lab", href = "https://med.uth.edu/bmb/faculty/leng-han-ph-d/", target = "_blank", style = "color:#008176"),
      ",",
      shiny::tags$a("UTHealth", href = "https://med.uth.edu/", target = "_blank", style = "color:#008176"),
      " Houston, USA"
      ,align = "center"),
    shiny::tags$p("Any comments and suggestions, please contact us.",align = "center")
  )
  
  
  # Load footer ----
  
      ) # End of tabItem
