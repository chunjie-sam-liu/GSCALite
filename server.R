# save as "server.R"
# shiny server


# Load library ------------------------------------------------------------

library(ggplot2)
library(shiny)
library(shinyjs)

library(magrittr)
# library(maftools)

# Options -----------------------------------------------------------------

options(shiny.reactlog = FALSE) 
options(shiny.sanitize.errors = FALSE)


# Load configuration ------------------------------------------------------

source(file = "config.R", local = TRUE) 


# Load server functions ---------------------------------------------------

source(file = file.path(config$server, "functions_server.R"))


# Shiny session Start -----------------------------------------------------

shinyServer(
  func = function(input, output, session){
    # Init session ----
    source(file = file.path(config$server, "init_server.R"), local = TRUE)
    
    # Load database -----------------------------------------------------------
    
    source(file = file.path(config$server, "load_data.R"), local = TRUE)
    
    ### Input Modules
    
    # Welcome ----
    source(file = file.path(config$server, "welcome_server.R"), local = TRUE)
    
    # tcga expr
    # source(file = file.path(config$server, "tcga_expr_server.R"), local = TRUE)
    
    # tcga cnv ----

    source(file = file.path(config$server, "tcga_cnv_server.R"), local = TRUE)
    
    # tcga snv ----
    # source(file = file.path(config$server, "tcga_snv_server.R"), local = TRUE)
    
    # tcga meth ----
    # source(file = file.path(config$server, "tcga_meth_server.R"), local = TRUE)
    
    # tcga mirna ----
    source(file = file.path(config$server, "tcga_mirna_server.R"), local = TRUE)
    
    # tcga rppa ----
    # source(file = file.path(config$server, "tcga_rppa_server.R"), local = TRUE)
    
    # drug ----
    # gdsc
    # source(file = file.path(config$server, "tcga_gdsc_server.R"), local = TRUE)
    # ctrp
    # source(file = file.path(config$server, "tcga_ctrp_server.R"), local = TRUE)
    
    # Load data
    
  }
)


