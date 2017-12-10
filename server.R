# save as "server.R"
# shiny server


# Load library ------------------------------------------------------------

library(ggplot2)
library(shiny)
library(shinyjs)

library(magrittr)
library(maftools)

# Options -----------------------------------------------------------------

options(shiny.reactlog = FALSE) 
options(shiny.sanitize.errors = FALSE)


# Load configuration ------------------------------------------------------

source(file = "config.R", local = TRUE) 


# Load server functions ---------------------------------------------------

source(file = file.path(config$server, "functions_server.R"))


# Load database -----------------------------------------------------------



# Shiny session Start -----------------------------------------------------

shinyServer(
  func = function(input, output, session){
    # Init session ----
    source(file = file.path(config$server, "init_server.R"), local = TRUE)
    
    ### Input Modules
    
    # 
    
    
    # Welcome ----
    source(file = file.path(config$server, "welcome_server.R"), local = TRUE)
    
    # tcga cnv ----
    source(file = file.path(config$server, "tcga_cnv_server.R"), local = TRUE)
  }
)


