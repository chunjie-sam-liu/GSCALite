# source from 'server.R' and 'ui.R'
# save as 'config.R'
# configuration file

# Store variable in config
config <- list()

# Version
config$version <- "1.0.0"

# Working directory -------------------------------------------------------
# This is the absolute path to server.R, ui.R and config.R
# It's the working directory of app
# DEFAULT getwd()



# config$wd <- "/project/huff/huff/github/GSCALite"
config$wd <- "/home/liucj/web/GSCALite"
 # config$wd <- "/home/zhangq/github/GSCALite"



# User directory ----------------------------------------------------------
# This controls the unique directory,
# which is set for every user at the beginning of a session is created.
# usually this is a directory with a hashed name in temporary directory
# But for debugging it might be handy to have it in the working directory
# config$user_dir <- "WD"

# Database ----------------------------------------------------------------
# This contains the TCGA, GTEx and Drug data
config$database <- "/home/liucj/shiny-data/GSCALite"

# Path bins ---------------------------------------------------------------
config$bins <- "bins"

# Path functions ----------------------------------------------------------
config$functions <- "functions"

# Path logs ---------------------------------------------------------------
config$logs <- "logs"

# Path scripts ------------------------------------------------------------
config$scripts <- "scripts"

# Path server -------------------------------------------------------------
config$server <- "server"

# Path ui -----------------------------------------------------------------
config$ui <- "ui"

# Path user data ----------------------------------------------------------
config$userdata <- "userdata"

# Paths -------------------------------------------------------------------
config$bins <- file.path(config$wd, config$bins)
config$functions <- file.path(config$wd, config$functions)
config$logs <- file.path(config$wd, config$logs)
config$scripts <- file.path(config$wd, config$scripts)
config$server <- file.path(config$wd, config$server)
config$ui <- file.path(config$wd, config$ui)
config$userdata <- file.path(config$wd, config$userdata)

# Path to zip -------------------------------------------------------------
Sys.setenv("R_ZIPCMD" = "/usr/bin/zip")
