
# Load library ------------------------------------------------------------
library(magrittr)



# Load data ---------------------------------------------------------------


expr <- readr::read_rds(path = "/data/GSCALite/TCGA/expr/pancan33_expr_filtered.rds.gz")

clinical <- readr::read_rds(path = "/data/TCGA/TCGA_data/pancan34_clinical.rds.gz")
