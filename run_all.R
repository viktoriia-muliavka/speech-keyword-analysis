# run_all.R

install.packages("renv")
renv::init()
renv::snapshot()

source("R/01_data_import_and_keyword_search.R") 
source("R/02_get_manifestos.R")
source("R/03_analysis.R")

