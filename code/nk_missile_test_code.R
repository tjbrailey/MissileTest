### North Korean Missile Test Temporal Study ###
              ### POLI 170A ###

# Setup
setwd(paste0(getwd(), '/data/'))
wd<-setwd(getwd())

# Install required packages
library(magrittr)
library(ggplot2)

# Install datasets
files <- list.files(wd, "north_korea_missile_test_database.xlsx")
files <- files[]

read_excel_allsheets <- function(filename) { 
  sheets <- readxl::excel_sheets(filename) 
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X)) 
  names(x) <- sheets 
  x 
} 

out <- lapply(files, read_excel_allsheets)
basename(files)

# Create separate tibbles from the nested list
missile_tests <- out[[1]]$`Missile Tests`
data_summary <- out[[1]]$`Data Summary`
missile_summary <- out[[1]]$`Missile Summary`
facilities <- out[[1]]$`Facilities`
rm(out, files, read_excel_allsheets)
