getwd()
wd <- getwd()

files <- list.files(wd, "north_korea_missile_test_database.xlsx")
files <- files[2]

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

