setwd("C:/Users/bharath.kancharla/Desktop/UPC_to_Theme_V2")
library(stringr)
library(data.table)
setDTthreads(0)

DATA <- c("Data")

files <- list.files(path = DATA, "*.csv")

for (file in files){
  file_name <- str_split(file, pattern = "\\.")[[1]][1]
  temp_data <- fread(file.path(DATA, file))
  saveRDS(temp_data, file.path(DATA, str_glue("{file_name}.rds")))
}
