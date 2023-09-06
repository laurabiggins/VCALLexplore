library(tidyverse)
library(R6)
source("R/processing_utils.R")
source("R/VCall.R")

file_names <- list.files("D:/temp/Harry_White/")
ds_names <- gsub(file_names, pattern = ".csv", replacement = "", fixed = TRUE)

for(i in 1:length(file_names)){
  file_name <- file_names[i]
  ds_name <- ds_names[i]
  my_ds <- parsing_wrapper(read_delim(paste0("D:/temp/Harry_White/",file_name)), ds_name)
  saveRDS(my_ds, file = paste0("data/", ds_name, ".rds"))
}
# 
# my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/T1curated.csv"), "T1")
# saveRDS(my_ds, file = "data/T1.rds")












