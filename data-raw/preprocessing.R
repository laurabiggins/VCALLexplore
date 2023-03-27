library(tidyverse)
library(R6)
source("R/processing_utils.R")
source("R/VCall.R")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/BCcurated.csv"), "BC")
saveRDS(my_ds, file = "data/BC.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/T1curated.csv"), "T1")
saveRDS(my_ds, file = "data/T1.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/Dcurated.csv"), "D")
saveRDS(my_ds, file = "data/D.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/FOcurated.csv"), "FO")
saveRDS(my_ds, file = "data/FO.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/MZcurated.csv"), "MZ")
saveRDS(my_ds, file = "data/MZ.rds")







