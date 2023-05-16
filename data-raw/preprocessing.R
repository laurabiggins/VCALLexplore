library(tidyverse)
library(R6)
source("R/processing_utils.R")
source("R/VCall.R")

# my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/BCcurated.csv"), "BC")
# saveRDS(my_ds, file = "data/BC.rds")
# 
# my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/T1curated.csv"), "T1")
# saveRDS(my_ds, file = "data/T1.rds")
# 
# my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/Dcurated.csv"), "D")
# saveRDS(my_ds, file = "data/D.rds")
# 
# my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/FOcurated.csv"), "FO")
# saveRDS(my_ds, file = "data/FO.rds")
# 
# my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/MZcurated.csv"), "MZ")
# saveRDS(my_ds, file = "data/MZ.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/May23/MZ.csv"), "MZ")
saveRDS(my_ds, file = "data/MZ.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/May23/FO.csv"), "FO")
saveRDS(my_ds, file = "data/FO.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/May23/T1.csv"), "T1")
saveRDS(my_ds, file = "data/T1.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/May23/preB.csv"), "preB")
saveRDS(my_ds, file = "data/preB.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/May23/proB.csv"), "proB")
saveRDS(my_ds, file = "data/proB.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/May23/proBnp.csv"), "proBnp")
saveRDS(my_ds, file = "data/proBnp.rds")

my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/May23/proBoof.csv"), "proBoof")
saveRDS(my_ds, file = "data/proBoof.rds")












