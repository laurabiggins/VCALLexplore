#library(tidyverse)
#library(R6)
source("R/processing_utils.R")
source("R/VCall.R")

# ds2 <- readr::read_tsv("data-raw/umtboth.txt")
# my_ds <- parsing_wrapper(ds2, "umtboth")
# saveRDS(my_ds, file = "data/umtboth.rds")
# 
# ds1 <- readr::read_tsv("data-raw/ydall.txt")
# my_ds1 <- parsing_wrapper(ds1, "ydall")
# saveRDS(my_ds1, file = "data/ydall.rds")
# 
# ds <- readr::read_csv("D:/temp/ybcboth.csv")
# my_ds <- parsing_wrapper(ds, "ybcboth")
# saveRDS(my_ds, file = "data/ybcboth.rds")

ds <- readr::read_csv("data-raw/a2bcprod.csv")
my_ds <- parsing_wrapper(ds, "a2bcprod")
saveRDS(my_ds, file = "data/a2bcprod.rds")

ds <- readr::read_csv("data-raw/a2dprod.csv")
my_ds <- parsing_wrapper(ds, "a2dprod")
saveRDS(my_ds, file = "data/a2dprod.rds")
