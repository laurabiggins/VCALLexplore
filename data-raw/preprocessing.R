library(tidyverse)
library(R6)
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
# remove the *01 etc from the VCALL name
my_ds <- ds %>%
  separate(V_CALL, sep = "\\*", into = c("V_CALL", NA)) 
my_ds <- parsing_wrapper(my_ds, "a2bcprod")
saveRDS(my_ds, file = "data/a2bcprod.rds")

ds <- readr::read_csv("data-raw/a2dprod.csv")
# remove the *01 etc from the VCALL name
my_ds <- ds %>%
  separate(V_CALL, sep = "\\*", into = c("V_CALL", NA))

my_ds <- parsing_wrapper(my_ds, "a2dprod")
saveRDS(my_ds, file = "data/a2dprod.rds")

  
ds <- readr::read_csv("data-raw/Sam_sevAD.csv")
# remove the *01 etc from the VCALL name
my_ds <- ds %>%
  separate(V_CALL, sep = "\\*", into = c("V_CALL", NA))

my_ds <- parsing_wrapper(my_ds, "Sam_sevAD")
saveRDS(my_ds, file = "data/Sam_sevAD.rds")

ds <- readr::read_csv("data-raw/SamYD.csv")
# remove the *01 etc from the VCALL name
my_ds <- ds %>%
  separate(V_CALL, sep = "\\*", into = c("V_CALL", NA))
my_ds <- parsing_wrapper(my_ds, "SamYD")
saveRDS(my_ds, file = "data/SamYD.rds")


