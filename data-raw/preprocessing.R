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


# x <- read_delim("D:/temp/Harry_White/BCcurated.csv")
# y <- read_delim("D:/temp/Harry_White/FOcurated.csv")

# add a column with V group
# 
# my_ds <- read_delim("D:/temp/Harry_White/BCcurated.csv") %>%
#   add_Vgroup() %>%
#   parsing_wrapper("BC")
 

# my_ds$get_Jcalls(vgroup = "IGHV13", drf = 2, CDR3_length = 12)
# my_ds$J_calls
# z <- my_ds$get_Jcalls()

my_ds$get_Jcalls(vgroup = "IGHV13", drf = NULL, CDR3_length = 12) %>%
  dplyr::add_count(J_CALL) %>%
  mutate(Jpercent_perDS = (n/ds_Jtotal)*100) %>%
  select(J_CALL, n, Jpercent_perDS) %>%
  distinct()
  
# I think we need to keep in the ID column so that we don't lose the count info

library(tidyverse)
library(R6)
source("R/processing_utils.R")
source("R/VCall.R")

#J <- process_J_calls(x)
my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/BCcurated.csv"), "BC")
saveRDS(my_ds, file = "data/BC.rds")
my_ds <- parsing_wrapper(read_delim("D:/temp/Harry_White/T1curated.csv"), "T1")
saveRDS(my_ds, file = "data/T1.rds")



x <- read_delim("D:/temp/Harry_White/BCcurated.csv") %>%
  add_Vgroup()

x1 <- process_individual_aa_left(x)




# this is the onoe that we're actually using
process_individual_aa_left <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA") {
  dataset |>
    dplyr::select(V_CALL, {cdr3_col}) |>
    dplyr::rename(AA = {cdr3_col}) |>
    dplyr::mutate(n_aa = nchar(AA)) |>
    dplyr::relocate(n_aa, .before=AA) |>
    dplyr::filter(nchar(AA) <= 22 & nchar(AA) >=9) |>
    tidyr::separate(AA, sep = 1:21, into = as.character(1:22), fill = "right", remove = FALSE) |>
    tidyr::pivot_longer(-(V_CALL:AA), names_to = "pos") |>
    dplyr::filter(value != "") |>
    dplyr::mutate(pos = forcats::as_factor(pos)) |>
    dplyr::add_count(V_CALL, pos, value, name = "aa_count") |>
    dplyr::add_count(V_CALL, pos, name = "pos_total") |>
    dplyr::add_count(pos, name="ds_pos_total") |>
    dplyr::mutate(aa_percent = (aa_count/pos_total)*100) |>
    dplyr::mutate(aa_ds_percent = (aa_count/ds_pos_total)*100) |>
    dplyr::select(-AA, -n_aa, -pos_total, -ds_pos_total) |>
    dplyr::distinct()
}





