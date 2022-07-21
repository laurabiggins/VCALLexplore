library(tidyverse)
library(R6)

#ds1 <- readr::read_tsv("data-raw/ydall.txt")
ds2 <- readr::read_tsv("data-raw/umtboth.txt")
my_ds <- parsing_wrapper(ds2, "umtboth")
my_ds$get_Jcalls("IGHV1-12")

VCall <- R6Class("VCall", list(
  
  name = NULL,
  J_calls = NA,
  D_calls = NA,
  np_lengths = NA,
  aa_lengths = NA,
  
  initialize = function(name, J_calls=NA, D_calls=NA, np_lengths = NA, aa_lengths = NA) {
    stopifnot(is.character(name), length(name) == 1)

    self$name <- name
    self$J_calls <- J_calls
    self$D_calls <- D_calls
    self$np_lengths <- np_lengths
    self$aa_lengths <- aa_lengths
  },
  
  get_Jcalls = function(v_call) {
    if(v_call %in% self$J_calls$V_CALL) {
      dplyr::filter(self$J_calls, V_CALL==v_call)
    }
  },
  
  get_Dcalls = function(v_call) {
    if(v_call %in% self$D_calls$V_CALL) {
      dplyr::filter(self$D_calls, V_CALL==v_call)
    }
  },
  
  get_np_lengths= function(v_call) {
    if(v_call %in% self$np_lengths$V_CALL) {
      dplyr::filter(self$np_lengths, V_CALL==v_call)
    }
  }
  
))


process_J_calls <- function(dataset){
  dplyr::count(dataset, V_CALL, J_CALL)
}

process_D_calls <- function(dataset){
  dataset |>
    dplyr::select(V_CALL, D_CALL) |>
    tidyr::drop_na(D_CALL) |>
    tidyr::separate(D_CALL, into=c("singleD"), sep = ",", extra = "drop") |>
    dplyr::count(V_CALL, singleD)
}

process_np_lengths <- function(dataset){
  dplyr::select(dataset, V_CALL, NP1_LENGTH, NP2_LENGTH)
}

# tibble that contains a column called CDR3_IGBLAST_AA (or similar) that contains all the aa sequences
process_aa_lengths <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA"){
  dataset |>
    dplyr::select({cdr3_col}) |>
    dplyr::rename(AA = {cdr3_col}) |>
    dplyr::mutate(n_aa = nchar(AA)) |>
    dplyr::count(n_aa)
}


parsing_wrapper <- function(dataset, dataset_name){
  
  J <- process_J_calls(dataset)
  D <- process_D_calls(dataset)
  np <- process_np_lengths(dataset)
  aa <- process_aa_lengths(dataset)
  VCall$new(dataset_name, J_calls=J, D_calls=D, np_lengths = np, aa_lengths = aa)
}










my_J_calls <- ds2 |>
  count(V_CALL, J_CALL)

D_calls <- ds2 |>
  select(V_CALL, D_CALL) |>
  drop_na(D_CALL) |>
  separate(D_CALL, into=c("singleD"), sep = ",", extra = "drop") |>
  count(V_CALL, singleD)

np_lengths <- ds2 |>
  select(V_CALL, NP1_LENGTH, NP2_LENGTH)



library(ggplot2)
J_calls %>%
  filter(V_CALL == "IGHV1-11") %>%
  ggplot(aes(x = J_CALL, y =n)) +
  geom_col()



aa_lengths1 <- summary_aa_lengths(ds1, "ydall")
aa_lengths2 <- process_aa_lengths(ds2)

aa_lengths <- full_join(aa_lengths1, aa_lengths2)

summary1 <- ds1 %>%
  select(V_CALL, CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  relocate(n_aa, .before=AA) %>%
  filter(nchar(AA) <= 22 & nchar(AA) >=9) %>% # we need a maximum or it gets very messy 
  separate(AA, sep = 1:21, into = as.character(1:22), fill = "right", remove = FALSE) %>%
  pivot_longer(-(V_CALL:AA), names_to = "pos") %>%
  filter(value != "") %>%
  mutate(pos = forcats::as_factor(pos)) 

summary2 <- ds2 %>%
  select(V_CALL, CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  relocate(n_aa, .before=AA) %>%
  filter(nchar(AA) <= 22 & nchar(AA) >=9) %>%
  separate(AA, sep = 1:21, into = as.character(1:22), fill = "right", remove = FALSE) %>%
  pivot_longer(-(V_CALL:AA), names_to = "pos") %>%
  filter(value != "") %>%
  mutate(pos = forcats::as_factor(pos)) 


pos_counts1 <- summary1 %>%
  add_count(V_CALL, pos, value, name = "aa_count1") %>%
  add_count(V_CALL, pos, name = "pos_total1") %>%
  mutate(aa_percent1 = (aa_count1/pos_total1)*100) %>%
  select(-AA, -n_aa) %>%
  #select(-AA, -total, -n_aa) %>%
  distinct() # if we don't do this we get loads of repetition due to the n_aa lengths

pos_counts2 <- summary2 %>%
  add_count(V_CALL, pos, value, name = "aa_count2") %>%
  add_count(V_CALL, pos, name = "pos_total2") %>%
  mutate(aa_percent2 = (aa_count2/pos_total2)*100) %>%
  select(-AA, -n_aa) %>%
  #select(-AA, -total, -n_aa) %>%
  distinct()

# We've got some NAs where the aa isn't present at that position in one of the datasets
# pos_counts2 %>%
#   filter(V_CALL=="IGHV1-62-3") %>%
#   filter(pos==9)



# there are lots of different lengths, so we need to remove those from the join or the table size gets ridiculous. If we're going to filter on length, it'll have to be upstream.
# I've removed n_aa from pos_counts
joined <- pos_counts1 %>%
  full_join(pos_counts2, by = c("V_CALL", "pos", "value")) %>%
  #replace(is.na(.), 0)
  mutate(aa_count1 = replace_na(aa_count1, 0)) %>%
  mutate(aa_percent1 = replace_na(aa_percent1, 0)) %>%
  mutate(aa_count2 = replace_na(aa_count2, 0)) %>%
  mutate(aa_percent2 = replace_na(aa_percent2, 0)) %>%
  mutate(percent_diff = aa_percent1-aa_percent2) 

joined <- joined %>%
  mutate(fold_change = dplyr::if_else(aa_percent1>=aa_percent2, aa_percent1/aa_percent2, -(aa_percent2/aa_percent1))) %>%
  mutate(fold_change = dplyr::if_else(is.infinite(fold_change), percent_diff, fold_change)) %>%
  mutate(fold_change = replace(fold_change, fold_change > 100, 100)) %>%
  mutate(fold_change = replace(fold_change, fold_change < -100, -100)) %>%
  mutate(raw_diff = aa_count1-aa_count2)
  
#mydb <- dbConnect(RSQLite::SQLite(), "vcall_db")

# we can have a table of the summary of amino acid lengths, where each column is one of the datasets,
# then separate tables for summaries of each dataset

# then we'll need the joined ones - how efficient is the sql join compared to this dplyr version - I'm guessing similar...

saveRDS(aa_lengths1, "data/aa_lengths1.rds")
saveRDS(aa_lengths2, "data/aa_lengths2.rds")
saveRDS(summary1, "data/summary1.rds")
saveRDS(summary2, "data/summary2.rds")
saveRDS(joined, file="data/joined.rds")



library(plotly)
joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  #dplyr::filter(abs(percent_diff) > 2) %>%
  plotly::plot_ly(x= ~pos, y= ~percent_diff, color= ~value) %>%
  plotly::add_text(
    text = ~value,
    #hovertext = ~name,
    #hoverinfo = "text",
    size = I(20)
  )

joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  #dplyr::filter(abs(percent_diff) > 2) %>%
  plotly::plot_ly(x= ~pos, y= ~fold_change, color= ~value) %>%
  plotly::add_text(
    text = ~value,
    #hovertext = ~name,
    #hoverinfo = "text",
    size = I(20)
  )


joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  group_by(value) %>%
  summarise(
    median_diff = median(abs(percent_diff)),
    mean_diff = mean(abs(percent_diff)),
    sd_diff = sd(percent_diff)
  ) %>%
  #arrange(desc(mean_diff))
  arrange(desc(sd_diff))

#my_colours <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_distinct(joined$value))

joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  dplyr::filter(abs(percent_diff) > 1) %>%
  plotly::plot_ly(x = ~pos, y= ~value, z= ~ percent_diff) %>%
  plotly::add_heatmap() #+
#scale_color_manual(values = my_colours)

joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  filter(pos ==1)


# aa_counts1 <- summary1 %>%
#   group_by(V_CALL, pos) %>%
#   count(value) %>%
#   ungroup() 