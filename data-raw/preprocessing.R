#library(tidyverse)
#library(R6)

source("R/processing_utils.R")
source("R/VCall.R")

#ds1 <- readr::read_tsv("data-raw/ydall.txt")
ds2 <- readr::read_tsv("data-raw/umtboth.txt")
my_ds <- parsing_wrapper(ds2, "umtboth")
saveRDS(my_ds, file = "data/umtboth.rds")

ds1 <- readr::read_tsv("data-raw/ydall.txt")
my_ds1 <- parsing_wrapper(ds1, "ydall")
saveRDS(my_ds1, file = "data/ydall.rds")

my_ds$get_Jcalls("IGHV1-12")
my_ds$get_aa_counts("IGHV1-12")

my_ds$J_calls
my_ds$D_calls
my_ds$V_calls
my_ds$aa_lengths
my_ds$np_lengths

my_ds$name
my_ds$aa_counts_left
my_ds1$name
my_ds1$aa_counts_left

joined <- full_join(my_ds$aa_counts_left, my_ds1$aa_counts_left, by = c("V_CALL", "pos", "value")) |>
  dplyr::mutate(percent_diff_vcall = aa_ds_percent.x-aa_ds_percent.y) |>
  dplyr::mutate(percent_diff_ds = aa_ds_percent.x-aa_ds_percent.y) |>
  dplyr::mutate(fold_change_vcall = dplyr::if_else(aa_percent.x>=aa_percent.y, aa_percent.x/aa_percent.y, -(aa_percent.y/aa_percent.x))) |>
  dplyr::mutate(fold_change_vcall = dplyr::if_else(is.infinite(fold_change_vcall), percent_diff_vcall, fold_change_vcall)) |>
  dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall > 100, 100)) |>
  dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall < -100, -100)) |>
  dplyr::mutate(fold_change_ds = dplyr::if_else(aa_ds_percent.x>=aa_ds_percent.y, aa_ds_percent.x/aa_ds_percent.y, -(aa_ds_percent.y/aa_ds_percent.x))) |>
  dplyr::mutate(fold_change_ds = dplyr::if_else(is.infinite(fold_change_ds), percent_diff_ds, fold_change_ds)) |>
  dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds > 100, 100)) |>
  dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds < -100, -100))
  #mutate(raw_diff = aa_count1-aa_count2)

joined <- full_join(my_ds$get_aa_counts_left("IGHV10-1"), my_ds1$get_aa_counts_left("IGHV10-1"), by = c("V_CALL", "pos", "value")) |>
  dplyr::mutate(percent_diff_vcall = aa_ds_percent.x-aa_ds_percent.y) |>
  dplyr::mutate(percent_diff_ds = aa_ds_percent.x-aa_ds_percent.y) |>
  dplyr::mutate(fold_change_vcall = dplyr::if_else(aa_percent.x>=aa_percent.y, aa_percent.x/aa_percent.y, -(aa_percent.y/aa_percent.x))) |>
  dplyr::mutate(fold_change_vcall = dplyr::if_else(is.infinite(fold_change_vcall), percent_diff_vcall, fold_change_vcall)) |>
  dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall > 100, 100)) |>
  dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall < -100, -100)) |>
  dplyr::mutate(fold_change_ds = dplyr::if_else(aa_ds_percent.x>=aa_ds_percent.y, aa_ds_percent.x/aa_ds_percent.y, -(aa_ds_percent.y/aa_ds_percent.x))) |>
  dplyr::mutate(fold_change_ds = dplyr::if_else(is.infinite(fold_change_ds), percent_diff_ds, fold_change_ds)) |>
  dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds > 100, 100)) |>
  dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds < -100, -100))



library(plotly)
join1 %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  #dplyr::filter(abs(percent_diff) > 2) %>%
  plotly::plot_ly(x= ~pos, y= ~percent_diff_vcall, color= ~value) %>%
  plotly::add_text(
    text = ~value,
    #hovertext = ~name,
    #hoverinfo = "text",
    size = I(20)
  )


library(ggplot2)
J_calls %>%
  filter(V_CALL == "IGHV1-11") %>%
  ggplot(aes(x = J_CALL, y =n)) +
  geom_col()

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


# old version but need to check what we need for creating the joined file
# 
ds1 <- readr::read_tsv("data-raw/ydall.txt")
ds2 <- readr::read_tsv("data-raw/umtboth.txt") 

process_left_positions <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA"){
  dataset |>
    dplyr::select(V_CALL, {cdr3_col})
}

 

summary1 <- ds1 %>%
  select(V_CALL, CDR3_IGBLAST_AA) %>%
  #select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  relocate(n_aa, .before=AA) %>%
  #filter(nchar(AA) >= 9) %>%
  filter(nchar(AA) <= 22 & nchar(AA) >=9) %>%
  separate(AA, sep = 1:21, into = as.character(1:22), fill = "right", remove = FALSE) %>%
  pivot_longer(-(V_CALL:AA), names_to = "pos") %>%
  filter(value != "") %>%
  #mutate(pos = as.integer(pos)) 
  mutate(pos = forcats::as_factor(pos)) 

summary2 <- ds2 %>%
  select(V_CALL, CDR3_IGBLAST_AA) %>%
  #select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  relocate(n_aa, .before=AA) %>%
  filter(nchar(AA) <= 22 & nchar(AA) >=9) %>%
  separate(AA, sep = 1:21, into = as.character(1:22), fill = "right", remove = FALSE) %>%
  pivot_longer(-(V_CALL:AA), names_to = "pos") %>%
  filter(value != "") %>%
  #mutate(pos = as.integer(pos)) 
  mutate(pos = forcats::as_factor(pos)) 

pos_counts1 <- summary1 %>%
  add_count(V_CALL, pos, value, name = "aa_count1") %>%
  add_count(V_CALL, pos, name = "pos_total1") %>%
  add_count(pos, name="ds_pos_total") %>%
  mutate(aa_percent1 = (aa_count1/pos_total1)*100) %>%
  mutate(ds_aa_percent = (aa_count1/pos_total1)*100) %>%
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
