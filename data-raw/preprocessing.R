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
my_ds$aa_lengths
my_ds$np_lengths
my_ds$aa_counts





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


# aa_counts1 <- summary1 %>%
#   group_by(V_CALL, pos) %>%
#   count(value) %>%
#   ungroup() 