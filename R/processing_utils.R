# takes input tibble with a column called V_CALL containing for e.g. "IGHV1-55" "IGHV1-67" "IGHV3-3", strips any characters after "-" and adds a column called Vgroup containing the stripped names e.g. "IGHV1-55" "IGHV1-67" "IGHV3-3".

add_Vgroup <- function(tbl){
  tbl %>%
    tidyr::separate_wider_delim(
      cols = V_CALL,
      delim = "-",
      names = c("Vgroup", NA),
      cols_remove = FALSE,
      too_many = "drop"
    )
}

# process_J_calls <- function(dataset){
#   dplyr::count(dataset, V_CALL, J_CALL) |> 
#   dplyr::group_by(V_CALL) |>  
#   dplyr::mutate(percent_per_Vgene = (n/sum(n))*100) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(percent_ds = (n/sum(n))*100)
# }


# This is the initial preprocessing. These values will be used in the plots by
# default, unless any filtering for RF or CDR3 length has been specified, in 
# which case we do this on the fly, as part of a VCall object method, and 
# overwrite the "n", "percent_per_Vgene", "percent_per_Vgroup" and "percent_ds" columns. 
# process_J_calls <- function(dataset){
#   dplyr::add_count(dataset, V_CALL, J_CALL) |> 
#     dplyr::group_by(V_CALL) |>  
#     dplyr::mutate(percent_per_Vgene = (n/sum(n))*100) |>
#     dplyr::ungroup() |>
#     dplyr::group_by(Vgroup) |>  
#     dplyr::mutate(percent_per_Vgroup = (n/sum(n))*100) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(percent_ds = (n/sum(n))*100) |>
#     select(Vgroup, V_CALL, J_CALL, DRF, CDR3_LENGTH, n, percent_per_Vgene, percent_per_Vgroup, percent_ds)
# }

process_J_calls <- function(dataset){
  select(dataset, SEQUENCE_ID, Vgroup, V_CALL, J_CALL, DRF, CDR3_LENGTH)  %>%
    dplyr::add_count(J_CALL, name = "ds_Jtotal")
}

process_D_calls <- function(dataset){
  dataset |>
    dplyr::select(V_CALL, D_CALL) |>
    tidyr::drop_na(D_CALL) |>
    tidyr::separate(D_CALL, into=c("singleD"), sep = ",", extra = "drop") |>
    dplyr::count(V_CALL, singleD) |>
    dplyr::group_by(V_CALL) |> 
    dplyr::mutate(percent_per_Vgene = (n/sum(n))*100) |>
    dplyr::ungroup() |>
    dplyr::mutate(percent_ds = (n/sum(n))*100)
}

process_V_calls <- function(dataset){
  dataset |>
    dplyr::select(V_CALL) |>
    dplyr::distinct() |>
    dplyr::pull(V_CALL)
}

process_np_lengths <- function(dataset){
  dplyr::select(dataset, V_CALL, NP1_LENGTH, NP2_LENGTH)
}

# tibble that contains a column called CDR3_IGBLAST_AA (or similar) that contains all the aa sequences
# not using this but keeping it in in case we want the lengths over the whole dataset
process_aa_lengths_all <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA"){
  dataset |>
    dplyr::select({cdr3_col}) |>
    dplyr::rename(AA = {cdr3_col}) |>
    dplyr::mutate(n_aa = nchar(AA)) #|>
    #dplyr::count(n_aa)
}

process_aa_lengths <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA"){
  dataset |>
    dplyr::select(V_CALL, {cdr3_col}) |>
    dplyr::rename(AA = {cdr3_col}) |>
    dplyr::mutate(n_aa = nchar(AA))# |>
   #dplyr::count(V_CALL, n_aa)
}

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


parsing_wrapper <- function(dataset, dataset_name){
  dataset <- add_Vgroup(dataset)
  J <- process_J_calls(dataset)
  D <- process_D_calls(dataset)
  V <- process_V_calls(dataset)
  np <- process_np_lengths(dataset)
  aa <- process_aa_lengths(dataset)
  aa_count_left <- process_individual_aa_left(dataset)
  VCall$new(dataset_name, J_calls=J, D_calls=D, V_calls=V, np_lengths = np, aa_lengths = aa, aa_counts_left = aa_count_left)
}
