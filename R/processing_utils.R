process_J_calls <- function(dataset){
  dplyr::count(dataset, V_CALL, J_CALL) |> 
  dplyr::group_by(V_CALL) |>  
  dplyr::mutate(percentage = (n/sum(n))*100) |>
  dplyr::ungroup()
}

process_D_calls <- function(dataset){
  dataset |>
    dplyr::select(V_CALL, D_CALL) |>
    tidyr::drop_na(D_CALL) |>
    tidyr::separate(D_CALL, into=c("singleD"), sep = ",", extra = "drop") |>
    dplyr::count(V_CALL, singleD) |>
    dplyr::group_by(V_CALL) |> 
    dplyr::mutate(percentage = (n/sum(n))*100) |>
    dplyr::ungroup()
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
process_aa_lengths <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA"){
  dataset |>
    dplyr::select({cdr3_col}) |>
    dplyr::rename(AA = {cdr3_col}) |>
    dplyr::mutate(n_aa = nchar(AA)) #|>
    #dplyr::count(n_aa)
}

process_individual_aa <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA") {
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
    dplyr::mutate(aa_percent = (aa_count/pos_total)*100) |>
    dplyr::select(-AA, -n_aa) |>
    dplyr::distinct()
}


parsing_wrapper <- function(dataset, dataset_name){
  
  J <- process_J_calls(dataset)
  D <- process_D_calls(dataset)
  V <- process_V_calls(dataset)
  np <- process_np_lengths(dataset)
  aa <- process_aa_lengths(dataset)
  aa_count <- process_individual_aa(dataset)
  VCall$new(dataset_name, J_calls=J, D_calls=D, V_calls=V, np_lengths = np, aa_lengths = aa, aa_counts = aa_count)
}
