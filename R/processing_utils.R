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

process_J_calls <- function(dataset){
  dplyr::select(dataset, SEQUENCE_ID, Vgroup, V_CALL, J_CALL, DRF, CDR3_LENGTH)  %>%
    dplyr::add_count(J_CALL, name = "ds_Jtotal")
}

process_D_calls <- function(dataset){
  dataset |>
    dplyr::select(SEQUENCE_ID, Vgroup, V_CALL, D_CALL, DRF, CDR3_LENGTH) |>
    tidyr::drop_na(D_CALL) |>
    dplyr::add_count(D_CALL, name = "ds_Dtotal")
}

process_V_calls <- function(dataset){
  dataset |>
    dplyr::select(V_CALL) |>
    dplyr::distinct() |>
    dplyr::pull(V_CALL)
}

process_V_groups <- function(dataset){
  dataset |>
    dplyr::select(Vgroup) |>
    dplyr::distinct() |>
    dplyr::pull(Vgroup)
}


process_np_lengths <- function(dataset){
  #dplyr::select(dataset, V_CALL, NP1_LENGTH, NP2_LENGTH)
  dplyr::select(dataset, SEQUENCE_ID, Vgroup, V_CALL, DRF, CDR3_LENGTH, NP1_LENGTH, NP2_LENGTH) 
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
  dplyr::select(dataset, SEQUENCE_ID, Vgroup, V_CALL, DRF, CDR3_LENGTH) 
}

# now assuming that in the curated datasets the column name is fixed as "CDR3_IGBLAST_AA"
process_individual_aa_left <- function(dataset) {
  dplyr::select(dataset, SEQUENCE_ID, Vgroup, V_CALL, DRF, CDR3_LENGTH, CDR3_IGBLAST_AA)  %>%
    dplyr::filter(CDR3_LENGTH <= 22 & CDR3_LENGTH >=9) |>
    tidyr::separate(CDR3_IGBLAST_AA, sep = 1:21, into = as.character(1:22), fill = "right", remove = FALSE) |>
    tidyr::pivot_longer(-(SEQUENCE_ID:CDR3_IGBLAST_AA), names_to = "pos") |>
    dplyr::filter(value != "") |>
    dplyr::mutate(pos = forcats::as_factor(pos)) |>
    dplyr::add_count(pos, name="ds_pos_total")
}


# this is the onoe that we're actually using
# process_individual_aa_left <- function(dataset, cdr3_col = "CDR3_IGBLAST_AA") {
#   dataset |>
#     dplyr::select(V_CALL, {cdr3_col}) |>
#     dplyr::rename(AA = {cdr3_col}) |>
#     dplyr::mutate(n_aa = nchar(AA)) |>
#     dplyr::relocate(n_aa, .before=AA) |>
#     dplyr::filter(nchar(AA) <= 22 & nchar(AA) >=9) |>
#     tidyr::separate(AA, sep = 1:21, into = as.character(1:22), fill = "right", remove = FALSE) |>
#     tidyr::pivot_longer(-(V_CALL:AA), names_to = "pos") |>
#     dplyr::filter(value != "") |>
#     dplyr::mutate(pos = forcats::as_factor(pos)) |>
#     dplyr::add_count(V_CALL, pos, value, name = "aa_count") |>
#     dplyr::add_count(V_CALL, pos, name = "pos_total") |>
#     dplyr::add_count(pos, name="ds_pos_total") |>
#     dplyr::mutate(aa_percent = (aa_count/pos_total)*100) |>
#     dplyr::mutate(aa_ds_percent = (aa_count/ds_pos_total)*100) |>
#     dplyr::select(-AA, -n_aa, -pos_total, -ds_pos_total) |>
#     dplyr::distinct()
# }


parsing_wrapper <- function(dataset, dataset_name){
  dataset <- add_Vgroup(dataset)
  J <- process_J_calls(dataset)
  D <- process_D_calls(dataset)
  V <- process_V_calls(dataset)
  Vgp <- process_V_groups(dataset)
  np <- process_np_lengths(dataset)
  aa <- process_aa_lengths(dataset)
  aa_count_left <- process_individual_aa_left(dataset)
  VCall$new(
    dataset_name, 
    J_calls=J, 
    D_calls=D, 
    V_calls=V,
    V_groups = Vgp,
    np_lengths = np,
    aa_lengths = aa, 
    aa_counts_left = aa_count_left)
}

# this was in the R6 class but wasn't working so I moved it out. It's very much tied to 
# the VCall R6 class though, so I'm not sure if it should just be in there, but with slightly different syntax to actually make it work. 

# get_DJcalls = function(vcall_obj, call_type = "J_calls", v_call = NULL, vgroup = NULL, drf = NULL, CDR3_length = NULL) {
#  # browser()
#   # First filter by vcall or vgroup if supplied
#   if (!is.null(v_call)){
#     if(v_call %in% vcall_obj$J_calls$V_CALL) {
#       filt <- dplyr::filter(vcall_obj[[call_type]], V_CALL==v_call)
#     } else {
#       warning("Couldn't find specified v_call")
#       return(NULL)  
#     }
#   } else if (!is.null(vgroup)){
#     if(vgroup %in% vcall_obj[[call_type]]$Vgroup) {
#       filt <- dplyr::filter(vcall_obj[[call_type]], Vgroup==vgroup)
#     } else {
#       warning("Couldn't find specified vgroup")
#       return(NULL)  
#     }
#   } else {
#     filt <- vcall_obj[[call_type]]
#     #browser()
#   }
#   # Second - filter by DRF
#   if(!is.null(drf)){
#     filt <- dplyr::filter(filt, DRF==drf) %>%
#       tidyr::drop_na(DRF)
#   } else {
#     # remove the drf column and 
#   } 
#   if(!is.null(CDR3_length)){
#     filt <- dplyr::filter(filt, CDR3_LENGTH==CDR3_length) %>%
#       tidyr::drop_na(CDR3_LENGTH)
#   }  
#   filt
# }



