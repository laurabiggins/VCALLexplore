box_wrapper <- function(box_id, box_title, panel_tags, box_width = 6, collapsible = TRUE, collapsed=TRUE) {
  shinydashboard::box(
    id = box_id,
    title = box_title,
    width = box_width, 
    class = "plotbox",
    collapsible = collapsible,
    collapsed = collapsed,
    panel_tags)
}

hex_colours <- c("#f9c74f", "#43aa8b", "#f3722c", "#577590", "#f94144", "#90be6d", "#f8961e", "#4a4e69") 
extra_hex_cols <- c(hex_colours, paste0(hex_colours, "a3"), paste0(hex_colours, "5e"))

colour_palette <- ggplot2::scale_fill_manual(values = extra_hex_cols)

tidy_aa_joined_data <- function(messy_aa_data){
  messy_aa_data |>
    dplyr::mutate(percent_diff_vcall = aa_percent.x-aa_percent.y) |>
    dplyr::mutate(percent_diff_ds = aa_ds_percent.x-aa_ds_percent.y) |>
    dplyr::mutate(fold_change_vcall = dplyr::if_else(aa_percent.x>=aa_percent.y, aa_percent.x/aa_percent.y, -(aa_percent.y/aa_percent.x))) |>
    dplyr::mutate(fold_change_vcall = dplyr::if_else(is.infinite(fold_change_vcall), percent_diff_vcall, fold_change_vcall)) |>
    dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall > 100, 100)) |>
    dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall < -100, -100)) |>
    dplyr::mutate(fold_change_ds = dplyr::if_else(aa_ds_percent.x>=aa_ds_percent.y, aa_ds_percent.x/aa_ds_percent.y, -(aa_ds_percent.y/aa_ds_percent.x))) |>
    dplyr::mutate(fold_change_ds = dplyr::if_else(is.infinite(fold_change_ds), percent_diff_ds, fold_change_ds)) |>
    dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds > 100, 100)) |>
    dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds < -100, -100))
}

