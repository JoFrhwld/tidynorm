make_param_col_names <- function(target_pos, data, norm){

  target_names <- names(target_pos)

  list(
    target_pos = target_pos,
    target_names = target_names,

    L_names = glue::glue(
      "{formant}_L",
      formant = target_names
    ) |>
      rlang::set_names(target_names),

    S_names = glue::glue(
      "{formant}_S",
      formant = target_names
    ) |>
      rlang::set_names(target_names),

    norm_names = glue::glue(
      norm,
      .col = target_names
    ) |>
      rlang::set_names(target_names)
  )

}


make_L <- function(.data, grouping, targets, fn){
  .data <- dplyr::mutate(
    .data,
    .by = {{grouping}},
    dplyr::across(
      {{targets}},
      .fns = fn,
      .names = "{.col}_L"
    )
  )
}

make_S <- function(.data, grouping, targets, fn){
  .data <- dplyr::mutate(
    .data,
    .by = {{grouping}},
    dplyr::across(
      {{targets}},
      .fns = fn,
      .names = "{.col}_S"
    )
  )
}
