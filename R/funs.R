#' identity norm fun
#' @export
identity_norm_fun <- function(
    .data,
    grouping,
    .names
){
  .data = dplyr::mutate(
    .data,
    L = 0,
    S = 1
  )
  return(.data)
}

#' lobanov_norm_fun
#' @export
lobanov_norm_fun <- function(
    .data,
    grouping,
    .names
){
  .data <- dplyr::mutate(
    .data,
    .by = !!grouping,
    L = mean(.col, na.rm = T),
    S = sd(.col, na.rm = T),
    dplyr::across(
      .col,
      .fns = \(x) (x - L)/S,
      .names = .names
    )
  )

  return(.data)
}

#' Neary norm fun
#' @export
nearey_norm_fun <- function(
    .data,
    grouping,
    .names
){
  .data <- dplyr::mutate(
    .data,
    .by = !!grouping,
    L = mean(log(.col), na.rm = T),
    S = 1,
    dplyr::across(
      .col,
      .fns = \(x) log(x) - L,
      .names = .names
    )
  )
}

#' deltaF norm fun
#' @export
deltaF_norm_fun <- function(
    .data,
    grouping,
    .names
){

  .data <- dplyr::mutate(
    .data,
    .by = !!grouping,
    .formant_num = stringr::str_extract(
      .formant,
      r"{[fF](\d)}",
      group = 1
    ) |> as.numeric(),
    L = 0,
    S = mean(
      .col/(.formant_num - 0.5),
      na.rm = T
    ),
    dplyr::across(
      .col,
      .fns = \(x) x/S,
      .names = .names
    )
  ) |>
    dplyr::select(
      -.formant_num
    )

  return(.data)
}
