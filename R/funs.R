#' identity norm fun
#' @export
#' @keywords internal
identity_norm_fun <- function(
    .data,
    .by,
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
#' @keywords internal
lobanov_norm_fun <- function(
    .data,
    .by,
    .names
){
  .data <- dplyr::mutate(
    .data,
    .by = {{.by}},
    L = base::mean(!!sym(".col"), na.rm = T),
    S = stats::sd(!!sym(".col"), na.rm = T),
    dplyr::across(
      !!sym(".col"),
      .fns = \(x) (x - !!sym("L"))/!!sym("S"),
      .names = .names
    )
  )

  return(.data)
}

#' Neary norm fun
#' @export
#' @keywords internal
nearey_norm_fun <- function(
    .data,
    .by,
    .names
){
  .data <- dplyr::mutate(
    .data,
    .by = {{.by}},
    L = mean(log(!!sym(".col")), na.rm = T),
    S = 1,
    dplyr::across(
      .col,
      .fns = \(x) log(x) - !!sym("L"),
      .names = .names
    )
  )
}

#' deltaF norm fun
#' @export
#' @keywords internal
deltaF_norm_fun <- function(
    .data,
    .by,
    .names
){

  .data <- dplyr::mutate(
    .data,
    .by = {{.by}},
    .formant_num = stringr::str_extract(
      !!sym(".formant"),
      r"{[fF](\d)}",
      group = 1
    ) |> as.numeric(),
    L = 0,
    S = mean(
      !!sym(".col")/(!!sym(".formant_num") - 0.5),
      na.rm = T
    ),
    dplyr::across(
      !!sym(".col"),
      .fns = \(x) x/!!sym("S"),
      .names = .names
    )
  ) |>
    dplyr::select(
      -!!sym(".formant_num")
    )

  return(.data)
}
