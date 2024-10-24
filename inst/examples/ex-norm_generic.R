library(tidynorm)
library(dplyr)

median_norm_fun <- function(.data, .grouping, .names){
  .data <- .data |>
    mutate(
      .by = !!.grouping,
      L = median(.col, na.rm = TRUE),
      S = mad(.col, na.rm = TRUE),
      across(
        .col,
        .fns = \(x) (x - L)/S,
        .names = .names
      )
    )
  return(.data)
}

speaker_data |>
  norm_generic(
    F1:F3,
    .by = speaker,
    .norm_fun = median_norm_fun,
    .by_formant = TRUE,
    .drop_orig = TRUE,
    .names = "{.col}_mad"
  )

