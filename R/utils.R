norm_messages <- list(
  .step = "{ .step}",
  .norm_procedure = "normalized with {.fn { .norm_procedure}}",
  .targets = "normalized {.var { .targets}}",
  .f3  = "{.var { .f3}} used for third formant.",
  .norm_cols = "normalized values in {.var { .norm_cols}}",
  .token_id_col = "token id column: {.var { .token_id_col}}",
  .time_col = "time column: {.var { .time_col}}",
  .param_col = "DCT parameter column: {.var { .param_col}}",
  .by = "grouped by {.var { .by}}",
  .by_formant = "within formant: { .by_formant}",
  .by_token = "within token: { .by_token}",
  .norm = "{ .norm}"
)

number_names <- c(
  one = 1,
  two = 2,
  three = 3,
  four = 4,
  five = 5,
  first = 1,
  second = 2,
  third = 3,
  fourth = 4,
  fifth = 5
)

check_grouping <- function(
    .data,
    .by,
    call = caller_env()) {
  grouped_by <- dplyr::group_vars(.data)
  grouping <- try_fetch(
    tidyselect::eval_select(enquo(.by), .data),
    error = \(cnd) selection_errors(cnd, arg = ".by", call = call)
  )
  if (length(grouped_by) > 0 & length(grouping) > 0) {
    cli_abort(
      c(
        "You cannot provide {.arg .by} to a grouped data frame.",
        "i" = "{.fn dplyr::group_by} was used on {.var .data}, and it is still grouped.",
        "i" = "You may want to {.fn dplyr::ungroup} {.var .data} before normalizing."
      ),
      call = call
    )
  }

  if (length(grouped_by) < 1 & length(grouping) < 1 & options::opt("tidynorm.warnings")) {
    cli_par()
    cli_warn(
      c(
        "There is was no grouping provided.",
        "i" = "You may want to provide {.arg .by} with a speaker id column."
      ),
      call = call
    )
    cli_end()
  }
}

check_tokens <- function(
    tokens,
    call = caller_env()) {
  if (length(tokens) < 1) {
    cli_abort(
      c(
        "No column passed to {.arg .token_id_col}.",
        "i" = "Provide column name(s) that uniquely identify tokens."
      )
    )
  }
}

check_args <- function(
    args,
    fmls,
    call = caller_env()) {
  fmls_undot <- stringr::str_remove(
    fmls,
    "^\\."
  )
  args <- args[nzchar(args)]
  if (any(!args %in% fmls)) {
    offenders <- args[!args %in% fmls]
    indef <- ""
    message <- c(
      "{.arg {offenders}} {?is/are} not{? a / }valid argument{?s}"
    )
    if (any(args %in% fmls_undot)) {
      undotted <- args[args %in% fmls_undot]
      redotted <- stringr::str_c(".", undotted)
      message <- c(
        message,
        "i" = "Should {.arg {undotted}} be {.arg {redotted}}?"
      )
    }
    cli_abort(
      message = message,
      call = call
    )
  }
}

selection_errors <- function(
    cnd,
    arg = "",
    call = caller_env()) {
  if (cnd_inherits(cnd, "vctrs_error_subscript_oob")) {
    cli_abort(
      "Problem with column selection for {.arg {arg}}",
      parent = cnd,
      call = call
    )
  } else if (cnd_inherits(cnd, "vctrs_error_subscript")) {
    cli_abort(
      c(
        "Problem with column selection for {.arg {arg}}",
        "i" = "Most arguments need to start with a .",
        "i" = "{.arg .{arg}}"
      ),
      parent = cnd,
      call = call
    )
  } else {
    cli_abort(
      "Problem with column selection for {.arg {arg}}",
      parent = cnd,
      call = call
    )
  }
}

make_dct_grouping <- function(
    .data,
    .by,
    .token_id_col) {
  cols <- enquos(
    .by = .by,
    .token_id_col = .token_id_col
  )
  if (length(dplyr::group_vars(.data)) > 0) {
    by_grouping <- expr(NULL)
    .data <- dplyr::group_by(
      .data,
      {{ .token_id_col }},
      .add = TRUE
    )
    joining <- dplyr::group_vars(.data)
  } else {
    by_grouping <- expr(c({{ .by }}, {{ .token_id_col }}))
    joining <- c()
    for (col in cols) {
      joining <- c(
        joining,
        names(
          tidyselect::eval_select(col, .data)
        )
      )
    }
  }

  return(
    list(
      .data = .data,
      by_grouping = by_grouping,
      joining = joining
    )
  )
}

append_norm_info <- function(
    .data,
    info) {
  prev_attr <- attributes(.data)$norminfo
  attr(.data, "norminfo") <- c(
    prev_attr,
    list(
      info
    )
  )

  .data
}

update_norm_info <- function(
    .data,
    info) {
  norminfo <- attr(.data, "norminfo")
  last_norm <- norminfo[[length(norminfo)]]

  last_names <- names(last_norm)
  new_names <- names(info)

  for (n in new_names) {
    last_norm[[n]] <- info[[n]]
  }

  norminfo[[length(norminfo)]] <- last_norm
  attr(.data, "norminfo") <- norminfo

  .data
}

wrap_up <- function(.data, .silent = FALSE) {

  if(.silent) return()


  if (is.null(attr(.data, "norminfo"))) {
    cli_par()
    cli_inform(
      "Not normalized with a {.pkg tidynorm} procedure."
    )
    return()
    cli_end()
  }

  norminfo <- attr(.data, "norminfo")
  last_norm <- norminfo[[length(norminfo)]]

  message <- c("Normalization info")

  for (n in names(norm_messages)) {
    if (n %in% names(last_norm)) {
      message <- c(
        message,
        "*" = norm_messages[[n]]
      )
    }
  }

  cli_par()
  cli_inform(
    message,
    .envir = last_norm
  )
  cli_end()
}

#' Check Normalization Procedures
#'
#' `check_norm()` will generate a message with information
#' about which normalization procedures have been applied to the
#' data.
#'
#' @param .data A data frame produced by a tidynorm function.
#'
#' @returns
#' This only prints an info message.
#'
#' @examples
#' speaker_norm <- speaker_data |>
#'   norm_nearey(
#'     F1:F3,
#'     .by = speaker,
#'     .silent = TRUE
#'   )
#'
#' check_norm(speaker_norm)
#'
#' @export
check_norm <- function(.data) {
  if (is.null(attr(.data, "norminfo"))) {
    cli_par()
    cli_inform(
      "x" = "Not normalized with a {.pkg tidynorm} procedure."
    )
    return()
    cli_end()

    return()
  }

  norminfo <- attr(.data, "norminfo")

  for (step in norminfo) {
    message <- "Normalization Step"
    for (n in names(norm_messages)) {
      if (n %in% names(step)) {
        message <- c(
          message,
          "*" = norm_messages[[n]]
        )
      }
    }
    cli_par()
    cli_inform(message, .envir = step)
    cli_end()
  }
}

#' Convert text-based formant names to
#' numeric
#' @noRd
name_to_formant_num <- function(.formant_name, call = caller_env()) {
  checkmate::check_character(.formant_name, any.missing = FALSE, min.len = 1)

  has_digit <- stringr::str_detect(
     .formant_name, r"{[fF][1-5]}"
     ) |>
       all()

  if (has_digit) {
    nums = stringr::str_extract(
      .formant_name,
      r"{[fF]([1-5])}",
      group = 1
    ) |>
      as.numeric()
    return(nums)
  }

  has_name <- stringr::str_detect(
    .formant_name,
    stringr::regex(
      stringr::str_flatten(names(number_names), collapse = "|"),
      ignore_case = TRUE
    )
  ) |>
    all()

  if (has_name) {
    digit_name <- stringr::str_extract(
      .formant_name,
      stringr::regex(
        stringr::str_flatten(names(number_names), collapse = "|"),
        ignore_case = TRUE
      )
    ) |>
      tolower()
    nums = number_names[digit_name] |>
      unname()

    return(nums)
  }

  as.numeric(as.factor(.formant_name))

}
