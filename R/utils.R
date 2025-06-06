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

  if (length(grouped_by) < 1 & length(grouping) < 1) {
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


wrap_up <- function(
    .data,
    target_pos,
    .by,
    .by_formant,
    .names) {
  message <- c("Normalization info")

  grouping <- tidyselect::eval_select(.by, data = .data)
  target_names <- names(target_pos)
  message <- c(
    message,
    "*" = "normalized {.var {target_names}}"
  )

  norm_names <- glue::glue(.names, .formant = target_names)
  message <- c(
    message,
    "*" = "normalized values in {.var {norm_names}}"
  )

  if (length(grouping) > 0) {
    message <- c(
      message,
      "*" = "grouped by {.var {names(grouping)}}"
    )
  }

  message <- c(
    message,
    "*" = ifelse(
      .by_formant,
      "formant intrinsic",
      "formant extrinsic"
    )
  )

  cli_par()
  cli_inform(
    message
  )
  cli_end()
}
