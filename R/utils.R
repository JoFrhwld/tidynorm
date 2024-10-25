check_grouping <- function(
    .data,
    .by,
    call = caller_env()
){
  grouped_by <- dplyr::group_vars(.data)
  grouping <- try_fetch(
    tidyselect::eval_select(.by, .data),
    error = \(cnd) selection_errors(cnd, call = call)
  )
  if(length(grouped_by) > 0 & length(grouping) > 0){
    cli_abort(
      c(
        "You cannot provide {.arg .by} to a grouped data frame.",
        "i" = "{.fn dplyr::group_by} was used on {.var .data}, and it is still grouped.",
        "i" = "You may want to {.fn dplyr::ungroup} {.var .data} before normalizing."
      ),
      call = call
    )
  }

  if(length(grouped_by) < 1 & length(grouping) < 1){
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
    call = caller_env
){
  if(length(tokens)<1){
    cli_abort(
      c(
        "No column passed to {.arg .token_id_col}.",
        "i" = "Provide column name(s) that uniquely identify tokens."
      )
    )
  }
}

check_n_target <- function(
    target_pos,
    n = 2,
    call = caller_env()
){
  n_target <- length(target_pos)
  if(n_target < n){
    cli_warn(
      c(
        "{n_target} column{?s} w{?as/ere} targeted."
      ),
      call = call
    )
  }
}

selection_errors <- function(
    cnd,
    call = caller_env()
){
  if(cnd_inherits(cnd, "vctrs_error_subscript_oob")){
    cli_abort(
      "Problem with column selection",
      parent = cnd,
      call = call
    )
  } else if(cnd_inherits(cnd, "vctrs_error_subscript")){
    cli_abort(
      c(
        "Problem with column selection",
        "i" = "Most arguments need to start with a .",
        "i" = "e.g. {.arg .by_formant}"
      ),
      parent = cnd,
      call = call
    )
  } else {
    cli_abort(
      "Problem with column selection",
      parent = cnd,
      call = call
    )
  }
}

wrap_up <- function(
    .data,
    target_pos,
    .by,
    .by_formant,
    .names
){
  message <- c("Normalization info")

  grouping <- tidyselect::eval_select(.by, data = .data)
  target_names <- names(target_pos)
  message <- c(
    message,
    "*" = "normalized {.var {target_names}}"
  )

  norm_names <- glue::glue(.names, .col = target_names)
  message <- c(
    message,
    "*" = "normalized values in {.var {norm_names}}"
  )

  if(length(grouping) > 0){
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
