check_grouping <- function(
    .data,
    .by,
    call = caller_env()
){
  grouped_by <- dplyr::group_vars(.data)
  grouping <- tidyselect::eval_select(.by, .data)
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
    cli_warn(
      c(
        "There is was no grouping provided.",
        "i" = "You may want to provide {.arg .by} with a speaker id column."
      ),
      call = call
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

wrap_up <- function(
    .data,
    target_pos,
    .by,
    .by_formant
){
  grouping <- tidyselect::eval_select(.by, data = .data)
  message <-    c(
    "*" = "normalized {.var {names(target_pos)}}"
  )
  if(length(grouping) > 0){
    message <- c(
      message,
      "*" = "grouped by {.var {names(grouping)}}"
    )
  }

  if(.by_formant){
    message <- c(
      message,
      "*" = "formant intrinsic"
    )
  }else{
    message <- c(
      message,
      "*" = "formant extrinsic"
    )

  }

  cli_inform(
   message
  )
}
