
assert_valid_field <- function(name, type, field, suggest = NULL){

  msg = c(
    glue::glue(
      "Invalid specification of `{field}` for variable `{name}`"
    ),
    "x" = glue::glue(
      "`{field}` cannot be specified for {type} variables"
    )
  )

  if(!is.null(suggest)){
    msg %<>% c(
      "i" = glue::glue(
        "`{field}` may be specified for {.paste_collapse(suggest)} variables."
      )
    )
  }

  rlang::abort(message = msg, call = NULL)

}

assert_in_set <- function(values, choices,
                          value_type = "values",
                          variable = NULL){

  unmatched_values <- setdiff(values, choices)

  if(!is_empty(unmatched_values)){

    msg = c(
      glue("Invalid {value_type}:"),
      if(!is.null(variable)) c("i" = glue("Variable: `{variable}`")),
      "x" = glue("Unrecognized input values: \\
                 {.paste_collapse(unmatched_values)}"),
      "i" = glue::glue("Recognized values in dictionary: \\
                       {.paste_collapse(choices)}")
    )

    rlang::abort(message = msg, call = NULL)

  }

}

assert_valid_dotdot <- function(..., .list){
  if(!is_empty(list(...)) && !is_empty(.list)){
    rlang::abort(
      message = c(
        "`...` must be empty if `.list` is not `NULL`",
        i = glue::glue("This function can work with either \\
                       unquoted inputs or a list of inputs, \\
                       but it is not intended to be used with both \\
                       at once.")
      )
    )
  }
  if(is_empty(list(...)) && is_empty(.list)){
    rlang::warn(
      message = c(
        i = glue::glue("`...` is empty and so is `.list`. No action can \\
        be taken when both of these inputs are unspecified.")
      )
    )
  }
}
