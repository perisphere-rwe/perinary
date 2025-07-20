
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

