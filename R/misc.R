

.paste_collapse <- function(x){

  if(is.null(x)) return("none")

  if(length(x) == 1) return(as.character(x))

  last <- if(length(x) == 2) " and " else ", and "

  as.character(
    glue::glue_collapse(x, sep = ', ', last = last)
  )

}

.paste_named_vec <- function(x, name_fill = "MISSING_NAME"){

  if(is.list(x)){
    return(
      purrr::map(x, .paste_named_vec, name_fill = name_fill) %>%
      purrr::imap_chr(
        .f = function(.x, .n){
          .n <- .n %||% name_fill
          if(.n == "") .n <- name_fill
          glue("{.n} = c({.x})")
        }
      )
    )
  }

  if(is.null(names(x))) names(x) <- name_fill
  names(x)[names(x) == ""] <- name_fill
  paste(paste(names(x), x, sep = " = "), collapse = ", ")

}
