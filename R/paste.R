#' Internal paste helpers
#'
#' @description
#' Small utilities to format vectors/lists for readable messages and
#' logs. Used throughout `perinary` to make assertion and warning text
#' concise and human-friendly.
#'
#' - **`paste_collapse()`**: collapse a vector with commas and a natural
#'   language conjunction before the last element (e.g., `"a, b, and c"`).
#' - **`paste_named_vec()`**: format a (named) vector or list as
#'   `"name = value"` pairs (lists are rendered as `name = c(v1, v2, ...)`),
#'   filling in missing/empty names with a sentinel.
#'
#' @section Conventions:
#' - `NULL` inputs are rendered as `"none"` to avoid noisy `NULL` prints.
#' - The last separator is `" and "` for length 2, otherwise `", and "`.
#' - Empty or missing names are replaced with `name_fill` (default
#'   `"MISSING_NAME"`).
#'
#' @keywords internal
#' @name paste_helpers
NULL

#' @rdname paste_helpers
#'
#' @param x A vector (character, numeric, etc.) to collapse. `NULL`
#'   returns `"none"`. Length-1 values are coerced to character and
#'   returned unchanged.
#'
#' @returns
#' `paste_collapse()` returns a single character scalar with elements
#' of `x` collapsed by `", "` and a natural `"and"` before the last item.
#'

paste_collapse <- function(x, as_code=FALSE){

  if(is.null(x)) return("none")

  if(as_code) x %<>% paste_ticks()

  if(length(x) == 1) return(as.character(x))

  last <- if(length(x) == 2) " and " else ", and "

  as.character(
    glue::glue_collapse(x, sep = ', ', last = last)
  )

}

paste_ticks <- function(x){
  paste0("`", x, "`")
}

paste_quotes <- function(x){
  paste0("\"", x, "\"")
}

#' @rdname paste_helpers
#'
#' @param name_fill Character scalar used to replace missing or empty
#'   names when formatting (default `"MISSING_NAME"`).
#'
#' @returns
#' `paste_named_vec()` returns:
#' - If `x` is a **list**: a named character vector, where each element is
#'   of the form `"name = c(v1, v2, ...)"`. Each element name corresponds
#'   to the original list element name (or `name_fill` if missing/empty).
#' - If `x` is an **atomic vector**: a single character scalar of the form
#'   `"name1 = value1, name2 = value2, ..."`.
#'

paste_named_vec <- function(x, name_fill = "MISSING_NAME"){

  if(is.list(x)){
    return(
      purrr::map(x, paste_named_vec, name_fill = name_fill) %>%
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
