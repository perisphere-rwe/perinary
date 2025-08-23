
# nocov start
#' @noRd
roxy_describe_dd <- function(plural = FALSE){

  if(plural){
    "[Data dictionary][data_dictionary()] objects"
  } else {
    "A [data dictionary][data_dictionary()] object"
  }


}

#' @noRd
roxy_describe_numeric <- function(){

  "A [numeric variable][numeric_variable()]"

}

#' @noRd
roxy_describe_nominal <- function(){

  "A [nominal variable][nominal_variable()]"

}

#' @noRd
roxy_default_dd <- function(){

  "If `NULL`, uses the default dictionary set via [set_default_dictionary()]"

}

#' @noRd
roxy_dotlist <- function(fun){

  glue("This argument allows {paste_collapse(paste_ticks(fun))} to be used programmatically and is ",
       "optional. It is intended to be used as an alternative to `...`.")

}

roxy_dotdots <- function(){

}

# nocov end
