
# DataVariable ----

DataVariable <- R6Class(

  "DataVariable",

  public = list(

    # relevant for all variables
    name = NULL,
    type = NULL,
    label = NULL,
    description = NULL,

    # relevant for nominal variables but should be
    # set to a value of 'none' for numeric variables
    category_levels = NULL,
    category_labels = NULL,

    # relevant for numeric variables but should be
    # set to a value of 'none' for nominal variables
    units = NULL,
    divby_modeling = NULL,

    # Checkers

    check_input = function(field, value){
      switch(field,
             "name"            = self$check_name(value),
             "type"            = self$check_type(value),
             "label"           = self$check_label(value),
             "description"     = self$check_description(value),
             "units"           = self$check_units(value),
             "divby_modeling"  = self$check_divby_modeling(value),
             "category_level"  = self$check_category_levels(value),
             "category_label"  = self$check_category_labels(value))
    },

    check_name = function(value) {
      name <- value
      checkmate::assert_character(name, len = 1, any.missing = FALSE)
    },

    check_type = function(value) {
      type <- value
      checkmate::assert_character(type, len = 1, any.missing = FALSE)
      checkmate::assert_choice(type, choices = c("Data",
                                                 "Numeric",
                                                 "Nominal",
                                                 "Logical",
                                                 "Identifier",
                                                 "Date"))
    },

    check_label = function(value) {
      label <- value
      checkmate::assert_character(label,
                                  len = 1,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },

    check_description = function(value) {
      description <- value
      checkmate::assert_character(description,
                                  len = 1,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },

    check_category_levels = function(value) {
      if(!is.null(value))
        assert_valid_field(self$name,
                           self$type,
                           field = "category_levels",
                           suggest = "Nominal")
    },

    check_category_labels = function(value) {
      if(!is.null(value))
        assert_valid_field(self$name,
                           self$type,
                           field = "category_labels",
                           suggest = "Nominal")
    },

    check_units = function(value) {
      if (!is.null(value))
        assert_valid_field(self$name,
                           self$type,
                           field = "units",
                           suggest = "Numeric")
    },

    check_divby_modeling = function(value) {
      if (!is.null(value)){

        assert_valid_field(self$name,
                           self$type,
                           field = "divby_modeling",
                           suggest = "Numeric")

      }

    },

    # Retrievers
    get_element = function(x){
      self[[x]]
    },

    fmt_element = function(x){
      paste_collapse(self$get_element(x))
    },

    chk_element = function(field, value){
      self$check_input(field, value)
    },

    set_element = function(field, value){
      self[[field]] <- value
      invisible(self)
    },

    # modify
    set_name = function(value) self$set_element("name", value),
    set_type = function(value) self$set_element("type", value),
    set_label = function(value) self$set_element("label", value),
    set_description = function(value) self$set_element("description", value),
    set_units = function(value) self$set_element("units", value),
    set_divby_modeling = function(value) self$set_element("divby_modeling", value),
    set_category_levels = function(value) self$set_element("category_levels", value),
    set_category_labels = function(value) self$set_element("category_labels", value),

    # return the value (can return NULL)
    get_name = function() self$get_element("name"),
    get_type = function() self$get_element("type"),
    get_label = function() self$get_element("label"),
    get_description = function() self$get_element("description"),
    get_units = function() self$get_element("units"),
    get_divby_modeling = function() self$get_element("divby_modeling"),
    get_category_levels = function() self$get_element("category_levels"),
    get_category_labels = function() self$get_element("category_labels"),

    get_label_and_unit = function(sep = ', ') self$fetch_label(),
    get_label_divby = function() self$fetch_label(),

    # return a value, falling back to secondary options if needed
    fetch_name = function()
      self$get_name(),

    fetch_type = function()
      self$get_type(),

    fetch_label = function()
      self$get_label() %||% self$fetch_name(),

    fetch_description = function()
      self$get_description() %||% self$fetch_label(),

    fetch_units = function()
      self$get_units(),

    fetch_divby_modeling = function()
      self$get_divby_modeling() %||% 1,

    fetch_category_levels = function()
      self$get_category_levels(),

    fetch_category_labels = function()
      self$get_category_labels() %||% self$fetch_category_levels(),

    # format value, can return 'none' when value is NULL
    fmt_name = function() self$fmt_element("name"),
    fmt_type = function() self$fmt_element("type"),
    fmt_label = function() self$fmt_element("label"),
    fmt_description = function() self$fmt_element("description"),
    fmt_units = function() self$fmt_element("units"),
    fmt_divby_modeling = function() self$fmt_element("divby_modeling"),
    fmt_category_levels = function() self$fmt_element("category_levels"),
    fmt_category_labels = function() self$fmt_element("category_labels"),

    # Constructor
    initialize = function(name,
                          type = "Data",
                          label = NULL,
                          description = NULL) {

      if (missing(name) || !is.character(name) || length(name) != 1) {
        rlang::abort(
          message = "'name' must be a single character string and is required.",
          call = NULL
        )
      }


      self$check_name(name)
      self$check_type(type)
      self$check_label(label)
      self$check_description(description)

      self$name        <- name
      self$type        <- type
      self$label       <- label
      self$description <- description


    },

    # Method to print object information
    print = function(...) {
      cat(self$fmt_type(), "Variable:\n")
      cat("  Name               :", self$fmt_name(),        "\n")
      cat("  Label              :", self$fmt_label(),       "\n")
      cat("  Description        :", self$fmt_description(), "\n")
    }
  )

)

# NumericVariable ----

NumericVariable <- R6Class(

  "NumericVariable",

  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    check_units = function(value) {
      units <- value
      checkmate::assert_character(units,
                                  len = 1,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },


    check_divby_modeling = function(value) {

      if(is.null(self$units) && !is.null(value)){
        rlang::abort(
          message = c(
            "x" = "Cannot set `divby_modeling` if units are unknown",
            "i" = glue("Use `set_units()` to specify units for `{self$name}`")
          ),
          call = NULL
        )
      }

      divby_modeling <- value

      checkmate::assert_numeric(divby_modeling,
                                len = 1,
                                lower = 1,
                                any.missing = FALSE,
                                null.ok = TRUE)

    },

    get_label_and_unit = function(sep = ', '){

      if(is.null(self$units)) return(self$fetch_label())

      # present units if they are there
      paste(self$fetch_label(), self$fetch_units(), sep = sep)

    },

    get_label_divby = function(sep = ", per "){
      paste0(self$fetch_label(), sep,
             self$fetch_divby_modeling(), " ",
             self$fetch_units() %||% "units")
    },

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          units = NULL,
                          divby_modeling = NULL) {

      # Call the parent class (DataVariable) initialize
      super$initialize(
        name = name,
        type = "Numeric",
        label = label,
        description = description
      )

      # Set numeric fields
      self$check_units(units)
      self$units <- units

      self$check_divby_modeling(divby_modeling)
      self$divby_modeling <- divby_modeling


    },

    # Overriding print method to include unit information
    print = function(...) {
      super$print(...)          # Call the parent class print
      cat("  Units              :", self$fmt_units(), "\n")
      cat("  Modeling Divisor   :", self$fmt_divby_modeling(), "\n")
    }
  )
)

# NominalVariable ----

NominalVariable <- R6Class(
  "NominalVariable",
  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    check_category_levels = function(value) {
      category_levels <- value
      checkmate::assert_character(category_levels,
                                  any.missing = FALSE,
                                  null.ok = TRUE,
                                  unique = TRUE)
    },

    check_category_labels = function(value) {


      category_labels <- value
      checkmate::assert_character(category_labels,
                                  any.missing = FALSE,
                                  null.ok = TRUE)

      if(!is.null(value)){
        # only you can prevent label duplication
        if(any(duplicated(value))){

          value_tbl <- table(value)

          dups <- value_tbl[value_tbl > 1]

          dups_explained <- dups %>%
            purrr::imap_chr(
              ~ names(value)[value==.y] %>%
                paste_quotes() %>%
                paste_collapse() %>%
                paste("you are assigning levels", ., "to the label",
                      paste_quotes(.y))
            ) %>%
            purrr::set_names(nm = "x")

          cli::cli_abort(
            message = c(
              "!" = paste("Invalid duplication of {length(dups)} label{?s}",
                          "for variable", paste_ticks(self$name)),
              dups_explained,
              "i" = "category labels must be unique within variables"
            ),
            call = NULL
          )

        }
      }


    },


    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          category_levels = NULL,
                          category_labels = NULL) {

      # Call parent class initialize
      super$initialize(
        name = name,
        type = "Nominal",
        label = label,
        description = description
      )

      self$check_category_levels(category_levels)
      self$category_levels <- category_levels

      self$check_category_labels(category_labels)
      self$category_labels <- category_labels %||% category_levels

    },

    # Overriding print method to include category information
    print = function(...) {

      super$print(...)  # Call the parent class print

      cat("  Category Levels    :", self$fmt_category_levels(), "\n")
      cat("  Category Labels    :", self$fmt_category_labels(), "\n")

    }
  )
)

# LogicalVariable ----

LogicalVariable <- R6::R6Class(
  "LogicalVariable",
  inherit = DataVariable,

  public = list(

    check_category_levels = function(value) {
      category_levels <- value
      checkmate::assert_logical(category_levels,
                                any.missing = FALSE,
                                len = 2,
                                null.ok = TRUE,
                                unique = TRUE)
    },

    check_category_labels = function(value) {
      category_labels <- value
      checkmate::assert_character(category_labels,
                                  any.missing = FALSE,
                                  max.len = 2,
                                  null.ok = TRUE,
                                  unique = TRUE)
    },

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          category_labels = c("FALSE", "TRUE")) {

      # Call parent initialize with type = "Logical"
      super$initialize(
        name = name,
        type = "Logical",
        label = label,
        description = description
      )

      # Logical variables always have TRUE/FALSE levels
      self$category_levels <- c(FALSE, TRUE)

      # Set and validate labels
      self$check_category_labels(category_labels)
      self$category_labels <- category_labels %||% as.character(self$category_levels)
    },

    # Print method (mirroring NominalVariable)
    print = function(...) {
      super$print(...)
      cat("  Category Levels    :", self$fmt_category_levels(), "\n")
      cat("  Category Labels    :", self$fmt_category_labels(), "\n")
    }
  )
)


# DateVariable ----

DateVariable <- R6Class(
  "DateVariable",
  inherit = DataVariable,

  public = list(

    # Optionally store a format string like "%Y-%m-%d"
    date_format = NULL,

    check_date_format = function(value) {
      if (!is.null(value)) {
        checkmate::assert_character(value, len = 1, any.missing = FALSE)
      }
    },

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          date_format = NULL) {

      super$initialize(
        name = name,
        type = "Date",
        label = label,
        description = description
      )

      self$check_date_format(date_format)
      self$date_format <- date_format
    },

    print = function(...) {
      super$print(...)
      if (!is.null(self$date_format)) {
        cat("  Date Format        :", self$date_format, "\n")
      }
    }
  )
)

# IdentifierVariable ----

IdentifierVariable <- R6Class(
  "IdentifierVariable",
  inherit = DataVariable,

  public = list(

    # Constructor
    initialize = function(name, label = NULL, description = NULL) {

      super$initialize(
        name = name,
        type = "Identifier",
        label = label,
        description = description
      )
    },

    print = function(...) {
      super$print(...)
    }

  )
)


# DataDictionary ----

is_data_dictionary <- function(x){
  inherits(x, "DataDictionary")
}

DataDictionary <- R6Class(

  "DataDictionary",

  public = list(

    # list of numeric and/or nominal variables
    variables = NULL,

    # data frame summary
    dictionary = NULL,

    category_key = NULL,

    # manage modification by reference
    copy_on_modify = NULL,

    # Constructor
    initialize = function(vars, copy_on_modify = TRUE) {

      # Validate that all are instances of DataVariable or its children
      if (length(vars) == 0) {
        stop("At least one variable must be provided ",
             "to create a DataDictionary.", call. = FALSE)
      }

      if (!all(purrr::map_lgl(vars, ~ inherits(.x, "DataVariable")))) {
        stop("All inputs must inherit from 'DataVariable' ",
             "(e.g., NumericVariable, NominalVariable).", call. = FALSE)
      }

      var_names <- purrr::map_chr(vars, ~.x$name)

      # Store variables list
      self$variables <- purrr::set_names(vars, var_names)

      # Extract data for the tibble
      self$dictionary <- private$create_dictionary(self$variables)
      self$category_key <- private$create_category_key(self$variables)
      self$copy_on_modify <- copy_on_modify

    },

    get_label = function(name, units = 'none'){

      switch(
        units,
        'none'        = self$variables[[name]]$get_label(),
        'descriptive' = self$variables[[name]]$get_label_and_unit(),
        'model'       = self$variables[[name]]$get_label_divby()
      )

    },

    get_description = function(name){
      self$variables[[name]]$get_description()
    },

    get_divby = function(name){
      self$variables[[name]]$get_divby_modeling()
    },

    get_units = function(name){
      self$variables[[name]]$get_units()
    },

    get_category_levels = function(name, concatenate = TRUE){

      res <- purrr::map(
        .x = name,
        .f = ~ self$variables[[.x]]$fetch_category_levels()
      )

      if(!concatenate) return(res)

      purrr::reduce(res, .f = base::c)

    },

    get_category_labels = function(name, concatenate = TRUE){

      res <- purrr::map(
        .x = name,
        .f = ~ self$variables[[.x]]$fetch_category_labels()
      )

      if(!concatenate) return(res)

      purrr::reduce(res, .f = base::c)

    },

    get_names = function(){
      names(self$variables)
    },

    get_names_nominal = function(){
      private$get_names_by_type("Nominal")
    },

    get_names_numeric = function(){
      private$get_names_by_type("Numeric")
    },

    get_names_date = function(){
      private$get_names_by_type("Date")
    },

    get_names_logical = function(){
      private$get_names_by_type("Logical")
    },

    get_names_identifier = function(){
      private$get_names_by_type("Identifier")
    },

    get_names_with_units = function(){
      names(purrr::discard(self$variables, ~is.null(.x$units)))
    },

    get_names_with_divby = function(){
      names(purrr::discard(self$variables, ~is.null(.x$divby_modeling)))
    },

    get_name_translater = function(name = NULL,
                                   units = 'none',
                                   quiet = FALSE){

      checkmate::assert_character(name, null.ok = TRUE)

      .name <- name %||% names(self$variables)

      for(i in seq_along(.name)){
        checkmate::assert_choice(.name[i],
                                 .var.name = .name[i],
                                 choices = names(self$variables))
      }

      output <- purrr::map(
        .x = purrr::set_names(.name),
        .f = ~ self$get_label(.x, units)
      ) %>%
        purrr::compact()

      unlabeled <- setdiff(.name, names(output))

      if(!is_empty(unlabeled)){

        if(!quiet){
          warning("Incomplete translate information for {",
                  paste(unlabeled, collapse = ", "),
                  "} : labels are missing.",
                  call. = FALSE)
        }

      }

      output

    },

    get_category_translater = function(name = NULL, quiet = FALSE){

      checkmate::assert_character(name, null.ok = TRUE)

      choices <- self$variables %>%
        purrr::map_lgl(~.x$type == "Nominal") %>%
        which() %>%
        names()

      # give back all the translaters if no name is specified
      name <- name %||% choices

      output <- character(length = 0L)

      for(i in seq_along(name)){

        checkmate::assert_choice(name[i], choices = choices)

        .labs <- self$variables[[ name[i] ]]$category_labels
        .lvls <- self$variables[[ name[i] ]]$category_levels

        if(is.null(.labs)){

          if(!quiet){
            warning("Translate information for variable {", name[i],
                    "} is incomplete: labels are missing",
                    call. = FALSE)
          }

          next

        }

        output %<>% append(
          values = purrr::set_names(
            x = self$variables[[ name[i] ]]$category_labels,
            nm = self$variables[[ name[i] ]]$category_levels
          )
        )

      }

      output

    },

    get_category_key = function(){
      self$category_key
    },

    index = function(data, names = 'name', levels = 'level'){

      name_sort <- factor(data[[names]],
                          levels = union(self$dictionary$name,
                                         unique(data[[names]])))

      split(data, f = name_sort, drop = FALSE) %>%
        purrr::imap_dfr(
          .f = ~ {

            out <- .x

            if(.y %in% self$get_names()){

              if(self$variables[[.y]]$type == "Nominal"){

                .levels <- self$variables[[.y]]$get_category_levels()

                out <- .x %>%
                  dplyr::arrange(
                    factor(.data[[levels]], levels = .levels)
                  )

              }

            }

            out

          }

        )

    },

    translate_data = function(x, ...,
                              units,
                              warn_unmatched,
                              apply_variable_labels,
                              apply_category_labels,
                              nominals_to_factor){

      overlapping_variables <- self %>%
        infer_overlapping_variables(x, warn_unmatched)

      unit_variables <- overlapping_variables %>%
        intersect(self$get_names_with_units())

      nominal_variables <- overlapping_variables %>%
        intersect(self$get_names_nominal())

      for(i in overlapping_variables){

        .label <- self$get_label(i)

        if(i %in% unit_variables && units != 'none'){

          .label <- .label %||% i
          .unit <- self$get_units(i)
          .divby <- self$get_divby(i) %||% 1
          # variables without divby need to say per 1 unit for consistency

          if(units == 'model'){

            x[[i]] %<>% magrittr::divide_by(.divby)
            .label %<>% paste0(", per ", .divby, " ", .unit)

          } else if(units == 'descriptive') {

            .label %<>% paste0(", ", .unit)

          }

        }

        if(i %in% nominal_variables && apply_category_labels){
          x[[i]] %<>%
            self$translate_categories(names = i,
                                      to_factor = nominals_to_factor)
        }

        if(apply_variable_labels){
          attr(x[[i]], 'label') <- .label
        }


      }

      x

    },

    translate_names = function(x, ...,
                               .list = NULL,
                               units = "none",
                               to_factor = FALSE,
                               warn_unmatched = TRUE){

      x_uni <- unique(stats::na.omit(x))

      .list <- .list %||% list(...)

      translater <- self$get_name_translater(quiet = TRUE, units = units)

      if(!is_empty(.list)) translater %<>% c(.list)

      unmatched <- setdiff(x_uni, names(translater))

      if(!is_empty(unmatched) && warn_unmatched){
        rlang::warn(
          message = c(
            "i" = "Unique values in `x` could not be matched with labels in `dictionary`:",
            purrr::set_names(unmatched, "i"),
            "i" = "To disable this warning, set `warn_unmatched = FALSE` in `translate()`."
          )
        )
      }

      if(to_factor){
        private$recode_as_factor(x, translater, unmatched)
      } else {
        private$recode_as_character(x, translater)
      }

    },

    translate_categories = function(x, ...,
                                    .list = NULL,
                                    names = NULL,
                                    to_factor = FALSE,
                                    warn_unmatched = TRUE){

      .list <- .list %||% list(...)

      if(!is.null(names)){

        if(length(names) == 1) names <- rep(names, length(x))

        stopifnot(length(names) == length(x))

        single_name <- length(unique(names)) == 1

        if(!single_name && to_factor){

          cli::cli_warn(
            message = c(
              "Detected more than one unique value in {.var name} with {.var to_factor = TRUE}",
              i = "{.var translate_categories()} does not concatenate factor levels across multiple variables",
              i = "output will be returned with type {.var character} instead of {.var factor}"
            )
          )

          to_factor <- FALSE

        }

        return(
          tibble::tibble(x = x, name = names) %>%
            dplyr::group_by(name) %>%
            tidyr::nest(data = c(x)) %>%
            dplyr::mutate(
              out = purrr::map2(
                .x = data,
                .y = name,
                .f = ~ {

                  if(.y %in% self$get_names()){

                    translater <- self$get_category_translater(
                      name = .y,
                      quiet = TRUE
                    ) %>%
                      private$bind_list_to_translater(
                        .list = .list,
                        add_leftovers = single_name
                        # if you add leftovers, it can disrupt the order
                        # of levels when there is more than one name.
                      )

                  } else {
                    # if the name doesn't match, there should be a mapping
                    # available in the .list input
                    translater <- .list

                    if(is_empty(translater)){
                      return(rep(NA_character_, length(.x$x)))
                    }

                  }

                  unmatched <- unique(stats::na.omit(.x$x)) %>%
                    setdiff(names(translater))

                  if(to_factor){
                    private$recode_as_factor(.x$x, translater, unmatched)
                  } else {
                    private$recode_as_character(.x$x, translater)
                  }

                }
              )
            ) %>%
            tidyr::unnest(cols = c(data, out)) %>%
            dplyr::pull(out)
        )

      }

      x_uni <- unique(stats::na.omit(x))

      translater <-
        self$get_category_translater(name = names, quiet = TRUE) %>%
        private$bind_list_to_translater(.list = .list)


      # the names of translater are levels
      dup_levels <- duplicated(names(translater))
      # the values of translater are labels
      dup_labels <- duplicated(translater)

      # mapping the same level to multiple labels causes a problem.
      # e.g., mapping level of "yes" to labels of X and Y? When should
      # it map to X and when should it map to Y? We can't tell if
      # we only see "yes"'s in the vector that we are trying to translate.

      dup_problems <- names(translater)[dup_levels] %>%
        # mapping multiple levels to the same label is okay.
        # e.g., mapping levels of X and Y to the label of "yes"? no problem.
        # this step removed those.
        setdiff(names(translater)[dup_labels]) %>%
        # this step makes it so we only throw warnings if the duplicates
        # are actually in the input vector
        intersect(x)

      if(!purrr::is_empty(dup_problems)){

        dups_explained <- dup_problems %>%
          purrr::map_chr(
            ~ translater[names(translater) %in% .x] %>%
              paste0("'", ., "'") %>%
              paste(collapse = ' and ') %>%
              paste0("The category '", .x, "' maps to labels of ", .)
          ) %>%
          purrr::set_names(nm = 'x')

        cli::cli_abort(
          message = c(
            "Detected one-to-many relationship between categories and labels",
            dups_explained,
            "v" = "Use `names` to clarify your mapping (see examples in `?translate_categories`)"
          ),
          call = NULL
        )

      }

      # don't let these be recoded to NA if returning output as factor
      unmatched <- x_uni %>%
        setdiff(names(translater))

      if(to_factor){
        private$recode_as_factor(x, translater, unmatched)
      } else {
        private$recode_as_character(x, translater)
      }

    },

    translate_nominal = function(x, ...,
                                 units,
                                 warn_unmatched){

      x_uni <- unique(stats::na.omit(x))

      name_translater <- self$get_name_translater(quiet = TRUE, units = units)
      level_translater <- self$get_category_translater(quiet = TRUE)

      x_in_variable_names <- x_uni %in% names(name_translater)
      x_in_variable_levels <- x_uni %in% names(level_translater)

      if(!is_empty(list(...))){
        name_translater %<>% c(list(...))
        level_translater %<>% c(list(...))
      }

      if(any(x_in_variable_levels & x_in_variable_names)){
        stop("unique values of x were found to be present in ",
             "both the variable labels and the category labels ",
             "for the given dictionary. It is not clear which ",
             "of these should be used to translate values of x.",
             call. = FALSE)
      }

      if(any(x_in_variable_levels)){

        # the names of level_translater are levels
        dup_levels <- duplicated(names(level_translater))
        # the values of level_translater are labels
        dup_labels <- duplicated(level_translater)

        # mapping the same level to multiple labels causes a problem.
        # e.g., mapping level of "yes" to labels of X and Y? When should
        # it map to X and when should it map to Y? We can't tell if
        # we only see "yes"'s in the vector that we are trying to translate.

        dup_problems <- names(level_translater)[dup_levels] %>%
          # mapping multiple levels to the same label is okay.
          # e.g., mapping levels of X and Y to the label of "yes"? no problem.
          # this step removed those.
          setdiff(names(level_translater)[dup_labels]) %>%
          # this step makes it so we only throw warnings if the duplicates
          # are actually in the input vector
          intersect(x)

        if(!purrr::is_empty(dup_problems)){

          dups_explained <- dup_problems %>%
            purrr::set_names() %>%
            purrr::map(
              ~ level_translater[names(level_translater) %in% .x] %>%
                paste0("'", ., "'") %>%
                paste(collapse = ' and ') %>%
                paste0("The level '", .x, "' maps to labels of ", .)
            )

          msg <- paste0(
            "One or more levels in the dictionary map to multiple labels:\n\n",
            paste("-", paste(dups_explained), collapse = "\n"),
            "\n\n`translate()` can only map a level to one label, ",
            "so only the first label will be used.\n",
            "This issue can be fixed by changing levels of nominal variables ",
            "or by using translate vectors for each variable, separately."
          )

          warning(msg, call. = FALSE)

        }

      }

      if(all(x_in_variable_levels)){

        out <- private$recode_as_factor(x, level_translater)
        # out <- dplyr::recode(x, !!!level_translater)

        return(out)

      }

      if(all(x_in_variable_names)){

        out <- private$recode_as_factor(x, name_translater)
        # out <- dplyr::recode(x, !!!name_translater)

        return(out)

      }

      leftovers <- setdiff(x_uni, c(names(name_translater),
                                    names(level_translater)))

      if(!is_empty(leftovers) && warn_unmatched){
        rlang::warn(
          message = c(
            "i" = "Unique values in x could not be matched with variable labels or variable level labels in the dictionary.",
            "i" = glue("The x values that could not be matched are: {paste_collapse(leftovers)}"),
            "i" = "To disable this warning, set `warn_unmatched = FALSE` in `translate()`."
          )
        )
      }

      # would not make sense to convert this to a factor
      out <- dplyr::recode(x, !!!c(name_translater, level_translater))

      out

    },

    translate_numeric = function(x,
                                 name,
                                 units,
                                 warn_unmatched){

      if(!name %in% self$get_names_numeric()){
        stop("name mis-match")
      }

      x

    },

    set_identifiers = function(variable_names){

      for(i in variable_names){

        self$variables[[i]] <-
          identifier_variable(name = i,
                              label = self$get_label(i),
                              description = self$get_description(i))

      }

      self$dictionary <- private$create_dictionary(self$variables)
      self$category_key <- private$create_category_key(self$variables)


    },

    # Print method
    print = function(...) {

      cat("Data Dictionary:\n")
      print(self$dictionary, n = Inf)

    },

    check_modify_call = function(key, field){

      checkmate::assert_character(names(key),
                                  any.missing = FALSE,
                                  unique = TRUE,
                                  null.ok = FALSE)

      private$check_inputs_match(key)
      private$check_inputs_unique(key)

      for(i in names(key)){
        self$variables[[i]]$chk_element(field = field, value = key[[i]])
      }

    },

    # Check before you modify. If we try to do both at once and a
    # check fails halfway through, the dictionary is still modified
    # up to that point.

    modify_dictionary = function(key, field){

      for(i in names(key)){

        self$variables[[i]]$set_element(field = field,
                                        value = key[[i]])

      }

      self$dictionary <- private$create_dictionary(self$variables)
      self$category_key <- private$create_category_key(self$variables)

    }

  ),

  private = list(
    deep_clone = function(name, value){
      if(name == 'variables'){
        purrr::map(value, .f = ~.x$clone(deep=TRUE))
      } else {
        value
      }
    },

    get_types = function() {
      purrr::map_chr(self$variables, 'type')
    },

    get_names_by_type = function(type) {
      names(self$variables)[private$get_types() == type]
    },

    # Function to create tibble summary of variables
    create_dictionary = function(vars) {

      tibble::tibble(
        name            = purrr::map_chr(vars, ~ .x$fmt_name()),
        type            = purrr::map_chr(vars, ~ .x$fmt_type()),
        label           = purrr::map_chr(vars, ~ .x$fmt_label()),
        description     = purrr::map_chr(vars, ~ .x$fmt_description()),
        units           = purrr::map_chr(vars, ~ .x$fmt_units()),
        divby_modeling  = purrr::map_chr(vars, ~ .x$fmt_divby_modeling()),
        category_levels = purrr::map_chr(vars, ~ .x$fmt_category_levels()),
        category_labels = purrr::map_chr(vars, ~ .x$fmt_category_labels())
      )

    },

    create_category_key = function(vars){

      tibble::enframe(vars, name = 'name') %>%
        dplyr::mutate(type = purrr::map_chr(value, "type")) %>%
        dplyr::filter(type == "Nominal") %>%
        dplyr::mutate(level = purrr::map(value, ~.x$category_levels),
                      label = purrr::map(value, ~.x$category_labels)) %>%
        dplyr::select(name, level, label) %>%
        tidyr::unnest(cols = c(level, label)) %>%
        dplyr::group_by(name) %>%
        dplyr::mutate(reference = level == dplyr::first(level)) %>%
        dplyr::ungroup()

    },

    check_inputs_match = function(key, dictionary_name = NULL){

      unmatched_inputs <- setdiff(names(key), self$dictionary$name)

      if(!purrr::is_empty(unmatched_inputs)){

        n <- length(unmatched_inputs)
        nm <- paste_collapse(unmatched_inputs)
        dd <- paste(dictionary_name %||% "dictionary",
                    "get_names()",
                    sep = "$")

        cli::cli_abort(
          c(
            "Invalid names in {.var ...} or {.var .list}",
            "i" = paste("There {?is/are} {n} unrecognized name{?s}:", nm),
            "x" = "Input names must match dictionary variable names.",
            ">" = "Use `{dd}` to see valid names"
          )
        )

      }

    },

    check_inputs_unique = function(key){

      duplicated_inputs <- table(names(key)) %>%
        tibble::enframe() %>%
        dplyr::mutate(value = as.numeric(value)) %>%
        dplyr::filter(value > 1) %>%
        dplyr::pull(name)

      if(!purrr::is_empty(duplicated_inputs)){
        warning("duplicated input name(s): \n\n",
                paste(paste("-", duplicated_inputs), collapse = "\n"),
                "\n\nInputs should only need to be specified once."
        )
      }

    },

    translate_categories_internal = function(x, .list, name){

      if(name %in% self$get_names()){
        translater <-
          self$get_category_translater(name = name, quiet = TRUE)
      } else {
        translater <- .list
      }

      private$recode_as_factor(x, translater)

    },

    bind_list_to_translater = function(translater, .list,
                                       add_replacements = TRUE,
                                       add_leftovers = TRUE){

      # let ... replace categories or add new ones
      # - replace existing categories if needed
      # - add the leftovers to translater

      .list_split <- tibble::enframe(.list) %>%
        dplyr::mutate(in_translater = factor(name %in% names(translater),
                                             levels = c(FALSE, TRUE),
                                             labels = c("leftover",
                                                        "replace"))) %>%
        split(f = .$in_translater, drop = FALSE) %>%
        purrr::map( ~ .x %>%
                      dplyr::select(name, value) %>%
                      tibble::deframe() %>%
                      unlist())

      if(!is_empty(.list_split$replace) && add_replacements){
        translater[names(.list_split$replace)] <- .list_split$replace
      }

      if(!is_empty(.list_split$leftover) && add_leftovers){
        translater %<>% c(.list_split$leftover)
      }

      translater

    },

    recode_as_factor = function(x, translater, unmatched = NULL){

      x %>%
        dplyr::recode(!!!translater) %>%
        factor(levels = c(translater, unmatched))

    },

    recode_as_character = function(x, translater){

      as.character(x) %>%
        dplyr::recode(!!!translater)

    }

  )

)

