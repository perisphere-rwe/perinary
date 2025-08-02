
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
      if (!is.null(value))
        assert_valid_field(self$name,
                           self$type,
                           field = "divby_modeling",
                           suggest = "Numeric")
    },

    # Retrievers
    get_element = function(x){
      self[[x]]
    },

    fmt_element = function(x){
      .paste_collapse(self$get_element(x))
    },

    chk_element = function(field, value){
      self$check_input(field, value)
    },

    set_element = function(field, value){
      self[[field]] <- value
      invisible(self)
    },

    # modify in place
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
      divby_modeling <- value
      checkmate::assert_numeric(divby_modeling,
                                len = 1,
                                lower = 1,
                                any.missing = FALSE,
                                null.ok = TRUE)
    },

    get_label_and_unit = function(sep = ', '){

      # present units if they are there
      if(!is.null(self$fetch_units)){
        return(paste(self$fetch_label(),
                     self$fetch_units(),
                     sep = sep))
      }

      self$fetch_label()

    },

    get_label_divby = function(){
      paste0(self$fetch_label(), ", per ",
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

      self$check_units(units)
      self$check_divby_modeling(divby_modeling)

      # Set numeric fields
      self$units          <- units
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
                                  null.ok = TRUE)
    },

    check_category_labels = function(value) {
      category_labels <- value
      checkmate::assert_character(category_labels,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
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
      self$check_category_labels(category_labels)

      self$category_levels <- category_levels
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

    # Validation specific to logical variables
    check_category_levels = function(value) {
      if (!is.null(value)) {
        checkmate::assert_logical(value, any.missing = FALSE, len = 2, unique = TRUE)
      }
    },

    check_category_labels = function(value) {
      if (!is.null(value)) {
        checkmate::assert_character(value, len = 2, any.missing = FALSE)
      }
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
      self$dictionary <- self$create_dictionary(vars)

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

    get_variable_names = function(){
      names(self$variables)
    },

    get_variable_recoder = function(name = NULL,
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
          warning("Incomplete recode information for {",
                  paste(unlabeled, collapse = ", "),
                  "} : labels are missing.",
                  call. = FALSE)
        }

      }

      output

    },

    get_level_recoder = function(name = NULL, quiet = FALSE){

      checkmate::assert_character(name, null.ok = TRUE)

      choices <- self$variables %>%
        purrr::map_lgl(~.x$type == "Nominal") %>%
        which() %>%
        names()

      name <- name %||% choices

      output <- character(length = 0L)

      for(i in seq_along(name)){

        checkmate::assert_choice(name[i], choices = choices)

        .labs <- self$variables[[ name[i] ]]$category_labels
        .lvls <- self$variables[[ name[i] ]]$category_levels

        if(is.null(.labs)){

          if(!quiet){
            warning("Recode information for variable {", name[i],
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

    recode = function(x, ..., units = 'none', warn_unmatched = TRUE){

      x_uni <- unique(stats::na.omit(x))

      variable_recoder <- self$get_variable_recoder(quiet = TRUE, units = units)
      level_recoder <- self$get_level_recoder(quiet = TRUE)

      x_in_variable_names <- x_uni %in% names(variable_recoder)
      x_in_variable_levels <- x_uni %in% names(level_recoder)

      if(!is_empty(list(...))){
        variable_recoder %<>% c(list(...))
        level_recoder %<>% c(list(...))
      }

      if(any(x_in_variable_levels & x_in_variable_names)){
        stop("unique values of x were found to be present in ",
             "both the variable labels and the category labels ",
             "for the given dictionary. It is not clear which ",
             "of these should be used to recode values of x.",
             call. = FALSE)
      }

      if(any(x_in_variable_levels)){

        # the names of level_recoder are levels
        dup_levels <- duplicated(names(level_recoder))
        # the values of level_recoder are labels
        dup_labels <- duplicated(level_recoder)

        # mapping the same level to multiple labels causes a problem.
        # e.g., mapping level of "yes" to labels of X and Y? When should
        # it map to X and when should it map to Y? We can't tell if
        # we only see "yes"'s in the vector that we are trying to recode.

        dup_problems <- names(level_recoder)[dup_levels] %>%
          # mapping multiple levels to the same label is okay.
          # e.g., mapping levels of X and Y to the label of "yes"? no problem.
          # this step removed those.
          setdiff(names(level_recoder)[dup_labels]) %>%
          # this step makes it so we only throw warnings if the duplicates
          # are actually in the input vector
          intersect(x)

        if(!purrr::is_empty(dup_problems)){

          dups_explained <- dup_problems %>%
            purrr::set_names() %>%
            purrr::map(
              ~ level_recoder[names(level_recoder) %in% .x] %>%
                paste0("'", ., "'") %>%
                paste(collapse = ' and ') %>%
                paste0("The level '", .x, "' maps to labels of ", .)
            )

          msg <- paste0(
            "One or more levels in the dictionary map to multiple labels:\n\n",
            paste("-", paste(dups_explained), collapse = "\n"),
            "\n\n`recode()` can only map a level to one label, ",
            "so only the first label will be used.\n",
            "This issue can be fixed by changing levels of nominal variables ",
            "or by using recode vectors for each variable, separately."
          )

          warning(msg, call. = FALSE)

        }

      }

      if(all(x_in_variable_levels)){
        return(dplyr::recode(x, !!!level_recoder))
      }

      if(all(x_in_variable_names)){
        return(dplyr::recode(x, !!!variable_recoder))
      }

      leftovers <- setdiff(x_uni, c(names(variable_recoder),
                                    names(level_recoder)))

      if(!is_empty(leftovers) && warn_unmatched){
        warning(
          "Unique values in x could not be matched with variable labels ",
          "or variable level labels in the dictionary. The x values that ",
          "could not be matched are: ", paste(leftovers, collapse = ", "),
          ". To disable this warning, set `warn_unmatched = FALSE` in the ",
          "call to `recode()`.",
          call. = FALSE
        )
      }

      dplyr::recode(x, !!!c(variable_recoder, level_recoder))

    },

    # Function to create tibble summary of variables
    create_dictionary = function(vars) {

      tibble::tibble(
        name            = purrr::map_chr(vars, ~ .x$fmt_name()),
        label           = purrr::map_chr(vars, ~ .x$fmt_label()),
        description     = purrr::map_chr(vars, ~ .x$fmt_description()),
        units           = purrr::map_chr(vars, ~ .x$fmt_units()),
        divby_modeling  = purrr::map_chr(vars, ~ .x$fmt_divby_modeling()),
        category_levels = purrr::map_chr(vars, ~ .x$fmt_category_levels()),
        category_labels = purrr::map_chr(vars, ~ .x$fmt_category_labels())
      )

    },

    check_inputs_match = function(key){

      unmatched_inputs <- setdiff(names(key), self$dictionary$name)

      if(!purrr::is_empty(unmatched_inputs)){

        msg <- c(
          "unrecognized input name(s)",
          set_names(unmatched_inputs, "i"),
          i = "Inputs must match names of variables in the dictionary."
        )

        rlang::abort(message = msg, call = NULL)

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


    # Check before you modify. If we try to do both at once and a
    # check fails halfway through, the dictionary is still modified
    # up to that point, which may cause unexpected errors for users.

    check_modify_call = function(key, field){

      checkmate::assert_character(names(key),
                                  any.missing = FALSE,
                                  unique = TRUE,
                                  null.ok = FALSE)

      self$check_inputs_match(key)

      self$check_inputs_unique(key)

      for(i in names(key)){
        self$variables[[i]]$chk_element(field = field, value = key[[i]])
      }

    },


    modify_dictionary = function(key, field){

      for(i in names(key)){

        self$variables[[i]]$set_element(field = field,
                                        value = key[[i]])

      }

      self$dictionary <- self$create_dictionary(self$variables)

    },

    # Print method
    print = function(...) {

      cat("Data Dictionary:\n")
      print(self$dictionary, n = Inf)

    }
  ),

  private = list(
    deep_clone = function(name, value){
      if(name == 'variables'){
        purrr::map(value, .f = ~.x$clone(deep=TRUE))
      } else {
        value
      }
    }
  )

)

