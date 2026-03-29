
# DataVariable ----

#' @importFrom checkmate assert_character assert_choice
#' @importFrom cli cli_abort
DataVariable <- R6Class(

  "DataVariable",

  public = list(

    # relevant for all variables
    name = NULL,
    type = NULL,
    label = NULL,
    description = NULL,
    template_label = NULL,
    template_description = NULL,

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
             "name"                 = self$check_name(value),
             "type"                 = self$check_type(value),
             "label"                = self$check_label(value),
             "description"          = self$check_description(value),
             "template_label"       = self$check_template_label(value),
             "template_description" = self$check_template_description(value),
             "units"                = self$check_units(value),
             "divby_modeling"       = self$check_divby_modeling(value),
             "category_level"       = self$check_category_levels(value),
             "category_label"       = self$check_category_labels(value))
    },

    check_name = function(value) {
      name <- value
      assert_character(name, len = 1, any.missing = FALSE)
    },

    check_type = function(value) {
      type <- value
      assert_character(type, len = 1, any.missing = FALSE)
      assert_choice(type, choices = c("Data",
                                      "Numeric",
                                      "Nominal",
                                      "Logical",
                                      "Identifier",
                                      "Date"))
    },

    check_label = function(value) {
      label <- value
      assert_character(label,
                       len = 1,
                       any.missing = FALSE,
                       null.ok = TRUE)
    },

    check_description = function(value) {
      description <- value
      assert_character(description,
                       len = 1,
                       any.missing = FALSE,
                       null.ok = TRUE)
    },

    check_template_label = function(value){

      assert_character(value,
                       .var.name = 'template_label',
                       len = 1,
                       any.missing = FALSE,
                       null.ok = TRUE)

      if(!is.null(value)) assert_valid_template(value)

    },

    check_template_description = function(value){

      assert_character(value,
                       .var.name = 'template_description',
                       len = 1,
                       any.missing = FALSE,
                       null.ok = TRUE)

      if(!is.null(value)) assert_valid_template(value)

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
    set_template_label = function(value) self$set_element("template_label", value),
    set_template_description = function(value) self$set_element("template_description", value),
    set_units = function(value) self$set_element("units", value),
    set_divby_modeling = function(value) self$set_element("divby_modeling", value),
    set_category_levels = function(value) self$set_element("category_levels", value),
    set_category_labels = function(value) self$set_element("category_labels", value),

    # return the value (can return NULL)
    get_name = function() self$get_element("name"),
    get_type = function() self$get_element("type"),
    get_label = function() self$get_element("label") %||% self$get_template_label(),
    get_description = function() self$get_element("description") %||% self$get_template_description(),
    get_template_label = function() self$get_element("template_label"),
    get_template_description = function() self$get_element("template_description"),
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

    fetch_label = function(){

      self$get_label() %||% self$get_template_label() %||% self$fetch_name()

    },

    fetch_description = function(){

      self$get_description() %||% self$get_template_description() %||% self$fetch_label()

    },

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
    fmt_template_label = function() self$fmt_element("template_label"),
    fmt_template_description = function() self$fmt_element("template_description"),
    fmt_units = function() self$fmt_element("units"),
    fmt_divby_modeling = function() self$fmt_element("divby_modeling"),
    fmt_category_levels = function() self$fmt_element("category_levels"),
    fmt_category_labels = function() self$fmt_element("category_labels"),

    # Constructor
    initialize = function(name,
                          type = "Data",
                          label = NULL,
                          description = NULL,
                          template_label = NULL,
                          template_description = NULL) {

      if (missing(name) || !is.character(name) || length(name) != 1) {
        cli_abort("{.arg name} must be a single character string and is required.")
      }

      self$check_name(name)
      self$check_type(type)
      self$check_label(label)
      self$check_description(description)
      self$check_template_label(template_label)
      self$check_template_description(template_description)

      self$name                 <- name
      self$type                 <- type
      self$label                <- label
      self$description          <- description
      self$template_label       <- template_label
      self$template_description <- template_description


    },

    # Method to print object information
    print = function(...) {
      cat(self$fmt_type(), "Variable:\n")
      cat("  Name                 :", self$fmt_name(),                 "\n")
      cat("  Label                :", self$fmt_label(),                "\n")
      cat("  Description          :", self$fmt_description(),          "\n")
      cat("  Label template       :", self$fmt_template_label(),       "\n")
      cat("  Description template :", self$fmt_template_description(), "\n")
    }
  )

)

# NumericVariable ----

#' @importFrom checkmate assert_character assert_numeric
#' @importFrom cli cli_abort
NumericVariable <- R6Class(

  "NumericVariable",

  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    check_units = function(value) {
      units <- value
      assert_character(units,
                       len = 1,
                       any.missing = FALSE,
                       null.ok = TRUE)
    },


    check_divby_modeling = function(value) {

      if(is.null(self$units) && !is.null(value)){
        cli_abort(c(
          "x" = "Cannot set {.arg divby_modeling} if units are unknown",
          "i" = "Use {.fn set_units} to specify units for {.val {self$name}}"
        ))
      }

      divby_modeling <- value

      assert_numeric(divby_modeling,
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
      cat("  Units                :", self$fmt_units(), "\n")
      cat("  Modeling Divisor     :", self$fmt_divby_modeling(), "\n")
    }
  )
)

# NominalVariable ----

#' @importFrom checkmate assert_character
#' @importFrom cli cli_abort
#' @importFrom purrr imap_chr
#' @importFrom rlang set_names
NominalVariable <- R6Class(
  "NominalVariable",
  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    check_category_levels = function(value) {
      category_levels <- value
      assert_character(category_levels,
                       any.missing = FALSE,
                       null.ok = TRUE,
                       unique = TRUE)
    },

    check_category_labels = function(value) {


      category_labels <- value
      assert_character(category_labels,
                       any.missing = FALSE,
                       null.ok = TRUE)

      if(!is.null(value)){
        # only you can prevent label duplication
        if(any(duplicated(value))){

          value_tbl <- table(value)

          dups <- value_tbl[value_tbl > 1]

          dups_explained <- dups |>
            imap_chr(
              \(.x, .y) names(value)[value==.y] |>
                paste_quotes() |>
                paste_collapse() |>
                (\(s) paste("you are assigning levels", s, "to the label",
                            paste_quotes(.y)))()
            ) |>
            set_names(nm = "x")

          cli_abort(
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

      cat("  Category Levels      :", self$fmt_category_levels(), "\n")
      cat("  Category Labels      :", self$fmt_category_labels(), "\n")

    }
  )
)

# LogicalVariable ----

#' @importFrom checkmate assert_character assert_logical
LogicalVariable <- R6Class(
  "LogicalVariable",
  inherit = DataVariable,

  public = list(

    check_category_levels = function(value) {
      category_levels <- value
      assert_logical(category_levels,
                     any.missing = FALSE,
                     len = 2,
                     null.ok = TRUE,
                     unique = TRUE)
    },

    check_category_labels = function(value) {
      category_labels <- value
      assert_character(category_labels,
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
      cat("  Category Levels      :", self$fmt_category_levels(), "\n")
      cat("  Category Labels      :", self$fmt_category_labels(), "\n")
    }
  )
)


# DateVariable ----

#' @importFrom checkmate assert_character
DateVariable <- R6Class(
  "DateVariable",
  inherit = DataVariable,

  public = list(

    # Optionally store a format string like "%Y-%m-%d"
    date_format = NULL,

    check_date_format = function(value) {
      if (!is.null(value)) {
        assert_character(value, len = 1, any.missing = FALSE)
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
        cat("  Date Format          :", self$date_format, "\n")
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

#' @importFrom checkmate assert_character assert_choice
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr all_of arrange filter first group_by mutate pull relocate
#'   select ungroup
#' @importFrom purrr compact keep imap_dfr map map2 map_chr map_lgl reduce
#' @importFrom rlang !!! is_empty quo_is_null set_names
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble enframe tibble
#' @importFrom tidyr nest unnest
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

    # Named vector of acronyms (NULL by default)
    acronyms = NULL,

    # Constructor
    initialize = function(vars, copy_on_modify = TRUE) {

      # Validate that all are instances of DataVariable or its children
      if (length(vars) == 0) {
        cli_abort("At least one variable must be provided to create a {.cls DataDictionary}.")
      }

      if (!all(map_lgl(vars, ~ inherits(.x, "DataVariable")))) {
        cli_abort(c(
          "All inputs must inherit from {.cls DataVariable}.",
          "i" = "Expected classes like {.cls NumericVariable} or {.cls NominalVariable}."
        ))
      }

      var_names <- map_chr(vars, ~.x$name)

      # Store variables list
      self$variables <- set_names(vars, var_names)

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

    get_template_label = function(name){
      self$variables[[name]]$get_template_label()
    },

    get_description = function(name){
      self$variables[[name]]$get_description()
    },

    get_template_description = function(name){
      self$variables[[name]]$get_template_description()
    },

    get_divby = function(name){
      self$variables[[name]]$get_divby_modeling()
    },

    get_units = function(name){
      self$variables[[name]]$get_units()
    },

    get_category_levels = function(name, concatenate = TRUE){

      res <- map(
        .x = name,
        .f = ~ self$variables[[.x]]$fetch_category_levels()
      )

      if(!concatenate) return(res)

      reduce(res, .f = c)

    },

    get_category_labels = function(name, concatenate = TRUE){

      res <- map(
        .x = name,
        .f = ~ self$variables[[.x]]$fetch_category_labels()
      )

      if(!concatenate) return(res)

      reduce(res, .f = c)

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
      names(keep(self$variables, ~!is.null(.x$units)))
    },

    get_names_with_divby = function(){
      names(keep(self$variables, ~!is.null(.x$divby_modeling)))
    },

    get_name_translater = function(name = NULL,
                                   units = 'none',
                                   quiet = FALSE){

      assert_character(name, null.ok = TRUE)

      .name <- name %||% names(self$variables)

      for(i in seq_along(.name)){
        assert_choice(.name[i],
                      .var.name = .name[i],
                      choices = names(self$variables))
      }

      output <- map(
        .x = set_names(.name),
        .f = ~ self$get_label(.x, units)
      ) |>
        compact()

      unlabeled <- setdiff(.name, names(output))

      if(!is_empty(unlabeled)){

        if(!quiet){
          cli_warn("Incomplete translate information for {.val {unlabeled}}: labels are missing.")
        }

      }

      output

    },

    get_category_translater = function(name = NULL, quiet = FALSE){

      assert_character(name, null.ok = TRUE)

      choices <- self$variables |>
        map_lgl(~.x$type == "Nominal") |>
        which() |>
        names()

      # give back all the translaters if no name is specified
      name <- name %||% choices

      output <- character(length = 0L)

      for(i in seq_along(name)){

        assert_choice(name[i], choices = choices)

        .labs <- self$variables[[ name[i] ]]$category_labels
        .lvls <- self$variables[[ name[i] ]]$category_levels

        if(is.null(.labs)){

          if(!quiet){
            cli_warn("Translate information for variable {.val {name[i]}} is incomplete: labels are missing.")
          }

          next

        }

        output <- append(
          output,
          values = set_names(
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

    get_acronyms = function(){
      self$acronyms
    },

    set_variable_order = function(...,
                                  .before,
                                  .after){

      null_before <- quo_is_null(.before)
      null_after <- quo_is_null(.after)

      if(!null_before && !null_after) {
        cli_warn(c(
          "Only one of {.arg .before} and {.arg .after} can be specified.",
          "i" = "The {.arg .after} input will be ignored."
        ))
      }

      tmp_data <- matrix(data = 0,
                         nrow = 1,
                         ncol = length(self$variables),
                         dimnames = list(rows=NULL,
                                         cols = names(self$variables))) |>
        as_tibble()

      if(!null_before){
        new_order <-
          names(relocate(tmp_data, ..., .before = !!.before))
      } else if (!null_after){
        new_order <-
          names(relocate(tmp_data, ..., .after = !!.after))
      } else {
        new_order <-
          names(relocate(tmp_data, ...))
      }


      self$variables <- self$variables[new_order]
      self$dictionary <- private$create_dictionary(self$variables)
      self$category_key <- private$create_category_key(self$variables)
      self

    },

    index_rows = function(data, names = 'name', levels = 'level'){

      name_sort <- factor(data[[names]],
                          levels = union(self$dictionary$name,
                                         unique(data[[names]])))

      split(data, f = name_sort, drop = FALSE) |>
        imap_dfr(
          .f = ~ {

            out <- .x

            if(.y %in% self$get_names()){

              if(self$variables[[.y]]$type == "Nominal"){

                .levels <- self$variables[[.y]]$get_category_levels()

                out <- .x |>
                  arrange(
                    factor(.data[[levels]], levels = .levels)
                  )

              }

            }

            out

          }

        )

    },

    index_columns = function(data, keep_unmatched = TRUE) {

      cols <- private$split_columns(data)

      selected <- if (keep_unmatched) {
        c(cols$matched, cols$unmatched)
      } else {
        cols$matched
      }

      select(data, all_of(selected))

    },

    translate_data = function(x, .cols,
                              units,
                              warn_unmatched,
                              apply_variable_labels,
                              apply_category_labels,
                              nominals_to_factor,
                              drop_unused_levels){

      overlapping_variables <- self |>
        infer_overlapping_variables(x[.cols], warn_unmatched)

      unit_variables <- overlapping_variables |>
        intersect(self$get_names_with_units())

      nominal_variables <- overlapping_variables |>
        intersect(self$get_names_nominal())

      for(i in overlapping_variables){

        .label <- self$get_label(i)

        if(i %in% unit_variables && units != 'none'){

          .label <- .label %||% i
          .unit <- self$get_units(i)
          .divby <- self$get_divby(i) %||% 1
          # variables without divby need to say per 1 unit for consistency

          if(units == 'model'){

            x[[i]] <- x[[i]] / .divby
            .label <- paste0(.label, ", per ", .divby, " ", .unit)

          } else if(units == 'descriptive') {

            .label <- paste0(.label, ", ", .unit)

          }

        }

        if(i %in% nominal_variables){

          if(apply_category_labels){
            x[[i]] <- self$translate_categories(x[[i]],
                                                names = i,
                                                to_factor = nominals_to_factor,
                                                drop_unused_levels = drop_unused_levels)
          } else if (nominals_to_factor) {
            x[[i]] <- factor(x[[i]],
                             levels = self$variables[[i]]$get_category_levels())
          }

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
                               warn_unmatched = TRUE,
                               allow_duplicates = FALSE,
                               drop_unused_levels = FALSE){

      x_uni <- unique(na.omit(x))

      .list <- .list %||% list(...)

      translater <- self$get_name_translater(quiet = TRUE, units = units)

      if(!is_empty(.list)) translater <- c(translater, .list)

      unmatched <- setdiff(x_uni, names(translater))

      if(!is_empty(unmatched) && warn_unmatched){
        cli_warn(c(
          "i" = "Unique values in {.arg x} could not be matched with labels in {.arg dictionary}:",
          set_names(unmatched, "i"),
          "i" = "To disable this warning, set {.code warn_unmatched = FALSE} in {.fn translate}."
        ))
      }

      overlapped <- intersect(x_uni, names(translater))

      dups <- translater[duplicated(translater[overlapped])]

      if(!is_empty(dups)){

        if(!allow_duplicates){
          dups_explained <- dups |>
            imap_chr(
              \(.x, .y) names(translater)[translater == .x] |>
                paste_quotes() |>
                paste_collapse() |>
                (\(s) paste("The label", paste_quotes(.x), "is being assigned to", s))()
            ) |>
            set_names(nm = "x")

          cli_abort(
            message = c(
              "Detected one-to-many relationship between labels and names",
              dups_explained,
              "i" = "Set {.code allow_duplicates = TRUE} to bypass, but this may cause other perinary functions to fail unexpectedly",
              "v" = "Modify labels with {.fn set_labels} or remove one of the duplicated names from {.arg x} to fix."
            ),
            call = NULL
          )
        }

      }

      if(to_factor){
        private$recode_as_factor(x, translater, unmatched, drop_unused_levels)
      } else {
        private$recode_as_character(x, translater)
      }

    },

    translate_categories = function(x, ...,
                                    .list = NULL,
                                    names = NULL,
                                    to_factor = FALSE,
                                    warn_unmatched = TRUE,
                                    drop_unused_levels = FALSE){

      .list <- .list %||% list(...)

      if(!is.null(names)){

        if(length(names) == 1) names <- rep(names, length(x))

        if (length(names) != length(x)) {
          cli_abort("{.arg names} must have the same length as {.arg x}.")
        }

        single_name <- length(unique(names)) == 1

        if(!single_name && to_factor){

          cli_warn(
            message = c(
              "Detected more than one unique value in {.var name} with {.var to_factor = TRUE}",
              i = "{.var translate_categories()} does not concatenate factor levels across multiple variables",
              i = "output will be returned with type {.var character} instead of {.var factor}"
            )
          )

          to_factor <- FALSE

        }

        return(
          tibble(x = x, name = names) |>
            group_by(name) |>
            nest(data = c(x)) |>
            mutate(
              out = map2(
                .x = data,
                .y = name,
                .f = ~ {

                  if(.y %in% self$get_names()){

                    translater <- self$get_category_translater(
                      name = .y,
                      quiet = TRUE
                    ) |>
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

                  unmatched <- unique(na.omit(.x$x)) |>
                    setdiff(names(translater))

                  if(to_factor){
                    private$recode_as_factor(
                      .x$x, translater, unmatched, drop_unused_levels
                    )
                  } else {
                    private$recode_as_character(.x$x, translater)
                  }

                }
              )
            ) |>
            unnest(cols = c(data, out)) |>
            pull(out)
        )

      }

      x_uni <- unique(na.omit(x))

      translater <-
        self$get_category_translater(name = names, quiet = TRUE) |>
        private$bind_list_to_translater(.list = .list)

      dup_problems <- private$find_dup_problems(translater, x)

      if(!is_empty(dup_problems)){

        dups_explained <- dup_problems |>
          map_chr(
            \(.x) paste0("'", translater[names(translater) %in% .x], "'") |>
              paste(collapse = ' and ') |>
              (\(s) paste0("The category '", .x, "' maps to labels of ", s))()
          ) |>
          set_names(nm = 'x')

        cli_abort(
          message = c(
            "Detected one-to-many relationship between categories and labels",
            dups_explained,
            "v" = "Use `names` to clarify your mapping (see examples in `?translate_categories`)"
          ),
          call = NULL
        )

      }

      # don't let these be recoded to NA if returning output as factor
      unmatched <- x_uni |>
        setdiff(names(translater))

      if(to_factor){
        # if a dictionary has 2 or more variables that share the same levels and
        # labels, you can have the odd situation where the same label/level pair
        # appear more than once in the translater vector. Calling factor() with
        # a duplicated level will throw an error, so the translater is filtered
        # here by removing any items that have the exact same level and label.
        dup_levels <- duplicated(names(translater))
        dup_labels <- duplicated(translater)
        translater <- translater[!(dup_labels & dup_levels)]

        private$recode_as_factor(x, translater, unmatched, drop_unused_levels)
      } else {
        private$recode_as_character(x, translater)
      }

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

      assert_character(names(key),
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
        map(value, .f = ~.x$clone(deep=TRUE))
      } else {
        value
      }
    },

    get_types = function() {
      map_chr(self$variables, 'type')
    },

    get_names_by_type = function(type) {
      names(self$variables)[private$get_types() == type]
    },

    # Function to create tibble summary of variables
    create_dictionary = function(vars) {

      tibble(
        name            = map_chr(vars, ~ .x$fmt_name()),
        type            = map_chr(vars, ~ .x$fmt_type()),
        label           = map_chr(vars, ~ .x$fmt_label()),
        description     = map_chr(vars, ~ .x$fmt_description()),
        units           = map_chr(vars, ~ .x$fmt_units()),
        divby_modeling  = map_chr(vars, ~ .x$fmt_divby_modeling()),
        category_levels = map_chr(vars, ~ .x$fmt_category_levels()),
        category_labels = map_chr(vars, ~ .x$fmt_category_labels())
      )

    },

    create_category_key = function(vars){

      enframe(vars, name = 'name') |>
        mutate(type = map_chr(value, "type")) |>
        filter(type == "Nominal") |>
        mutate(level = map(value, ~.x$category_levels),
               label = map(value, ~.x$category_labels)) |>
        select(name, level, label) |>
        unnest(cols = c(level, label)) |>
        group_by(name) |>
        mutate(reference = level == first(level)) |>
        ungroup()

    },

    check_inputs_match = function(key, dictionary_name = NULL){

      unmatched_inputs <- setdiff(names(key), self$dictionary$name)

      if(!is_empty(unmatched_inputs)){

        n <- length(unmatched_inputs)
        nm <- paste_collapse(unmatched_inputs)
        dd <- paste(dictionary_name %||% "dictionary",
                    "get_names()",
                    sep = "$")

        cli_abort(
          c(
            "Invalid names in {.var ...} or {.var .list}",
            "i" = paste("There {?is/are} {n} unrecognized name{?s}:", nm),
            "x" = "Input names must match dictionary variable names.",
            ">" = "Use `{dd}` to see valid names"
          )
        )

      }

    },

    check_inputs_unique = function(key) {
      assert_inputs_unique(names(key))
    },

    # Recode a character/factor vector using a named translater.
    # Names of translater are old values; values are new values.
    # Unmatched elements are kept as-is.
    # translater may be a named list or named character vector.
    recode_with_translater = function(x, translater) {
      translater <- unlist(translater)  # coerce list-form translaters
      result <- as.character(x)
      hits <- match(result, names(translater))
      result[!is.na(hits)] <- translater[hits[!is.na(hits)]]
      result
    },

    recode_as_factor = function(x,
                                translater,
                                unmatched = NULL,
                                drop_unused_levels = FALSE){

      # a duplicate level can occasionally squeak in but only if
      # it is explicitly allowed by the user or if it is harmless
      x <- private$recode_with_translater(x, translater) |>
        factor(levels = unique(c(translater, unmatched)))

      if (drop_unused_levels) x <- droplevels(x)

      x

    },

    recode_as_character = function(x, translater){
      private$recode_with_translater(x, translater)
    },

    # Identify translater keys that map to multiple labels and are present in x.
    # Returns a character vector of problematic level names.
    find_dup_problems = function(translater, x) {
      dup_levels <- duplicated(names(translater))
      dup_labels <- duplicated(translater)
      # A level mapping to multiple labels is a problem;
      # multiple levels mapping to the same label is fine.
      names(translater)[dup_levels] |>
        setdiff(names(translater)[dup_labels]) |>
        intersect(x)
    },

    bind_list_to_translater = function(translater, .list,
                                       add_replacements = TRUE,
                                       add_leftovers = TRUE){
      if (is_empty(.list)) return(translater)
      .vec <- unlist(.list)
      in_trans <- names(.vec) %in% names(translater)
      if (add_replacements) translater[names(.vec)[ in_trans]] <- .vec[ in_trans]
      if (add_leftovers)    translater <- c(translater, .vec[!in_trans])
      translater
    },

    # Returns a list with two character vectors: `matched` (data columns present
    # in the dictionary, in dictionary order) and `unmatched` (data columns
    # absent from the dictionary, in their original order).
    split_columns = function(data) {
      list(
        matched   = intersect(self$get_names(), names(data)),
        unmatched = setdiff(names(data), self$get_names())
      )
    }

  )

)

