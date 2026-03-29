# Package index

## Creating dictionaries

Functions for creating and combining `DataDictionary` objects.

- [`data_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  : Create Data Dictionary from Variable Definitions
- [`as_data_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/as_data_dictionary.md)
  : Create Data Dictionary from Existing Data
- [`bind_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/bind_dictionary.md)
  [`left_bind()`](https://perisphere-rwe.github.io/perinary/reference/bind_dictionary.md)
  [`right_bind()`](https://perisphere-rwe.github.io/perinary/reference/bind_dictionary.md)
  : Bind Dictionaries Together

## Variable types

Constructor functions for specifying the type of each variable in a
dictionary.

- [`nominal_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
  [`numeric_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
  [`date_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
  [`identifier_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
  [`logical_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
  : Variable Constructors

## Getting information

Functions for extracting information from a `DataDictionary`.

- [`get_unknowns()`](https://perisphere-rwe.github.io/perinary/reference/get_unknowns.md)
  : Get Unknown Fields in Dictionary
- [`get_term_key()`](https://perisphere-rwe.github.io/perinary/reference/get_term_key.md)
  : Get Term Key for Nominal Variables
- [`get_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/get_dictionary.md)
  : Extract Dictionary Meta Data as a Tibble
- [`get_acronym_defs()`](https://perisphere-rwe.github.io/perinary/reference/get_acronym_defs.md)
  : Get Acronym Definitions

## Setting metadata

Functions for adding or updating metadata in a `DataDictionary`. All
functions accept a `dictionary` as their first argument and can be
chained with `|>`.

- [`set_labels()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)
  [`set_descriptions()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)
  [`set_units()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)
  [`set_divby_modeling()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)
  [`set_category_labels()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)
  [`set_category_order()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)
  : Modify Data Dictionary Elements

- [`set_variable_order()`](https://perisphere-rwe.github.io/perinary/reference/set_variable_order.md)
  : Modify order of variables in a data dictionary

- [`set_identifiers()`](https://perisphere-rwe.github.io/perinary/reference/set_identifiers.md)
  : Set Identifier Variables

- [`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md)
  :

  Set the default dictionary used by `perinary` functions

- [`set_label_templates()`](https://perisphere-rwe.github.io/perinary/reference/set_templates.md)
  [`set_description_templates()`](https://perisphere-rwe.github.io/perinary/reference/set_templates.md)
  : Set Label and Description Templates

- [`set_acronyms()`](https://perisphere-rwe.github.io/perinary/reference/set_acronyms.md)
  : Set and Remove Acronyms

## Translating data

Functions for applying a `DataDictionary` to a dataset by renaming
columns, recoding category values, or combining both operations.

- [`translate_data()`](https://perisphere-rwe.github.io/perinary/reference/translate_data.md)
  [`translate_names()`](https://perisphere-rwe.github.io/perinary/reference/translate_data.md)
  [`translate_categories()`](https://perisphere-rwe.github.io/perinary/reference/translate_data.md)
  : Translate data, names, and categories with a dictionary
- [`use_template()`](https://perisphere-rwe.github.io/perinary/reference/use_template.md)
  : Use a template to document a variable

## Indexing columns and rows

Functions for reordering rows or columns in a data frame using
dictionary metadata.

- [`index_columns()`](https://perisphere-rwe.github.io/perinary/reference/index_columns.md)
  : Order columns by dictionary variables
- [`index_rows()`](https://perisphere-rwe.github.io/perinary/reference/index_rows.md)
  : Order rows by dictionary terms
- [`index_terms()`](https://perisphere-rwe.github.io/perinary/reference/index_terms.md)
  : Order model terms by dictionary variables and category levels
