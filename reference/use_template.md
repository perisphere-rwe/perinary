# Use a template to document a variable

This function should only be used inside of
[`set_labels()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)
or
[`set_descriptions()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md).
Calling `use_template()` in these functions will allow you to plug in
values for an existing template, which can be set in a dictionary using
`set_label_templates`

## Usage

``` r
use_template(...)
```

## Arguments

- ...:

  name-value pairs. The names must match with variables in the
  dictionary that have a template.

## Value

an environment
