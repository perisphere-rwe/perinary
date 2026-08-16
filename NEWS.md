# perinary 0.1.0

## Breaking changes

* `index_rows()` (and therefore `index_terms()`) no longer sorts rows for
  names unmatched by the dictionary to the end of the output. Unmatched
  values (e.g. `"(Intercept)"` in model output) now stay anchored at their
  original position in the row order, while variables found in the
  dictionary are still resorted into dictionary order around them.

## New features

* `index_rows()` can now order rows using category labels and variable
  labels, not just category levels and variable names. Values in the
  `names` and `levels` columns may be a mix of both representations. If a
  name/level and a label happen to collide (the same string is used for
  both), the name/level match takes priority.

* New `select_variables()` keeps, drops, or reorders variables in a
  `DataDictionary` using tidyselect semantics, similar to `dplyr::select()`
  for data frames. This is useful for narrowing a dictionary to the
  variables relevant to a dataset, or for resolving a label collision
  (e.g. dropping one of two variables that share a label) before calling
  [index_rows()].

## Bug fixes

* Fixed an issue where factor columns passed to `index_rows()` via the
  `names` or `levels` arguments could produce silently incorrect row
  ordering. Indexing a named lookup vector with a factor uses the factor's
  underlying integer codes rather than matching by label, which could
  scramble the sort order; affected columns are now coerced to character
  before use.

* Fixed a silent misattribution issue in `index_rows()`: if two or more
  variables shared the same label, rows referring to that label by name
  could be attributed to whichever variable was declared first in the
  dictionary, potentially applying the wrong variable's category order.
  A label shared by multiple variables is no longer used to identify a
  variable; a warning is issued and matching values are treated as
  unmatched (left in place) instead.
