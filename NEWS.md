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

## Bug fixes

* Fixed an issue where factor columns passed to `index_rows()` via the
  `names` or `levels` arguments could produce silently incorrect row
  ordering. Indexing a named lookup vector with a factor uses the factor's
  underlying integer codes rather than matching by label, which could
  scramble the sort order; affected columns are now coerced to character
  before use.
