# Internal paste helpers

Small utilities to format vectors/lists for readable messages and logs.
Used throughout `perinary` to make assertion and warning text concise
and human-friendly.

- **`paste_collapse()`**: collapse a vector with commas and a natural
  language conjunction before the last element (e.g., `"a, b, and c"`).

- **`paste_named_vec()`**: format a (named) vector or list as
  `"name = value"` pairs (lists are rendered as
  `name = c(v1, v2, ...)`), filling in missing/empty names with a
  sentinel.

## Usage

``` r
paste_collapse(x, as_code = FALSE)

paste_named_vec(x, name_fill = "MISSING_NAME")
```

## Arguments

- x:

  A vector (character, numeric, etc.) to collapse. `NULL` returns
  `"none"`. Length-1 values are coerced to character and returned
  unchanged.

- name_fill:

  Character scalar used to replace missing or empty names when
  formatting (default `"MISSING_NAME"`).

## Value

`paste_collapse()` returns a single character scalar with elements of
`x` collapsed by `", "` and a natural `"and"` before the last item.

`paste_named_vec()` returns:

- If `x` is a **list**: a named character vector, where each element is
  of the form `"name = c(v1, v2, ...)"`. Each element name corresponds
  to the original list element name (or `name_fill` if missing/empty).

- If `x` is an **atomic vector**: a single character scalar of the form
  `"name1 = value1, name2 = value2, ..."`.

## Conventions

- `NULL` inputs are rendered as `"none"` to avoid noisy `NULL` prints.

- The last separator is `" and "` for length 2, otherwise `", and "`.

- Empty or missing names are replaced with `name_fill` (default
  `"MISSING_NAME"`).
