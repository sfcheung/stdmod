# The 'add1' Method for a 'std_selected' Class Object

Intercept the [`add1()`](https://rdrr.io/r/stats/add1.html) method and
raise an error.

## Usage

``` r
# S3 method for class 'std_selected'
add1(object, ...)
```

## Arguments

- object:

  The output of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  or
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).

- ...:

  Additional arguments. They will be ignored.

## Value

It returns nothing. It is called for its side effect.

## Details

[`add1()`](https://rdrr.io/r/stats/add1.html) should not be used after
the output of [`lm()`](https://rdrr.io/r/stats/lm.html) is processed by
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
or
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
