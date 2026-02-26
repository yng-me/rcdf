# Get metadata attribute from RCDF data

Get metadata attribute from RCDF data

## Usage

``` r
get_attr(rcdf, key)
```

## Arguments

- rcdf:

  RCDF data

- key:

  Valid metadata key.

## Value

RCDF attribute/s or NULL

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming `df` is a valid RCDF object

get_attr(df, "area_names")

# To get nested attributes
get_attr(df, "meta.source_note")
} # }
```
