# Extract metadata from an RCDF file

Retrieves a specific metadata value from a `.rcdf` file.

## Usage

``` r
get_rcdf_metadata(path, key)
```

## Arguments

- path:

  Character string. The file path to the `.rcdf` file.

- key:

  Character string. The metadata key to extract from the file.

## Value

The value associated with the specified metadata key, or `NULL` if the
key does not exist.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming "example.rcdf" is a valid RCDF file in the working directory:
get_rcdf_metadata("example.rcdf", "creation_date")
} # }
```
