# Read environment variables from a file

Based on https://github.com/gaborcsardi/dotenv

## Usage

``` r
read_dot_env(path = ".env")
```

## Arguments

- path:

  A string specifying the path to the `.env` file. If not provided,
  defaults to `.env` in the current working directory.

## Value

A named list of environment variables. Each element is a key-value pair
extracted from the file. If no variables are found, `NULL` is returned.

## Details

Reads a `.env` file containing environment variables in the format
`KEY=VALUE`, and returns them as a named list. Lines starting with `#`
are considered comments and ignored.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming an `.env` file with the following content:
# DB_HOST=localhost
# DB_USER=root
# DB_PASS="secret"

env_vars <- read_dot_env(".env")
print(env_vars)
# Should output something like:
# $DB_HOST
# [1] "localhost"

# If no path is given, it defaults to `.env` in the current directory.
env_vars <- read_dot_env()
} # }
```
