# Merge multiple RCDF files

Merge multiple RCDF files

## Usage

``` r
merge_rcdf(
  rcdf_files,
  decryption_keys,
  passwords,
  merged_file_path,
  pub_key = NULL
)
```

## Arguments

- rcdf_files:

  A character vector of RCDF file paths

- decryption_keys:

  Decryption keys associated with each RCDF file. Must match the length
  of the vector passed in the `rcdf_files` argument.

- passwords:

  Password of the associated decryption keys. Must match the length of
  `decryption_keys`.

- merged_file_path:

  File path or name of the merged RCDF file.

- pub_key:

  Public key to encrypt the merged file. If `NULL`, a new RSA key pair
  will be generated.

## Value

Invisibly, a list with `path` (the merged file) and `password` (the
password of a newly generated private key, or `NULL` when `pub_key` was
given). The password is printed only in interactive sessions, so it does
not end up in job logs.

## Examples

``` r

if (FALSE) { # \dontrun{
dir <- system.file("extdata", package = "rcdf")

rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key-pw.pem')
pw <- '1234'

temp_dir <- tempdir()

merge_rcdf(
  rcdf_files = rcdf_path,
  decryption_keys = private_key,
  passwords = pw,
  merged_file_path = file.path(temp_dir, "merged.rcdf"),
  pub_key = file.path(dir, 'sample-public-key-pw.pem')
)

unlink(file.path(temp_dir, "merged.rcdf"), force = TRUE)
} # }
```
