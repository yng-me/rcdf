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

`NULL` (void)

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")

rcdf_path <- file.path(dir, 'mtcars-pw.rcdf')
private_key <- file.path(dir, 'sample-private-key-pw.pem')
pw <- '1234'

temp_dir <- tempdir()

data <- merge_rcdf(
  rcdf_files = rcdf_path,
  decryption_keys = private_key,
  passwords = pw,
  merged_file_path = file.path(temp_dir, "merged.rcdf")
)
#> ℹ Generatd new RSA keys in: /tmp/RtmpFEqr1r
#> ℹ Password for decryption key: 1BdCrIIbrefDNOTpxd4$NMgc%)wOTvGs

unlink(temp_dir, force = TRUE)
```
