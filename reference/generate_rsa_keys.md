# Generate RSA key pair and save to files

This function generates an RSA key pair (public and private) and saves
them to specified files.

## Usage

``` r
generate_rsa_keys(path, ..., password = NULL, which = "public", prefix = NULL)
```

## Arguments

- path:

  A character string specifying the directory path where the key files
  in `.pem` format should be saved.

- ...:

  Additional arguments passed to the
  [`openssl::rsa_keygen()`](https://jeroen.r-universe.dev/openssl/reference/keygen.html)
  function, such as key size.

- password:

  A character string specifying the password for the private key. If
  `NULL`, the private key will not be encrypted.

- which:

  A character string specifying which key to return. Can be either
  `"public"` or `"private"`. Default is `"public"`.

- prefix:

  A character string used as a prefix for the key file names. Defaults
  to `NULL`, which will result in no prefix.

## Value

A character string representing the file path of the generated key
(either public or private, based on the `which` argument).

## Examples

``` r
# Generate both public and private RSA keys and save them to the temp directory
path_to <- tempdir()
generate_rsa_keys(path = path_to, password = "securepassword")
#> [1] "/tmp/Rtmp2CaxW6/public-key.pem"
```
