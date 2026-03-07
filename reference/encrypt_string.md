# Encrypt string using RSA

Encrypt string using RSA

## Usage

``` r
encrypt_string(x, pub_key)
```

## Arguments

- x:

  A character of length 1

- pub_key:

  A public key object or .pem file

## Value

Encrypted base64-encoded string

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
pub_key <- file.path(dir, 'sample-public-key.pem')
encrypt_string('hello', pub_key)
#> [1] "FipIcE0LRE1G6Yrz1jSZNmbmxvGRajlIAH2RNau1cA2uopeBYtZ9oZgOOsc0q0MHT0r5WqkCIFOw+a2YyWy6Z/IbhmPbZhO02XTXkp76kwjRwVnDbZT2IAYetdC2t3HQv5qdUZ4oLXogpCKUTk1ATJ+s6VOeeJj36AW71VLDAJlj7SvNuGSA9UgRN/Ib5JDA4rZSeqLodtprkiyfq7E07ojHUEoBfEagLaEqK/DAhtG/fQicQ/2xB8Od4uVoyUOeRGfxU2U8B8qxhYhy32Y0lC2wN/dNf2ua5Z4FgyjzdBC5GTdRHVGYsHEHidc4WM7mhOqexHfmbKuLIw/vzWNwkw=="
```
