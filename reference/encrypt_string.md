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
#> [1] "RtWvbcwMJbphUpOwHAxzZhY8Hf1rgaj0foAKAT+Ifq3nbMywojVkmsq95JQH9JODgktxZnS2EzYjGSNeMWzUxI5juotEbcGaVzk1D9Va9PNHsxBDITfyBMAl2yfnrhvg+jNn7t2jQJwTsVDMKJFLGmm2FLpQmnRjfq2Lzxip6zISa69NQaV4AY2mkZFOVp9tTCqnSiJ6xlai+vJlIHGk3/2b7A7PQq03gnrctZ31jXxQUmIOSgXJ7FkYDVMBdOEk+7v0gHDq0EN3eW7kAWrd31cs2osMZnf6c+Vp+yJx8J91mSx3Yl+2rRxH59nOOgr6c7QSYuwesYgqrTxl/So6hQ=="
```
