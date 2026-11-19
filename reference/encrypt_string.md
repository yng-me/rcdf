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
#> [1] "sujbRBlU8dBS+iDj16qaqHb6Yikxb9k7iipGoI7QTnh678z3XBHGGAInsldPo3ch4zp0cjFMi04BC4amb7JTdEZ92MK9xomu4Pk1h+P3thf/PsY0rObh9QdpUfLfUdPl6XAWwznQmxOkYYTfWZfhaejLhGLCrDxxRTB20Ty3dvd99RiOH2doOBHnhT8Je1ZimwcXitrbyApN35vTVdJa1MkcjDTM6DOpOhEzVjQTGCueInX4lMnx2CjKt7IHg8vooVWuv8wFG69KGPVcXn4ZaZQ/FYTcvT9SbjtHLDy4+ZKhZulyyWhdwu3GE+GrhFhV8aFzodYzMEfnll+8xQSa5A=="
```
