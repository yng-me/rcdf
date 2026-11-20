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
#> [1] "gnRZZ8fdWiGXjJ0CrsaBKfLGju4o0lwj1IE5mbvNPEAwb2QLBfonzHTbHwf/o/lYSkaP+SRU+wyUPMoGi6iqftK6+AnOnFEmugQm/j9h5Rf7lVEy43wEfTovt/48IV5X2qTRIrBOaAp5iPsoMhYhIAYY52dImzyiJ8Nlcd+U3CJlmhDnlC/cA16j9yzM4y7S84P60CfoeLUvzGoAve0nGakD8cDtTDHLudcJbEKlZtIO2Jq0/EwEdak269Qj4Vj1c/PaNlGBAnMZMdObWmakk0xAIoxaUPQ3AcDgHVGTP3VAYlgjBCcq907HLHc1AzBz3hWjeL1Wga9tKSWOMTo0Gw=="
```
