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
#> [1] "h0KsP3Sg1kQVmh+O8HzAMRXBQr3sWIecoyXCB32mw5vhNCTDeczMwrZHum9PhxW4ANRSwHl75bptZ1vMRDEqmbrTbkV0K6lOT0Qgsj+F6e+eoFeF3lNuZqZojv/GA/QhhxfjJijrm1ourihLHYfiLKan5qG0zUPF7V5xAUFhtgfc2zIuSilcxyNiEJBWOoixA51jIIU3pPe8WVOttNhxctEsjAwzPikAYC987E3n5b5/8pX/yW0vxXDUZgPw7OWlcJlWyH/eHHIZyu9B7a9sxqnT1Hb2bFd6Oa88uwRAxJe4/Vs5/nb6ag4JRi9osQ2flnzIj0fWMTiXWMpioYZjdw=="
```
