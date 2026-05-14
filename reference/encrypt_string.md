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
#> [1] "aAlxjCkJgyoSjxTkonndEHfD5qU5gAnvio/jfg4cMby6Z1zEpKrnf85CoXAK7xOUhiKh65gtkL1Z62mXIO4n+M0nm/RpIPBcqG21t1zijr+4tGnHnCsgpsJAqmQ3NAyAAHp1KLm0e3kzmugWEC13sHsDKOyOj/G3749+Vvo0DcQcjfmwRUM1sfWLnS+qWav5dPSAvrtqnvpE6dh5UadfbSo/mM42akNmxbBgc71z2Bps073hn0g6/w+8qdErUBe6dyifHiXDEJKTA2eW1ox4BU1zc5bG9V5369oqhXEt5cfrf1W2lbNpYjCuiLzq6Q3bY+rWl/x7V3EP7x93Ort3+g=="
```
