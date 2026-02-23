# Working with multiple RCDF files

[`read_rcdf()`](https://yng-me.github.io/rcdf/reference/read_rcdf.md)
function allows you to read multiple RCDF files and load them into R
environment as one object. This will only work, however, if you have the

- Each RCDF file have the same data structure
- All RDCF files have the same decryption key and its associated
  password

``` r
data <- read_rcdf(
  path = c(
    'path/to/file/01.rcdf',
    'path/to/file/02.rcdf',
    'path/to/file/03.rcdf'
  ),
  decryption_key = c(
    'playground/keys/01-private-key.pem',
    'playground/keys/02-private-key.pem',
    'playground/keys/03-private-key.pem'
  ),
  password = c(
    'password01',
    'password02',
    'password03'
  )
)
```

Alternatively, you can merge

``` r
merge_rcdf(
  rcdf_files = c(
    'path/to/file/01.rcdf',
    'path/to/file/02.rcdf',
    'path/to/file/03.rcdf'
  ),
  decryption_key = c(
    'playground/keys/01-private-key.pem',
    'playground/keys/02-private-key.pem',
    'playground/keys/03-private-key.pem'
  ),
  password = c(
    'password01',
    'password02',
    'password03'
  )
  merged_file_path = 'path/to/merged.rcdf'
)
```
