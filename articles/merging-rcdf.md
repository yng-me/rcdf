# Working with multiple RCDF files

There are two common scenarios when working with multiple RCDF files:

1.  **Load several files into R at once** — useful when you want to
    analyse data from multiple files side by side. Tables with the same
    name are stacked automatically.
2.  **Merge files into a new RCDF file** — useful when you want to
    permanently combine files and share or archive the result as a
    single encrypted file.

------------------------------------------------------------------------

## Scenario 1 — Load multiple files into R

Pass a character vector of file paths (or a folder path) to
[`read_rcdf()`](https://yng-me.github.io/rcdf/reference/read_rcdf.md).
Provide matching vectors of keys and passwords if the files were
encrypted with different keys:

``` r

data <- read_rcdf(
  path = c(
    "path/to/file/01.rcdf",
    "path/to/file/02.rcdf",
    "path/to/file/03.rcdf"
  ),
  decryption_key = c(
    "keys/01-private-key.pem",
    "keys/02-private-key.pem",
    "keys/03-private-key.pem"
  ),
  password = c(
    "password01",
    "password02",
    "password03"
  )
)
```

> **Requirements for stacking to work:** - Each RCDF file must contain
> tables with the same names and column structure. - Passwords must
> correspond positionally to their decryption keys.

------------------------------------------------------------------------

## Scenario 2 — Merge files into a new RCDF file

[`merge_rcdf()`](https://yng-me.github.io/rcdf/reference/merge_rcdf.md)
reads multiple RCDF files, stacks their tables, and saves the result as
a brand-new encrypted RCDF file. This is handy for creating an archive
or distributing a combined dataset to collaborators:

``` r

merge_rcdf(
  rcdf_files = c(
    "path/to/file/01.rcdf",
    "path/to/file/02.rcdf",
    "path/to/file/03.rcdf"
  ),
  decryption_key = c(
    "keys/01-private-key.pem",
    "keys/02-private-key.pem",
    "keys/03-private-key.pem"
  ),
  password = c(
    "password01",
    "password02",
    "password03"
  ),
  merged_file_path = "path/to/merged.rcdf"
)
```

The merged file is encrypted with its own key, which you supply via the
`pub_key` argument (see
[`?merge_rcdf`](https://yng-me.github.io/rcdf/reference/merge_rcdf.md)
for all options).

------------------------------------------------------------------------

For more on reading single files and exporting data, see
[`vignette("rcdf")`](https://yng-me.github.io/rcdf/articles/rcdf.md).
