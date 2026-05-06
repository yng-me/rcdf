# Getting Started with rcdf

## What is RCDF?

Think of an RCDF file as a **locked cabinet** for your data. You can
store multiple datasets inside it, and the cabinet can only be opened
with the right key. When you share the file with a colleague, they
cannot see the contents unless you also give them a copy of the key.

Under the hood, an RCDF file is a single portable archive (`.rcdf`) that
holds your datasets as compressed, encrypted files along with a small
metadata record. The encryption happens automatically — you never have
to configure it yourself.

------------------------------------------------------------------------

## Setup

``` r

library(rcdf)
```

The package ships with a sample RCDF file and matching keys so you can
try everything without creating your own files first:

``` r

sample_dir  <- system.file("extdata", package = "rcdf")
sample_rcdf <- file.path(sample_dir, "mtcars.rcdf")
sample_key  <- file.path(sample_dir, "sample-private-key.pem")
```

------------------------------------------------------------------------

## Step 1 — Create your encryption keys

Before saving data, you need a **key pair** — two files that work
together:

| File              | What it does                 | Who should keep it |
|-------------------|------------------------------|--------------------|
| `public-key.pem`  | Locks (encrypts) your data   | Safe to share      |
| `private-key.pem` | Unlocks (decrypts) your data | Keep this secret   |

Generate a new pair with one function call:

``` r

pub_key <- generate_rsa_keys(
  path     = "~/my-keys",         # folder where both files are saved
  password = "a-strong-password"  # optional, but protects the private key
)
# pub_key holds the path to the public key — you will pass it to write_rcdf()
```

> **Important:** Your private key is the only way to open your files
> later. Back it up and store it securely. A password manager or
> encrypted drive is a good choice.

------------------------------------------------------------------------

## Step 2 — Save your data

Organise your datasets into a named list, then call
[`write_rcdf()`](https://yng-me.github.io/rcdf/reference/write_rcdf.md):

``` r

# Create a container for your tables
my_data <- rcdf_list()

# Add any data frames you want to store together
my_data$households <- data.frame(id = 1:100, region = sample(c("North", "South"), 100, TRUE))
my_data$survey     <- data.frame(id = 1:100, score  = rnorm(100))

# Save everything as a single encrypted file
write_rcdf(
  data    = my_data,
  path    = "~/data/survey-2024.rcdf",
  pub_key = pub_key              # path returned by generate_rsa_keys()
)
```

This creates `survey-2024.rcdf` — a single, portable file containing all
your tables, fully encrypted and ready to share.

------------------------------------------------------------------------

## Step 3 — Read the data back

To open an RCDF file, provide the matching private key:

``` r

survey_data <- read_rcdf(
  path           = "~/data/survey-2024.rcdf",
  decryption_key = "~/my-keys/private-key.pem",
  password       = "a-strong-password"   # only needed if you set one
)

# Each table comes back as a data frame inside the list
head(survey_data$households)
head(survey_data$survey)
```

Try it right now using the bundled sample data:

``` r

sample_data <- read_rcdf(
  path           = sample_rcdf,
  decryption_key = file.path(sample_dir, "sample-private-key-pw.pem"),
  password       = "1234"
)

head(sample_data$mtcars)
```

### Reading a whole folder at once

If you have many RCDF files in one folder, load them all in one call.
Tables that share a name across files are automatically stacked into a
single table:

``` r

all_data <- read_rcdf(
  path           = "~/data/monthly-exports/",
  decryption_key = "~/my-keys/private-key.pem",
  password       = "a-strong-password",
  recursive      = FALSE   # set TRUE to also search sub-folders
)
```

------------------------------------------------------------------------

## Step 4 — Export to other formats

Once you have the data in R, export it to whatever format your team
needs:

``` r

write_rcdf_as(
  data    = survey_data,
  path    = "~/exports/survey-2024",
  formats = c("csv", "xlsx")   # create both at once
)
# Result:
#   ~/exports/survey-2024/CSV/households.csv
#   ~/exports/survey-2024/CSV/survey.csv
#   ~/exports/survey-2024/Excel/households.xlsx
#   ~/exports/survey-2024/Excel/survey.xlsx
```

**All supported formats:**

| Format  | Argument to `formats` | Output                             |
|---------|-----------------------|------------------------------------|
| CSV     | `"csv"`               | one `.csv` per table               |
| TSV     | `"tsv"`               | one `.txt` per table               |
| JSON    | `"json"`              | one `.json` per table              |
| Parquet | `"parquet"`           | one `.parquet` per table           |
| Excel   | `"xlsx"`              | one `.xlsx` per table              |
| Stata   | `"dta"`               | one `.dta` per table               |
| SPSS    | `"sav"`               | one `.sav` per table               |
| SQLite  | `"sqlite"`            | one `.db` database with all tables |

You can also call the individual functions directly
(e.g. [`write_rcdf_csv()`](https://yng-me.github.io/rcdf/reference/write_rcdf_csv.md),
[`write_rcdf_xlsx()`](https://yng-me.github.io/rcdf/reference/write_rcdf_xlsx.md))
if you need more control. Run
[`?write_rcdf_as`](https://yng-me.github.io/rcdf/reference/write_rcdf_as.md)
to see all options.

------------------------------------------------------------------------

## Checking a file without decrypting it

You can inspect when an RCDF file was created, which package version
wrote it, and integrity checksums — all without the private key:

``` r

meta <- get_attrs(sample_rcdf)
meta$created_at   # when the file was created
meta$version      # package version used to create it
meta$checksum     # per-table checksums for integrity verification
```

------------------------------------------------------------------------

## Next steps

- **Working with multiple files** — see
  [`vignette("merging-rcdf")`](https://yng-me.github.io/rcdf/articles/merging-rcdf.md)
  for loading or combining RCDF files from different sources.
- **Full function reference** — run
  [`?write_rcdf`](https://yng-me.github.io/rcdf/reference/write_rcdf.md),
  [`?read_rcdf`](https://yng-me.github.io/rcdf/reference/read_rcdf.md),
  or
  [`?write_rcdf_as`](https://yng-me.github.io/rcdf/reference/write_rcdf_as.md)
  in the R console for all available options.
