
<!-- README.md is generated from README.Rmd. Please edit that file -->

# piper

<!-- badges: start -->
<!-- badges: end -->

The goal of `piper` is to allow for the construction of make-like
pipelines in R with very minimal overheads. In contrast to `targets`
(and its predecessor `drake`) which offers an opinionated pipeline
framework that demands highly functionalised code, `piper` is catholic
in approach, being adaptable to a wide range of data science workflows.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kinto-b/piper")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(piper)
make_with_source(
  dependencies = c("scratch/data/0_raw_data.csv", "scratch/lookup/concordance.csv"),
  source = c("scratch/1 data_prep.R"),
  targets = c("scratch/data/1_data.Rds")
)
make_with_recipe(
    dependencies = c("scratch/data/1_data.Rds", "scratch/data/0_pop.Rds"),
    recipe = {
      usethis::ui_info("Merging...")
    },
    targets = c("scratch/data/2_data.Rds")
)
show_pipeline()
```

<img src="man/figures/README-example_pipeline.png" width="75%" style="display: block; margin: auto;" />