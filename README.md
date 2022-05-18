
<!-- README.md is generated from README.Rmd. Please edit that file -->

# utilitybeltcli

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of utilitybeltcli is to …

## Installation

You can install the development version of utilitybeltcli from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("selkamand/utilitybeltcli")
```

## Example

Lets say we want to make an R function that wraps the tool ‘mosdepth’.

### Quick Start

``` r
# Step 1: Load library
library(utilitybeltcli)

# Step 2: Define basic components of the tool (and the R function we want to produce that wraps it)
MosdepthWrapper <- CommandlineWrapper$new(
  base_command = "mosdepth",
  tool_title = "Mosdepth",
  tool_description = "simple R wrapper",
  function_name = "run_mosdepth"
)

# Step 3: describe the paramaters of the tool including:
#   (a) flags (–-verbose)
#   (b) arguments (-–threads <num_threads>)
#   (c) positional arguments (base_command <positional_arg>)
MosdepthWrapper$add_arg(name = "threads", prefix = "--threads")
MosdepthWrapper$add_flag(name = "d4", prefix = "--d4", default = FALSE)
MosdepthWrapper$add_pos(name = "prefix", required = TRUE)
MosdepthWrapper$add_pos(name = "bam_or_cram", required = TRUE)

# Step 4: produce the R function wrapper for the tool
MosdepthWrapper$create_function_wrapping_commandline_tool()
#> #' Mosdepth
#> #'
#> #'simple R wrapper
#> #'
#> #' @param threads <no help message available> (string)
#> #' @param d4 <no help message available> (boolean)
#> #' @param prefix <no help message available> (string)
#> #' @param bam_or_cram <no help message available> (string)
#> run_mosdepth <- function(threads=NULL, d4=FALSE, prefix, bam_or_cram)
#> {
#>  utilitybeltassertions::assert_program_exists_in_path(program_names = "mosdepth")
#> 
#>  raw_command <- 'mosdepth --threads <threads> <d4;--d4> <prefix> <bam_or_cram>'
#> 
#>  raw_command <- sub(raw_command, pattern = "<threads>", replacement = threads)
#>  raw_command <- sub(raw_command, pattern = "<d4;(.*?)>", replacement = "\1")
#>  raw_command <- sub(raw_command, pattern = "<prefix>", replacement = prefix)
#>  raw_command <- sub(raw_command, pattern = "<bam_or_cram>", replacement = bam_or_cram)
#>  message("Running command: ", raw_command)
#> 
#>  exit_code = system(raw_command)
#> 
#>  assertthat::assert_that(exit_code == 0, msg = paste0("Failed to run tool. Exit code [",exit_code,"]"))
#> }
#> 
#> 
#> Writing function definition to clipboard
```

Once run, just paste the function in your custom package wrapping a tool
and you’re all set!
