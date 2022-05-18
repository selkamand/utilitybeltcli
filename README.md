
<!-- README.md is generated from README.Rmd. Please edit that file -->

# utilitybeltcli

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of utilitybeltcli is to make it easier to create R functions
that interface with commandline tools.

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
```

Once run, just paste the function in your custom package wrapping a tool
and you’re all set!
