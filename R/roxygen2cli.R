
#' My Test Function
#'
#' This is a simple oneline description
#'
#' @param path path argument
#' @param verbose should we print messages? (flag)
#' @param one_of choice of hi, hello, or hiya (required)
#'
#' @return some boolean
#'
test_function <- function(path, verbose, one_of = c("hi", "hello", "hiya")) {

}


#' Clipboard To Doc
#'
#' Take roxygen comment + function copied to clipboard and produces draft docopt file contents.
#' Add the following terms somewhere in your parameter descriptions to indicate their type:
#'
#' (flags) if boolean instead of arg
#' (required) if argument/flag is not optional. eventually we want to automatically infer this from function definition
#'
#' @param roxygen_string roxygen comments + described function (character). By default reads from clipboard.
#'
#' @return returns the content for a docopt file (string). But usually run for its side effect (cat of docopt string)
#' @export
#'
roxygen_text_to_doc <- function(roxygen_string = clipr::read_clip()) {

  full_docstring <- paste0(roxygen_string, collapse = "\n")

  # Add tests to make sure input is a roxygen text
  tryCatch(
    expr = {
      roxy_block_list <- roxygen2::parse_text(full_docstring)
    },
    error = function(err) {
      stop(
        fmt::fmterror(
          "Roxygen Parsing Error\n\nText on Clipboard: \n", full_docstring,
          "\n-------------------------------------",
          "\n\nFailed to parse roxygen object from clipboard.",
          "\nMake sure you copy not only the roxygen comment lines, but also the full function."
        )
      )
    },
    warning = function(warn) {

    }
  )

  if (length(roxy_block_list) == 0) {
    stop(
      fmt::fmterror(
        "Roxygen Parsing Error\n\nText on Clipboard: \n", full_docstring,
        "\n-------------------------------------",
        "\n\nCould not find any roxygen blocks. Make copy a full roxygen comment + function to the clipboard"
      )
    )
  }

  if (length(roxy_block_list) != 1) {
    stop(
      fmt::fmterror(
        "Roxygen Parsing Error\n\nText on Clipboard: \n", full_docstring,
        "\n-------------------------------------",
        "\n\nExpected 1 roxygen blocks. Got ", length(roxy_block_list)
      )
    )
  }

  roxy_block <- roxy_block_list[[1]]



  results <- list(
    title = roxygen2::block_get_tag(roxy_block, tag = "title"),
    param = roxygen2::block_get_tags(roxy_block, tag = "param"),
    value = roxygen2::block_get_tag(roxy_block, tag = "return"),
    description = roxygen2::block_get_tag(roxy_block, tag = "description")
  )

  lapply(seq_along(results), function(i) {
    tag <- names(results)[i]
    value <- results[[i]]
    if (is.null(value)) {
      stop(fmt::fmterror("Could not find tag: ", tag, "\n Try adding an '@", tag, "' tag entry to the roxygen comment"))
    }
  })


  # Make sure params have all required info

  roxygen_params <- results[["param"]]

  fmt::message_info(
    "Found [", length(roxygen_params), "] paramaters "
  )

  paramater_information_ls <- lapply(seq_along(roxygen_params), FUN = function(i) {
    param_name <- roxygen_params[[i]]$val$name
    param_description <- roxygen_params[[i]]$val$description

    assertthat::assert_that(
      nchar(gsub(param_description, pattern = " ", replacement = "")) > 0,
      msg = fmt::fmterror("Empty description for paramater: ", param_name)
    )


    return(
      c("name" = param_name,
        "description" = param_description,
        "type" = is_param_flag_or_arg(param_description),
        "required" = is_param_required(param_description))
    )
  })
  param_df = do.call(rbind.data.frame, paramater_information_ls)
  names(param_df) <- c("Name", "Description", "Type", "Required")
  param_df[["Required"]] <- as.logical(param_df[["Required"]])

  paramater_cli_strings <- paste0("--", param_df$Name, ifelse(param_df$Type=="arg", "=<arg>", ""))
  paramater_cli_strings_with_optionals_wrapped <- ifelse(!param_df$Required, yes = paste0("[", paramater_cli_strings ,"]"), no=paramater_cli_strings)

  paramater_cli_strings_option_section <- paste0(
  "Options:\n",
  paste0(
    "  ", paramater_cli_strings, "  ", param_df[["Description"]], collapse = "\n"
    )
  )

  title = results$title$val
  title_no_spaces = gsub(x=title,pattern = " ", replacement = "_")
  description = results$description$val
  usage = paste0(
    title_no_spaces, " ",paste0(paramater_cli_strings_with_optionals_wrapped, collapse = " ")
    )

  docopt <- create_docopt_file(title=title, description = description, usage = usage, options =paramater_cli_strings_option_section)
  cat(docopt)
  return(docopt)
}

#' Is paramater a flag / arg based on param_description
#'
#' If paramater description contains '(flag)' or '(boolean)' or '(bool)' assume argument is a flag
#' otherwise assume is an argument
#'
#' @param param_description paramater description (character)
#'
#' @return "flag" for flag arguments and 'arg' for non-flag args (character)
#'
is_param_flag_or_arg <- function(param_description){
  ifelse(
    test = grepl(x = param_description, pattern = "\\(flag\\)|\\(boolean\\)|\\(bool\\)"),
    yes = "flag",
    no = "arg"
  )
}

#' Is paramater required
#'
#' @param param_description paramater description (character)
#'
#' @return TRUE if paramater description contains '(required)', false if not
#'
#'
is_param_required <- function(param_description){
  grepl(x = param_description, pattern = "\\(required\\)")
}

create_docopt_file <- function(shebang = "#!/usr/bin/env Rscript", title = "MyTitle", description = "Some Description", usage = c("MyCommand subcommand --required=arg [--optional_flag]", "MyCommand --required=arg <positional_input>"), options = "Options:\n") {
  usage_single_string <- paste0(usage, collapse = "\n")

  vec_docfile_contents <- paste0(
    shebang, "\n",
    '"', "\n",
    title, ". ",
    description, "\n\n",
    "Usage: ", "\n",
    usage_single_string, "\n\n",
    options, "\n",
    '" -> doc', "\n", "\n",
    "library(docopt)", "\n",
    "arguments <- docopt(doc)"
  )
  #cat(vec_docfile_contents)

  return(vec_docfile_contents)
}
