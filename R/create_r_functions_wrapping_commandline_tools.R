document_roxygen_param_info <- function(paramater_name, description, type){
  param_string = paste0("#' @param ", paramater_name," ", description, " (", type, ")")
  return(param_string)
}

template_function <- function(){
  command = "mosdepth --threads threads <d4;--d4> prefix bam_or_cram"
}


#' R6 Class representing a commandline tool
#'
#' This class is designed for automating creation of R functions wrapping commandline tools
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Step 1: Load library
#'
#' library(utilitybeltcli)
#'
#'
#' # Step 2: Define basic components of the tool (and the R function we want to produce that wraps it)
#'
#' MosdepthWrapper <- CommandlineWrapper$new(
#'   base_command = "mosdepth",
#'   tool_title = "Mosdepth",
#'   tool_description = "simple R wrapper",
#'   function_name = "run_mosdepth"
#' )
#'
#'
#' # Step 3: describe the paramaters of the tool including:
#' #   (a) flags (–-verbose)
#' #   (b) arguments (-–threads <num_threads>)
#' #   (c) positional arguments (base_command <positional_arg>)
#'
#' MosdepthWrapper$add_arg(name = "threads", prefix = "--threads")
#' MosdepthWrapper$add_flag(name = "d4", prefix = "--d4", default = FALSE)
#' MosdepthWrapper$add_pos(name = "prefix", required = TRUE)
#' MosdepthWrapper$add_pos(name = "bam_or_cram", required = TRUE)
#'
#'
#' # Step 4: produce the R function wrapper for the tool
#' MosdepthWrapper$create_function_wrapping_commandline_tool()
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.flag
#' @importFrom clipr write_clip
#' @importFrom utilitybeltassertions fmtsuccess assert_program_exists_in_path
CommandlineWrapper <- R6::R6Class(
  classname = "CommandlineWrapper",
  public = list(
    #' @field base_command name of base command (e.g. grep) (string)
    base_command = NULL,

    #' @field cli_arg_list a list of the \strong{flags} (--verbose), \strong{arguments} (--threads <num_threads>), and \strong{positional arguments} (base_command <positional_arg>) of the commandline tool (list)
    cli_arg_list = NULL,

    #' @field function_name name of the R function that will wrap the commandline tool (string)
    function_name = NULL,

    #' @field tool_title name of the tool we are wrapping. Used only for roxygen documentation - doesn't need to match the tool specs one to one (string)
    tool_title = NULL,

    #' @field tool_description what does the commandline tool we're wrapping actually do (string)
    tool_description = NULL,

    #' Initialise CommandLineWrapperClass
    #'
    #' @param cli_arg_list cli_arg_list object
    #' @param base_command name of base command (e.g. grep) (string)
    #' @param function_name name of the R function that will wrap the commandline tool (string)
    #' @param tool_title name of the tool we are wrapping. Used only for roxygen documentation - doesn't need to match the tool specs one to one (string)
    #' @param tool_description what does the cli tool we're wrapping actually do (string)
    #'
    #' @return run for its side effects (writes a simple CLI tool wrapping R function to the clipboard)
    initialize = function(base_command, tool_title, tool_description, function_name){
      self$base_command <- base_command
      self$function_name <- function_name
      self$tool_title <- tool_title
      self$tool_description <- tool_description
    },

    #' Add Argument
    #'
    #' Adds an argument to cli_arg_list. Here, an arg is defined as something like  \strong{--threads <num_threads>}
    #'
    #' @param name name of argument/option. Does NOT need to match the commandline tools native option names (string)
    #' @param prefix prefix of the argument/option. e.g. --threads. This needs to match one of the commandline tools prefixes (string)
    #' @param default what is the default value. Specifying a non-null default here implies that the argument should always be included in the commandline. To let the user specify the argument value when running the wrapping R function, leave as null (string)
    #' @param help_message description of what this argument/option does (string)
    #'
    #' @return run for its side effects (updates objects cli_arg_list)
    add_arg = function(name, prefix, default="NULL", help_message = "<no help message available>"){
      self$cli_arg_list[[name]] <- list(
        type = "arg",
        name = name,
        prefix = prefix,
        default = default,
        help_message = help_message
      )
    },

    #' Add Positional Argument
    #'
    #' Adds a position argument to cli_arg_list. Here, a positional argument is defined as something like  \strong{base_command <positional_argument_1>}
    #'
    #' @param name name of argument/option. Does NOT need to match the commandline tools native option names (string)
    #' @param required is this positional argument required (bool)
    #' @param help_message description of what this argument/option does (string)
    #'
    #' @return run for its side effects (updates objects cli_arg_list)
    add_pos = function(name, required, help_message = "<no help message available>"){
      self$cli_arg_list[[name]] <- list(
        type = "pos",
        name = name,
        required = required,
        help_message = help_message
      )
    },


    #' Add flag
    #'
    #' Adds a flag to the cli_arg_list.
    #' Here, a flag is defined as something like  \strong{base_command --verbose}
    #'
    #' @param name name of argument/option. Does NOT need to match the commandline tools native option names (string)
    #' @param prefix prefix of the argument/option. e.g. --verbose. This needs to match one of the commandline tools prefixes (string)
    #' @param default what is the default value - should the flag be present/absent by default (boolean)
    #' @param help_message description of what this argument/option does (string)
    #'
    #' @return run for its side effects (updates objects cli_arg_list)
    add_flag = function(name, prefix, default=FALSE, help_message = "<no help message available>"){
      assertthat::assert_that(assertthat::is.flag(default))

      self$cli_arg_list[[name]] <- list(
        type = "flag",
        name = name,
        prefix = prefix,
        default = default,
        help_message = help_message
      )
    },

  #' Create R Function Wrapping Commandline Tool
  #'
  #' Creates an r function wrapping a commandline tool and saves it to clipboard
  #'
  #' @return Run for its side-effects (copies R function definition to clipboard)
  #'
    create_function_wrapping_commandline_tool = function(){
      cli_arg_list = self$cli_arg_list
      base_command = self$base_command
      function_name = self$function_name
      tool_title = self$tool_title
      tool_description = self$tool_description

      assertion_lines=""
      argument_line=""
      subfunctions = list()
      system_call = base_command
      roxygen_param_lines = list()

      for (argument in cli_arg_list){
        if(argument$type == "pos"){
          if(argument$required)
            argument_line = paste0(argument_line, ",", argument$name)
          else
            argument_line = paste0(argument_line, ",", argument$name, "=NULL")

          system_call = paste(system_call, paste0('<', argument$name, '>'))
          roxygen_param_lines[[argument$name]] <- document_roxygen_param_info(paramater_name = argument$name, description = argument$help_message, type = "string")
          subfunctions[argument$name] <- paste0('if(!is.null(',argument$name,')) raw_command <- sub(raw_command, pattern = "<',argument$name,'>", replacement = ',argument$name,')')
        }
        if(argument$type == "arg"){
          argument_line = paste0(argument_line, ",", argument$name, "=",argument$default)
          system_call = paste(system_call, paste0('<',argument$prefix, ";", argument$name, '>'))

          roxygen_param_lines[[argument$name]] <- document_roxygen_param_info(paramater_name = argument$name, description = argument$help_message, type = "string")
          subfunctions[argument$name] <- paste0('if(!is.null(',argument$name,')) raw_command <- sub(raw_command, pattern = "<',argument$prefix,';', argument$name, '>", replacement = paste("',argument$prefix,'",', argument$name,'))')
        }
        if(argument$type == "flag"){
          argument_line = paste0(argument_line, ",", argument$name, "=",argument$default)
          system_call = paste(system_call, paste0("<", argument$name, ";", argument$prefix ,">"))
          subfunctions[argument$name] <- paste0('if(',argument$name,') raw_command <- sub(raw_command, pattern = "<',argument$name,';(.*?)>", replacement = "\\\\1")')
          roxygen_param_lines[[argument$name]] <- document_roxygen_param_info(paramater_name = argument$name, description = argument$help_message, type = "boolean")

        }
      }

      argument_line=sub(argument_line, pattern = "^,", replacement = "")
      argument_line=gsub(argument_line, pattern = ",", replacement = ", ")
      argument_line=paste0("function(", argument_line, ")")
      argument_line=paste0(function_name," <- ", argument_line)

      roxygen_param_lines_string = paste0(unlist(roxygen_param_lines), collapse = "\n")
      substition_code_string <- paste0("\t",unlist(subfunctions), collapse = "\n")

      final_substitution <- 'raw_command = gsub(pattern = "<.*?>", replacement = "", x = raw_command, perl=TRUE)'
      # message(roxygen_param_lines_string)
      # message(argument_line)
      # message(system_call)
      # message(substition_code_string)
      program_on_path_assertion = paste0('utilitybeltassertions::assert_program_exists_in_path(program_names = "',base_command,'")')
      final_assembled_function <-
        paste0(
          "#' ",tool_title,"\n#'\n#'",tool_description,"\n#'\n",
          roxygen_param_lines_string, "\n",
          argument_line,
          '\n{\n',
          '\t',program_on_path_assertion,'\n\n',
          "\traw_command <- '", system_call, "'",
          '\n\n', substition_code_string,
          '\n\t', final_substitution,
          '\n\n\t', 'message("Running command: ", raw_command)',
          '\n\n\t', "exit_code = system(raw_command)",
          '\n\n\t', 'assertthat::assert_that(exit_code == 0, msg = paste0("Failed to run tool. Exit code [",exit_code,"]"))',
          '\n}'
        )
      message(final_assembled_function)
      message("\n\n", utilitybeltassertions::fmtsuccess("Writing function definition to clipboard"))
      clipr::write_clip(final_assembled_function, allow_non_interactive = TRUE)
    }

    )
  )

