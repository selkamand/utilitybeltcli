% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roxygen2cli.R
\name{roxygen_text_to_doc}
\alias{roxygen_text_to_doc}
\title{Clipboard To Doc}
\usage{
roxygen_text_to_doc(roxygen_string = clipr::read_clip())
}
\arguments{
\item{roxygen_string}{roxygen comments + described function (character). By default reads from clipboard.}
}
\value{
returns the content for a docopt file (string). But usually run for its side effect (cat of docopt string)
}
\description{
Take roxygen comment + function copied to clipboard and produces draft docopt file contents.
Add the following terms somewhere in your parameter descriptions to indicate their type:
}
\details{
(flags) if boolean instead of arg
(required) if argument/flag is not optional. eventually we want to automatically infer this from function definition
}
