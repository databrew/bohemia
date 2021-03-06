% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data_dictionary.R
\name{generate_data_dictionary}
\alias{generate_data_dictionary}
\title{Generate data dictionary}
\usage{
generate_data_dictionary(
  path,
  language = "English",
  include_variable_names = FALSE,
  include_relevant = TRUE,
  shorten_many = 15,
  choice_names_too = FALSE,
  invisibilize = FALSE
)
}
\arguments{
\item{language}{Language (default English)}

\item{include_variable_names}{Whether to include variable names}

\item{include_relevant}{Whether to include relevance}

\item{shorten_many}{At what number of choices should "etc" appear (instead of further choices)}

\item{invisibilize}{Whethr to invisiblize repeated rows in the question/type columns}

\item{choices_names_too}{Whether to include choice names too}
}
\value{
A data dictionary
}
\description{
Generate a data dictionary from a XLSform
}
