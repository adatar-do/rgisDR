% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/provinces.R
\name{dr_clean_prov_name}
\alias{dr_clean_prov_name}
\alias{dr_prov_clean_name}
\title{Clean Dominican Republic province names
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}}
\usage{
dr_clean_prov_name(prov, .tol = 0.25, .on_error = "fail")

dr_prov_clean_name(names)
}
\arguments{
\item{prov}{Character vector of province names to be cleaned.}

\item{.tol}{Numeric tolerance level for string similarity. Defaults to 0.25.
This parameter controls how similar two strings must be to be considered a match.
A lower value means stricter matching.}

\item{.on_error}{Character string specifying the error handling method. Defaults to "fail".
It can be one of the following: "fail" to stop execution on error,
"omit" to ignore unmatched names, or "na" to return NA for unmatched names.}

\item{names}{Deprecated. Use \code{prov} instead.}
}
\value{
A cleaned character vector of province names.
}
\description{
This function cleans and standardizes the names of provinces in the Dominican Republic,
with tolerance for string similarity and options for error handling.
}
\examples{
\dontrun{
  # Basic usage with province names
  cleaned_prov_names <- dr_clean_prov_name(c("montePlata", "Azua"))
}

}
