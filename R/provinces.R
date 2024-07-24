#' @importFrom sfDR dr_provinces
#' @export
sfDR::dr_provinces


#' Clean Dominican Republic province names
#' `r lifecycle::badge("experimental")`
#'
#' This function cleans and standardizes the names of provinces in the Dominican Republic,
#' with tolerance for string similarity and options for error handling.
#'
#' @param prov Character vector of province names to be cleaned.
#' @param names Deprecated. Use `prov` instead.
#' @param .tol Numeric tolerance level for string similarity. Defaults to 0.25.
#' This parameter controls how similar two strings must be to be considered a match.
#' A lower value means stricter matching.
#' @param .on_error Character string specifying the error handling method. Defaults to "fail".
#' It can be one of the following: "fail" to stop execution on error,
#' "omit" to ignore unmatched names, or "na" to return NA for unmatched names.
#'
#' @return A cleaned character vector of province names.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic usage with province names
#'   cleaned_prov_names <- dr_clean_prov_name(c("montePlata", "Azua"))
#' }
#'
dr_clean_prov_name <- function(prov, .tol = 0.25, .on_error = "fail"){
 .do_names_cleaning(prov, sfDR::dr_provinces(.sf = FALSE, .uniques = FALSE), "PROV_ID", "PROV_NAME", .tol, .on_error) -> res
  if(.error){
    cli::cli_abort("")
  }
  res
}

#' @rdname dr_clean_prov_name
dr_prov_clean_name <- function(names){
  lifecycle::deprecate_warn("0.1.0", "dr_prov_clean_name()", "dr_clean_prov_name()")
  dr_clean_prov_name(names)
}
