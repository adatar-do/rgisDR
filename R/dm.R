#' @importFrom sfDR dr_municipal_districts
#' @export
sfDR::dr_municipal_districts


.error <- FALSE

#' Clean Dominican Republic municipal district names
#' `r lifecycle::badge("experimental")`
#'
#' This function cleans and standardizes the names of municipal districts (distritos municipales)
#' in the Dominican Republic. It can also handle related names for municipalities
#' and provinces, with tolerance for string similarity and options for error handling.
#'
#' @param md Character vector of municipal district names to be cleaned.
#' @param .mun Optional character vector of municipality names. Defaults to NULL.
#' If provided, it will clean and match the municipal district names within the
#' context of these municipalities.
#' @param .prov Optional character vector of province names. Defaults to NULL.
#' If provided, it will clean and match the municipal district names within the
#' context of these provinces.
#' @param .tol Numeric tolerance level for string similarity. Defaults to 0.33.
#' This parameter controls how similar two strings must be to be considered a match.
#' A lower value means stricter matching.
#' @param .on_error Character string specifying the error handling method. Defaults to "fail".
#' It can be one of the following: "fail" to stop execution on error,
#' "omit" to ignore unmatched names, or "na" to return NA for unmatched names.
#'
#' @return A cleaned character vector of municipal district names.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with municipal district names only
#' cleaned_md_names <- dr_clean_md_name(c("Distrito Nacion...", "Azua"))
#'
#' # Usage with municipality names
#' cleaned_md_names <- dr_clean_md_name(
#'   c("DISTRITO NACIONAL", "Azua"),
#'   .mun = c("DISTRITO NACIONAL", "Azua de Compostela")
#' )
#' }
#'
dr_clean_md_name <- function(md, .mun = NULL, .prov = NULL, .tol = 0.33, .on_error = "fail") {
  dm <- md
  MD <- NULL
  dm2 <- NULL
  dm3 <- NULL
  MUN <- NULL
  MUN_ID <- NULL
  MUN_NAME <- NULL
  mun2 <- NULL
  . <- NULL
  orden_original <- NULL

  if (!is.null(.mun)) {
    data.frame(
      mun2 = .text_cleaning(.mun),
      dm2 = .text_cleaning(dm),
      MUN = .mun,
      MD = dm
    ) %>%
      dplyr::as_tibble() %>%
      dplyr::filter(mun2 == "_na_" & dm2 != "_na_") %>%
      dplyr::select(MUN, MD) %>%
      purrr::when(
        nrow(.) > 0 ~ {
          print(.)
          cli::cli_abort("You can't pass NA in .mun when md isn't NA.")
        },
        TRUE ~ .
      )
    dr_clean_mun_name(.mun, .prov) %>%
      data.frame(
        MUN_NAME = .
      ) %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(
        dr_municipalities(FALSE),
        by = "MUN_NAME"
      ) %>%
      dplyr::mutate(
        dplyr::across(.cols = c(MUN_NAME, MUN_ID), tidyr::replace_na, "NA"),
        dm2 = dm,
        orden_original = dplyr::row_number()
      ) %>%
      split(~MUN_ID) %>%
      purrr::map(
        \(.x, dr, tol, on_error){
          .x %>%
            dplyr::mutate(
              dm3 = .do_names_cleaning(
                dm2,
                dplyr::filter(
                  dr,
                  MUN_ID == .x$MUN_ID[[1]] | MUN_ID == "0000"
                ),
                "MD_ID",
                "MD_NAME",
                tol,
                on_error,
                .mun2 = .x$MUN_NAME[[1]]
              )
            ) -> res2
          #.error <<- .error
          .error <- .error
          res2
        },
        dr = sfDR:::.add_mun(sfDR::dr_municipal_districts(.sf = FALSE, .uniques = FALSE)),
        tol = .tol,
        on_error = .on_error
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(orden_original) %>%
      dplyr::pull(dm3) -> res
  } else {
    .do_names_cleaning(
      dm,
      dr = sfDR::dr_municipal_districts(.sf = FALSE, .uniques = FALSE),
      dr_id = "MD_ID",
      dr_name = "MD_NAME",
      .tol,
      .on_error
    ) -> res
  }
  if (.error | "error" %in% res[[1]]) {
    cli::cli_abort("")
  }
  res
}
