#' @importFrom sfDR dr_municipalities
#' @export
sfDR::dr_municipalities



#' Clean Dominican Republic municipality names
#' `r lifecycle::badge("experimental")`
#'
#' This function cleans and standardizes the names of municipalities in the
#' Dominican Republic. It can also handle related names for provinces, with tolerance
#' for string similarity and options for error handling.
#'
#' @param mun Character vector of municipality names to be cleaned.
#' @param .prov Optional character vector of province names. Defaults to NULL.
#' If provided, it will clean and match the municipality names within the context
#' of these provinces.
#' @param .tol Numeric tolerance level for string similarity. Defaults to 0.33.
#' This parameter controls how similar two strings must be to be considered a match.
#' A lower value means stricter matching.
#' @param .on_error Character string specifying the error handling method. Defaults to "fail".
#' It can be one of the following: "fail" to stop execution on error,
#' "omit" to ignore unmatched names, or "na" to return NA for unmatched names.
#'
#' @return A cleaned character vector of municipality names.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic usage with municipality names only
#'   cleaned_mun_names <- dr_clean_mun_name(c("Santo dominio este", "Azua de Compostela"))
#'
#'   # Usage with province names
#'   cleaned_mun_names <- dr_clean_mun_name(
#'     c("SANTO DOMINGO DE GUZMAN", "Azua de Compostela"),
#'     .prov = c("Distrito Nacional", "Azua")
#'   )
#' }
#'
dr_clean_mun_name <- function(mun, .prov = NULL, .tol = 0.33, .on_error = "fail"){
  prov2 <- NULL
  mun2 <- NULL
  PROV <- NULL
  MUN <- NULL
  . <- NULL
  PROV_NAME <- NULL
  PROV_ID <- NULL
  orden_original <- NULL
  mun3 <- NULL

res <- list()
  if(!is.null(.prov)){
    data.frame(
      prov2 = .text_cleaning(.prov),
      mun2 = .text_cleaning(mun),
      PROV = .prov,
      MUN = mun
    ) %>%
      dplyr::as_tibble() %>%
      dplyr::filter(prov2 == "_na_" & mun2 != "_na_") %>%
      dplyr::select(PROV, MUN) %>%
      purrr::when(
        nrow(.) > 0 ~ {print(.); cli::cli_abort("You can't pass NA in .prov when mun isn't NA.")},
        TRUE ~ .
      )
    dr_clean_prov_name(.prov, .on_error = "na") %>%
      data.frame(
        PROV_NAME = .
      ) %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(
        dr_provinces(FALSE),
        by = "PROV_NAME"
      ) %>%
      dplyr::mutate(
        dplyr::across(.cols = c(PROV_NAME, PROV_ID), tidyr::replace_na, "NA"),
        mun2 = mun,
        orden_original = dplyr::row_number()
        )  %>%
      split(~PROV_ID) %>%
      purrr::map(\(.x, dr, tol, on_error){
        .x %>%
          dplyr::mutate(
            mun3 = .do_names_cleaning(
              mun2,
              dplyr::filter(
                dr,
                PROV_ID == .x$PROV_ID[[1]] | PROV_ID == "00"
              ),
              "MUN_ID",
              "MUN_NAME",
              tol,
              .on_error,
              .prov = .x$PROV_NAME[[1]]
            )
          )
      },
      dr = sfDR:::.add_prov(sfDR::dr_municipalities(.sf = FALSE, .uniques = FALSE)),
      tol = .tol,
      on_error = .on_error
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(orden_original) %>%
      dplyr::pull(mun3) -> res
  } else {
    .do_names_cleaning(mun, sfDR::dr_municipalities(.sf = FALSE, .uniques = FALSE), "MUN_ID", "MUN_NAME", .tol, .on_error) -> res
  }
  if(.error){
    cli::cli_abort("")
  }
  res
}
