#' @importFrom sfDR dr_sections
#' @export
sfDR::dr_sections


#' Clean Dominican Republic section names
#' `r lifecycle::badge("experimental")`
#'
#' This function cleans and standardizes the names of sections in the Dominican Republic.
#' It can also handle related names for municipal districts, municipalities,
#' and provinces, with tolerance for string similarity and options for error handling.
#'
#' @param sec Character vector of section names to be cleaned.
#' @param .dm Optional character vector of municipal district names. Defaults to NULL.
#' If provided, it will clean and match the section names within the context of
#' these municipal districts.
#' @param .mun Optional character vector of municipality names. Defaults to NULL.
#' If provided, it will clean and match the section names within the context of
#' these municipalities.
#' @param .prov Optional character vector of province names. Defaults to NULL.
#' If provided, it will clean and match the section names within the context of
#' these provinces.
#' @param .tol Numeric tolerance level for string similarity. Defaults to 0.33.
#' This parameter controls how similar two strings must be to be considered a match.
#' A lower value means stricter matching.
#' @param .on_error Character string specifying the error handling method.
#' Defaults to "fail". It can be one of the following: "fail" to stop execution on error,
#' "omit" to ignore unmatched names, or "na" to return NA for unmatched names.
#'
#' @return A cleaned character vector of section names.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic usage with section names only
#'   cleaned_sec_names <- dr_clean_sec_name(
#'     c(
#'       "Santo Domingo de Guzmán (Zona urbana)",
#'       "Azua de Compost. (Zona urbana)"
#'      )
#'   )
#'
#'   # Usage with municipal district names
#'   cleaned_sec_names <- dr_clean_sec_name(
#'     c(
#'       "SANTO DOMINGO DE GUZMAN (Zona urbana)",
#'       "Azua de Compostela (Zona urbana)"
#'     ),
#'     .dm = c("Distrito Nacional", "Azua")
#'   )
#' }
#'
dr_clean_sec_name <- function(sec, .dm = NULL, .mun = NULL, .prov = NULL, .tol = 0.33, .on_error = "fail"){
  dm2 <- NULL
  sec2 <- NULL
  DM <- NULL
  SEC <- NULL
  . <- NULL
  MD_NAME <- NULL
  MD_ID <- NULL
  orden_original <- NULL
  sec3 <- NULL

  if(!is.null(.dm)){
    data.frame(
      dm2 = .text_cleaning(.dm),
      sec2 = .text_cleaning(sec),
      DM = .dm,
      SEC = sec
    ) %>%
      dplyr::as_tibble() %>%
      dplyr::filter(dm2 == "_na_" & sec2 != "_na_") %>%
      dplyr::select(DM, SEC) %>%
      purrr::when(
        nrow(.) > 0 ~ {print(.); cli::cli_abort("You can't pass NA in .dm when sec isn't NA.")},
        TRUE ~ .
      )
    dr_clean_md_name(.dm, .mun, .prov) %>%
      data.frame(
        MD_NAME = .
      ) %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(
        sfDR::dr_municipal_districts(.sf = FALSE, .uniques = TRUE),
        by = "MD_NAME"
      ) %>%
      dplyr::mutate(
        dplyr::across(.cols = c(MD_NAME, MD_ID), tidyr::replace_na, "NA"),
        sec2 = sec,
        orden_original = dplyr::row_number()
      ) %>%
      split(~MD_ID) %>%
      purrr::map(\(.x, dr, tol, on_error){
        .x %>%
          dplyr::mutate(
            sec3 = .do_names_cleaning(
              sec2,
              dplyr::filter(
                dr,
                MD_ID == .x$MD_ID[[1]] | MD_ID == "000000"
              ),
              "SEC_ID",
              "SEC_NAME",
              tol,
              on_error,
              .dm2 = .x$MD_NAME[[1]]
            )
          ) -> res2
        #.error <<- .error
        .error <- .error
        res2
      },
      dr = sfDR:::.add_dm(sfDR::dr_sections(.sf = FALSE, .uniques = FALSE)),
      tol = .tol,
      on_error = .on_error
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(orden_original) %>%
      dplyr::pull(sec3) -> res
  } else {
    .do_names_cleaning(sec, sfDR::dr_sections(.sf = FALSE, .uniques = FALSE), "SEC_ID", "SEC_NAME", .tol, .on_error) -> res
  }
  if(.error | "error" %in% res[[1]]){
    cli::cli_abort("")
  }
  res
}
