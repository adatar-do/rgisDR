dr_random_point <- function(.x, .level){
  coords <- NULL

  .x <- as.character(.x)
  .x  <- get(paste0("dr_clean_", .level, "_name"))(.x)
  .sf <- do.call(what = `::`, args = list("sfDR", paste0("DR_", toupper(.level)))) %>%
    dplyr::left_join(
      do.call(
    what = `::`,
    args = list(
      "sfDR",
      list(
        "mun" = "dr_munipality"
      )[.level][[1]]
      )
    ),
    by = paste0(toupper(.level), "_ID")
    ) %>%
    dplyr::select(-dplyr::contains("ID"))
  .name <- paste0(toupper(.level), "_NAME")
  .x <- .x %>%
    dplyr::as_tibble() %>%
    stats::setNames(.name) %>%
    dplyr::left_join(
      .sf,
      by = .name
    ) %>%
    dplyr::rename_with(~stringr::str_remove(.x, paste0( toupper(.level), "_")))

  .x %>%
    tibble::rowid_to_column() %>%
    split(~rowid) %>%
    purrr::map(.st_sample) %>%
    dplyr::bind_rows() %>%
    dplyr::pull(coords)
}


.st_sample <- function(.x){
  .x <- .x %>%
    sf::st_as_sf()
  .x %>%
    dplyr::mutate(
      coords = sf::st_sample(.x, 1)
    )

}
