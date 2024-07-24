dr_get_municipalities <- function(){
  DR_MUN %>%
    sf::st_as_sf()
}


dr_clean_mun_name <- function(names){
  names %>%
    stringr::str_to_lower() %>%
    stringr::str_remove(stringr::regex("municipio[ ]?", ignore_case = TRUE)) %>%
    stringr::str_remove(stringr::regex("ayuntamiento [de ]?", ignore_case = TRUE)) %>%
    stringr::str_trim() -> data
  nm <- dr_munipality$MUN_NAME %>% stringr::str_to_lower()

  comb <- expand.grid(unique(data), nm)
  comb[["dist"]] <- 0
  for (row in 1:nrow(comb)) {
    dist <- stringdist::stringdist(comb[row, "Var1"], comb[row, "Var2"])
    comb[row, "dist"] <- dist
  }
  comb  <- dplyr::arrange(comb, Var1, dist)
  comb <- dplyr::group_by(comb, Var1)
  comb <- dplyr::summarise(comb, Var2 = dplyr::first(Var2))
  comb <- dplyr::mutate(comb, dplyr::across(dplyr::everything(), stringr::str_to_title))

  dr_munipality %>%
    dplyr::select(Var2 = MUN_NAME, MUN_ID) %>%
    dplyr::left_join(comb, ., by = "Var2") %>%
    dplyr::select(-Var2) %>%
    dplyr::left_join(
      dr_munipality %>%
        dplyr::distinct(MUN_ID, .keep_all = T) %>%
        dplyr::select(Var2 = MUN_NAME, MUN_ID),
      by = "MUN_ID"
    ) %>%
    dplyr::select(-MUN_ID) -> comb
  res <- comb[["Var2"]]
  names(res) <- stringr::str_to_lower(comb[["Var1"]])
  res <- res[data]
  names(res) <- NULL
  res
}
