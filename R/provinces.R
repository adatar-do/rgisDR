dr_get_provinces <- function(){
  DR_PROV %>%
    left_join(
      dr_province %>%
        distinct(PROV_ID, .keep_all = TRUE),
      by = "PROV_ID",
      .
    ) %>%
    #relocate(OBJECTID_1) %>%
    #select(-TOPONIMIA) %>%
    #rename(REG_ID = REG) %>%
    sf::st_as_sf()
}

get_dr_provinces <- function(){
  lifecycle::deprecate_warn("0.1.0", "get_dr_provinces()", "dr_get_provinces()")
  get_dr_provinces()
}


dr_clean_prov_name <- function(names){
  names %>%
    stringr::str_to_lower() %>%
    stringr::str_remove(stringr::regex("provincia", ignore_case = TRUE)) %>%
    stringr::str_trim() -> data
  np <- dr_province$PROV_NAME %>% stringr::str_to_lower()

  comb <- expand.grid(unique(data), np)
  comb[["dist"]] <- 0
  for (row in 1:nrow(comb)) {
    dist <- stringdist::stringdist(comb[row, "Var1"], comb[row, "Var2"])
    comb[row, "dist"] <- dist
  }
  comb  <- dplyr::arrange(comb, Var1, dist)
  comb <- dplyr::group_by(comb, Var1)
  comb <- dplyr::summarise(comb, Var2 = dplyr::first(Var2))
  comb <- dplyr::mutate(comb, dplyr::across(dplyr::everything(), stringr::str_to_title))

  dr_province %>%
    dplyr::select(Var2 = PROV_NAME, PROV_ID) %>%
    dplyr::left_join(comb, ., by = "Var2") %>%
    dplyr::select(-Var2) %>%
    dplyr::left_join(
      dr_province %>%
        dplyr::distinct(PROV_ID, .keep_all = T) %>%
        dplyr::select(Var2 = PROV_NAME, PROV_ID),
      by = "PROV_ID"
    ) %>%
    dplyr::select(-PROV_ID) -> comb
  res <- comb[["Var2"]]
  names(res) <- stringr::str_to_lower(comb[["Var1"]])
  res <- res[data]
  names(res) <- NULL
  res
}


dr_prov_clean_name <- function(names){
  lifecycle::deprecate_warn("0.1.0", "dr_prov_clean_name()", "dr_clean_prov_name()")
  dr_prov_clean_name(names)
}
