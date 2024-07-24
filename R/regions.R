dr_get_regions <- function(){
  DR_REG %>%
    dplyr::left_join(dr_region) %>%
    sf::st_as_sf()
}
