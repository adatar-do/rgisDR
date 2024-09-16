#' Guess information for mapping Dominican Republic data
#' `r lifecycle::badge("experimental")`
#'
#' This function attempts to automatically determine the appropriate administrative
#' level, name of the linking variable, and key variable for mapping data related
#' to the Dominican Republic. It examines the provided data and compares it with
#' available administrative boundaries to find the best match.
#'
#' @param data A data frame containing the data to be mapped.
#' @param .level Optional character string specifying the administrative level.
#'   If provided, the function will only consider this level. Valid options are:
#'   "sections", "md", "mun", "prov", "reg".
#' @param .name Optional character string specifying the name of the variable in
#'   `data` that contains the administrative unit names. If provided, the function
#'   will only consider this variable.
#' @param .key Optional character string specifying the name of the key variable
#'   in the administrative boundary data. If provided, the function will only
#'   consider this key.
#'
#' @return A list containing the following elements:
#'   * `level`: The inferred administrative level.
#'   * `name`: The name of the variable in `data` containing the administrative
#'     unit names.
#'   * `key`: The name of the key variable in the administrative boundary data.
#'
#' @examples
#' \dontrun{
#'   # Guess information for municipality data
#'   my_data <- data.frame(municipality = c("Santo Domingo", "Santiago"), value = 1:2)
#'   info <- guess_info(my_data)
#'
#'   # Specify the administrative level
#'   info <- guess_info(my_data, .level = "mun")
#' }
guess_info <- function(data, .level = NULL, .name = NULL, .key = NULL) {
  if (is.null(.level) | is.null(.name) | is.null(.key)) {
    .options <- list(
      "sections" = dr_sections(FALSE),
      "md" = dr_municipal_districts(FALSE),
      "mun" = dr_municipalities(FALSE),
      "prov" = dr_provinces(FALSE),
      "reg" = dr_regions(FALSE)
    )

    if (!is.null(.level)) {
      .options <- .options[.level]
    }

    if (!is.null(.name)) {
      data <- data[.name]
    }

    if (!is.null(.key)) {
      for (.o in names(.options)) {
        if (.key %in% names(.options[[.o]])) {
          .options[[.o]] <- .options[[.o]][.key]
        }
      }
    }

    found <- FALSE

    for (.opt_name in names(.options)) {
      .opt <- .options[[.opt_name]]
      for (name in names(.opt)) {
        for (variable in names(data)) {
          if (all(unique(data[[variable]]) %in% .opt[[name]])) {
            .level <- .opt_name
            .name <- variable
            .key <- name
            found <- TRUE
            break
          }
        }
        if (found) break
      }
      if (found) break
    }
  }

  return(list(
    "level" = .level,
    "name" = .name,
    "key" = .key
  ))
}




#' Prepare data for mapping in the Dominican Republic
#' `r lifecycle::badge("experimental")`
#'
#' This function prepares data for mapping by joining it with the appropriate
#' administrative boundary data for the Dominican Republic. It uses the
#' `guess_info` function to determine the administrative level and linking
#' variables if they are not explicitly provided.
#'
#' @param data A data frame containing the data to be mapped.
#' @param fill The name of the variable in `data` to be used for the fill aesthetic
#'   in the map.
#' @param .level Optional character string specifying the administrative level.
#'   See `guess_info` for details.
#' @param .name Optional character string specifying the name of the linking
#'   variable in `data`. See `guess_info` for details.
#' @param .key Optional character string specifying the name of the key variable
#'   in the administrative boundary data. See `guess_info` for details.
#'
#' @return An `sf` object containing the joined data and administrative boundaries.
#'
#' @examples
#' \dontrun{
#'   # Prepare data for municipality mapping
#'   my_data <- data.frame(municipality = c("Santo Domingo", "Santiago"), value = 1:2)
#'   map_data <- dr_map_data(my_data, fill = "value")
#' }
dr_map_data <- function(data, fill, .level = NULL, .name = NULL, .key = NULL) {
  info <- guess_info(data, .level, .name, .key)

  if (is.null(info[["level"]])) {
    stop("No se pudo determinar el nivel deseado del mapa")
    # Esto puede estar relacionado con la no limpieza de la variable.
    # Indicar al usuario intentar esto antes.
  }

  if (is.null(info[["name"]])) {
    stop("No se pudo determinar el nombre de la variable de enlace")
  }

  if (is.null(info[["key"]])) {
    stop("No se pudo determinar con qué variable...")
  }

  map_data <- switch(info[["level"]],
                     "sections" = dr_sections(),
                     "md" = dr_municipal_districts(),
                     "mun" = dr_municipalities(),
                     "prov" = dr_provinces(),
                     "reg" = dr_regions()
  ) %>%
    dplyr::left_join(
      data,
      by = dplyr::join_by(!!sym(info[["key"]]) == !!sym(info[["name"]]))
    )

  return(map_data)
}







#' Initialize a ggplot object for mapping in the Dominican Republic
#' `r lifecycle::badge("experimental")`
#'
#' This function initializes a `ggplot` object for mapping data related to the
#' Dominican Republic. It uses `dr_map_data` to prepare the data and sets up the
#' plot for further customization.
#'
#' @param data A data frame containing the data to be mapped.
#' @param fill The name of the variable in `data` to be used for the fill aesthetic.
#' @param .level Optional character string specifying the administrative level.
#'   See `guess_info` for details.
#' @param .name Optional character string specifying the name of the linking
#'   variable in `data`. See `guess_info` for details.
#' @param .key Optional character string specifying the name of the key variable
#'   in the administrative boundary data. See `guess_info` for details.
#'
#' @return A `ggplot` object ready for further customization.
#'
#' @examples
#' \dontrun{
#'   # Initialize a ggplot object for municipality mapping
#'   my_data <- data.frame(municipality = c("Santo Domingo", "Santiago"), value = 1:2)
#'   p <- dr_ggplot(my_data, fill = "value")
#' }
dr_ggplot <- function(data, fill, .level = NULL, .name = NULL, .key = NULL) {
  dr_map_data(data, fill, .level, .name, .key) %>%
    ggplot2::ggplot()
}





#' Add a geom_sf layer for Dominican Republic maps
#' `r lifecycle::badge("experimental")`
#'
#' This function adds a `geom_sf` layer to a `ggplot` object, specifically
#' designed for mapping data related to the Dominican Republic. It can handle
#' both direct `sf` objects and data frames that need to be joined with
#' administrative boundaries using `dr_map_data`.
#'
#' @param data An optional data frame or `sf` object to be mapped. If `NULL`,
#'   the function assumes the data is already present in the `ggplot` object.
#' @param ... Additional arguments passed to `geom_sf`.
#'
#' @return A `ggplot` object with the added `geom_sf` layer.
#'
#' @examples
#' \dontrun{
#'   # Add a geom_sf layer to an existing ggplot object
#'   p <- ggplot() + dr_geom_sf(data = my_map_data)
#'
#'   # Add a geom_sf layer with custom aesthetics
#'   p <- ggplot() + dr_geom_sf(data = my_data, fill = "value", color = "black")
#' }
dr_geom_sf <- function(data = NULL, ...) {
  .args <- list(...)

  if (!is.null(data)) {
    map_data <- dr_map_data(data, fill = .args$fill, .level = .args$level, .name = .args$name, .key = .args$key)
    .args$data <- map_data
    do.call(geom_sf, .args)
    #  geom_sf(data = map_data, mapping = .args$mapping, ...)
  } else {
    do.call(geom_sf, .args)
  }

}



#' Create a choropleth map of the Dominican Republic in `ggplot2`
#' `r lifecycle::badge("experimental")`
#'
#' This function creates a choropleth map of the Dominican Republic using
#' `ggplot2`. It simplifies the process of mapping data related to the country
#' by automatically handling data preparation, joining with administrative
#' boundaries, and setting up the plot with a default theme.
#'
#' @param data A data frame containing the data to be mapped.
#' @param fill The name of the variable in `data` to be used for the fill aesthetic.
#' @param .level Optional character string specifying the administrative level.
#'   See `guess_info` for details.
#' @param .name Optional character string specifying the name of the linking
#'   variable in `data`. See `guess_info` for details.
#' @param .key Optional character string specifying the name of the key variable
#'   in the administrative boundary data. See `guess_info` for details.
#' @param ... Additional arguments passed to `dr_geom_sf`.
#'
#' @return A `ggplot` object representing the choropleth map.
#'
#' @examples
#' \dontrun{
#'   # Create a choropleth map of municipality values
#'   my_data <- data.frame(municipality = c("Santo Domingo", "Santiago"), value = 1:2)
#'   map <- dr_map(my_data, fill = "value")
#'
#'   # Customize the map with additional arguments
#'   map <- dr_map(my_data, fill = "value", color = "black", linewidth = 0.5)
#' }
dr_map <- function(data, fill, .level = NULL, .name = NULL, .key = NULL, ...) {
  p <- dr_ggplot(data, fill, .level, .name, .key)

  .args <- list(...)
  if (is.null(.args$mapping)) {
    .args$mapping <- ggplot2::aes(fill = !!sym(fill))
  }


  p +
    do.call(dr_geom_sf, .args) +
    ggplot2::theme_void()
}
