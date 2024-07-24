# guess_info function
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

# dr_map_data function
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

# dr_ggplot function
dr_ggplot <- function(data, fill, .level = NULL, .name = NULL, .key = NULL) {
  dr_map_data(data, fill, .level, .name, .key) %>%
    ggplot()
}

# dr_geom_sf function
#dr_geom_sf <- function(data = NULL, fill, .level = NULL, .name = NULL, .key = NULL, ...) {
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

# dr_map function
dr_map <- function(data, fill, .level = NULL, .name = NULL, .key = NULL, ...) {
  p <- dr_ggplot(data, fill, .level, .name, .key)

  .args <- list(...)
  if (is.null(.args$mapping)) {
    .args$mapping <- aes(fill = !!sym(fill))
  }


  p +
    do.call(dr_geom_sf, .args) +
    theme_void()
}
