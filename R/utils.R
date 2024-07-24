dr_correction_matrix <- function(df, original, cleaned){
  n <- NULL

  df %>%
    dplyr::arrange(!!as.symbol(cleaned)) %>%
    dplyr::group_by(!!as.symbol(cleaned)) %>%
    dplyr::distinct(!!as.symbol(original), .keep_all = TRUE) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    tidyr::pivot_wider(names_from = n, values_from = !!as.symbol(original)) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), tidyr::replace_na, ""))
}




.text_cleaning <- function(names){
  . <- NULL

  names %>%
    tidyr::replace_na("_NA_") %>%
    stringr::str_to_lower() %>%
    stringr::str_squish() %>%
    stringr::str_remove(stringr::regex("provincia[ ]?")) %>%
    stringr::str_remove(stringr::regex("municipio[ ]?", ignore_case = TRUE)) %>%
    stringr::str_remove(stringr::regex("ayuntamiento [de ]?", ignore_case = TRUE)) %>%
    stringr::str_remove(stringr::regex(" \\(d[.]?[ ]?m[.]?\\)", ignore_case = TRUE)) %>%
    stringr::str_remove(stringr::regex(" [(]?Zona urbana[)]?", ignore_case = TRUE)) %>%
    stringr::str_remove_all(stringr::regex("(^|[ ])la[s]?", ignore_case = TRUE)) %>%
    stringr::str_remove_all(stringr::regex("(^|[ ])de[l]?", ignore_case = TRUE)) %>%
    stringr::str_remove_all(stringr::regex("(^|[ ])los", ignore_case = TRUE)) %>%
    stringr::str_remove_all(stringr::regex("(^|[ ])el", ignore_case = TRUE)) %>%
    stringr::str_squish() %>%
    chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", "aeiouun", .) %>%
    stringr::str_remove_all(stringr::regex("[^0-9a-z ]", ignore_case = TRUE))
}




.calc_dist0 <- function(v1, v2){
  value <- NULL

  m1 <- stringdist::stringdistmatrix(
    stringr::str_split(v1, " o ")[[1]],
    stringr::str_split(v2, " o ")[[1]]
  ) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::pull(value) %>%
    min()
  m2 <- stringdist::stringdist(v1, v2)
  res <- min(m1+0.01, m2)
  names(res) <- "dist"
  res
}

.calc_dist <- function(df){
  dist <- NULL
  Var2 <- NULL
  Var1 <- NULL

  if(any(df[[1]] == df[[2]])){
    df[["dist"]] <- (df[[1]] != df[[2]])*1
  } else {
    df[["dist"]] <- purrr::pmap_dfr(df, ~.calc_dist0(..1, ..2))[["dist"]]
  }
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(diff = max(dist/nchar(as.character(Var2)), dist/nchar(as.character(Var1)))) %>%
    dplyr::ungroup()
}


.compute_dist <- function(names, dr_names, ...){
  #browser()
  dist <- NULL
  Var1 <- NULL

  #print(unique(.text_cleaning(names)))
  #print("---------------------------------------")
  #print(.text_cleaning(dr_names))
  expand.grid(unique(.text_cleaning(names)), .text_cleaning(dr_names)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>%
    split(~Var1) %>%
    purrr::map(.calc_dist) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(dist) %>%
    dplyr::distinct(Var1, .keep_all = TRUE)
}


#.compute_dist0 <- function(names, dr_names, ...){
#  expand.grid(unique(.text_cleaning(names)), .text_cleaning(dr_names)) %>%
#    dplyr::as_tibble() %>%
#    dplyr::rowwise() %>%
#    dplyr::mutate(
#      dplyr::across(.fns = as.character),
#      dist = .calc_dist(Var1, Var2),
#      diff = max(dist/nchar(as.character(Var2)), dist/nchar(as.character(Var1)))
#    ) %>%
#    dplyr::ungroup() %>%
#    dplyr::arrange(dist) %>%
#    dplyr::distinct(Var1, .keep_all = TRUE)
#}

.do_replacement <- function(res, names, dr, dr_id, dr_name, ...){
  . <- NULL
  Var1 <- NULL
  Var2 <- NULL

  dr %>%
    dplyr::select(!!as.symbol(dr_id), !!as.symbol(dr_name)) %>%
    dplyr::mutate(Var2 = .text_cleaning(!!as.symbol(dr_name))) %>%
    dplyr::left_join(res, ., by = "Var2") %>%
    dplyr::distinct(Var1, Var2, .keep_all = TRUE) %>%
    dplyr::left_join(
      data.frame(names = unique(names)) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(Var1 = .text_cleaning(names)),
      by = "Var1"
    ) %>%
    dplyr::select(-!!as.symbol(dr_name)) %>%
    dplyr::left_join(
      dr %>%
        dplyr::distinct(!!as.symbol(dr_id), .keep_all = TRUE) %>%
        dplyr::select(!!as.symbol(dr_id), new_names = !!as.symbol(dr_name)),
      by = dr_id
    )
}


.check_replacement <- function(res, .tol, .on_error, ...){
  Var1 <- NULL
  nc <- NULL
  mnc <- NULL
  times <- NULL
  new_names <- NULL
  text <- NULL

  .prov <- list(...)[[".prov"]]
  .mun2 <- list(...)[[".mun2"]]
  .dm2 <- list(...)[[".dm2"]]
  if(.on_error == "fail" && max(res$diff, na.rm = TRUE) > .tol){
    cat("\n")
    cli::cli_alert_info("The following corrections are questionable at {(.tol)} of tolerance:")
    cat("\n")
    res %>%
      #tidyr::drop_na() %>%
      dplyr::filter(diff > .tol) %>%
      #dplyr::slice_head(n = 10) %>%
      dplyr::mutate(
        nc = nchar(Var1),
        mnc = max(nc),
        times = mnc - nc + 3
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        text = stringr::str_c(
          dplyr::if_else(!is.null(.prov), paste0(.prov, " > "), ""),
          dplyr::if_else(!is.null(.mun2), paste0(.mun2, " > "), ""),
          dplyr::if_else(!is.null(.dm2), paste0(.dm2, " > "), ""),
          tidyr::replace_na(names, "_NA_"),
          "  ",
          paste(replicate(max(times, 1), "-"), collapse = ""),
          "> ",
          new_names,
          "   (.tol >= ",
          round(diff, 2),
          ")"
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(text) -> texts
    names(texts) <- rep("x", length(texts))
    cli::cli_bullets(texts)
    cat("\n")
    cat("\n")
    cli::rule()
    cli::cli_alert_info("Please, use one of this options:")
    cat("\n")
    cli::cli_bullets(c(
      "*" = "Fix these cases manually and try again.",
      "*" = "Increase the .tol to accept these corrections.",
      "*" = "Use the .on_error argument to control the result."
      ))
    cat("\n\n")
    cli::cli_abort("", call = NULL)
    #.error <<- TRUE
    .error <- TRUE
    res <- res %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~"error")) #%>% tidyr::drop_na()
  } else {
    res <- res %>%
      dplyr::mutate(
        new_names = dplyr::case_when(
          stringr::str_to_lower(.on_error) %in% c("omit", "fail") ~ dplyr::if_else(new_names == "_NA_", "_NA_", new_names),
          stringr::str_detect(stringr::str_to_lower(.on_error), "(^|_)na") ~ dplyr::if_else(diff >= .tol, "_NA_", new_names),
          TRUE ~ dplyr::if_else(diff >= .tol, stringr::str_c(dplyr::if_else(new_names == "_NA_", "_NA_", new_names), as.character(.on_error), sep = " "), new_names)
        )
      )
  }
  res
}


.get_clean_names <- function(res, names, ...){
  . <- NULL
  new_names <- NULL

  data.frame(names = names) %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(dplyr::select(res, new_names, names), by = "names") %>%
    dplyr::pull(new_names) %>% dplyr::if_else(. == "NA", NA_character_, .)
}


.do_names_cleaning <- function(names, dr, dr_id, dr_name, .tol, .on_error = "fail", ...){
  #browser()
  .compute_dist(names, dr[[dr_name]], ...) %>%
    .do_replacement(names, dr, dr_id, dr_name, ...) %>%
    .check_replacement(.tol, .on_error, ...) %>%
    .get_clean_names(names, ...)
}
