# =============================================================================
# functions.R
# Alle functies voor choropleth kaarten
# =============================================================================

# -----------------------------------------------------------------------------
# CONFIG FUNCTIES
# -----------------------------------------------------------------------------

#' Laad en merge configuratie met inheritance
#'
#' Leest config.yml en past inheritance toe (inherits veld).
#' Retourneert een platte config lijst voor het gekozen profiel.
#'
#' @param profiel Character, naam van het configuratie profiel (bijv. "haaglanden")
#' @param config_path Character, pad naar config.yml bestand
#'
#' @return List met alle configuratie waarden
#'
#' @examples
#' config <- laad_config("haaglanden")
#'
#' @export
laad_config <- function(profiel, config_path = "config.yml") {
  
  config_all <- yaml::read_yaml(config_path)
  
  if (!profiel %in% names(config_all)) {
    stop(paste("Profiel", profiel, "niet gevonden in config.yml"))
  }
  
  config <- config_all[[profiel]]
  

  # Pas inheritance toe
  if (!is.null(config$inherits)) {
    parent <- config_all[[config$inherits]]
    config <- merge_config(parent, config)
  }
  
  return(config)
}


#' Merge twee config lijsten (parent en child)
#'
#' Child waarden overschrijven parent waarden.
#' Nested lists worden recursief gemerged.
#'
#' @param parent List, parent configuratie
#' @param child List, child configuratie
#'
#' @return List, gemerged configuratie
merge_config <- function(parent, child) {
  
  result <- parent
  
  for (key in names(child)) {
    if (key == "inherits") next
    
    if (is.list(child[[key]]) && is.list(result[[key]])) {
      result[[key]] <- merge_config(result[[key]], child[[key]])
    } else {
      result[[key]] <- child[[key]]
    }
  }
  
  return(result)
}


# -----------------------------------------------------------------------------
# DATA LAAD FUNCTIES
# -----------------------------------------------------------------------------

#' Laad geo data uit GPKG
#'
#' Laadt gemeenten of wijken laag uit een GeoPackage bestand.
#' Hernoemt kolommen naar standaard namen (GM_CODE, GM_NAAM, WK_CODE, WK_NAAM).
#'
#' @param gpkg_path Character, pad naar GPKG bestand
#' @param niveau Character, "gemeente" of "wijk"
#'
#' @return sf object met geometrie en codes
#'
#' @examples
#' geo <- laad_geo_data("data/WijkBuurtkaart_2025_v0.gpkg", "gemeente")
#'
#' @export
laad_geo_data <- function(gpkg_path, niveau) {
  
  # Bepaal laagnaam op basis van niveau
  laag <- if (niveau == "gemeente") "gemeenten_v0" else "wijken_v0"
  
  # Lees data

  geo_raw <- sf::st_read(gpkg_path, layer = laag, quiet = TRUE)
  
  # Detecteer geometry kolom
  geom_col <- attr(geo_raw, "sf_column")
  
  # Hernoem kolommen naar standaard namen
  if (niveau == "gemeente") {
    geo <- geo_raw |>
      dplyr::rename(GM_CODE = gm_code, GM_NAAM = gm_naam) |>
      dplyr::select(GM_CODE, GM_NAAM, dplyr::all_of(geom_col))
  } else {
    geo <- geo_raw |>
      dplyr::rename(
        WK_CODE = wk_code, 
        WK_NAAM = wk_naam,
        GM_CODE = gm_code
      ) |>
      dplyr::select(WK_CODE, WK_NAAM, GM_CODE, dplyr::all_of(geom_col))
  }
  
  # Zorg dat geometry kolom "geometry" heet
  if (geom_col != "geometry") {
    names(geo)[names(geo) == geom_col] <- "geometry"
    sf::st_geometry(geo) <- "geometry"
  }
  
  return(geo)
}


#' Laad studenten data
#'
#' Leest Excel bestand en verwijdert brackets uit kolomnamen.
#'
#' @param path Character, pad naar Excel bestand
#' @param sheet Character of integer, sheet naam of nummer
#'
#' @return Data frame met studenten data
#'
#' @examples
#' df <- laad_studenten_data("data/studenten.xlsx")
#'
#' @export
laad_studenten_data <- function(path, sheet = 1) {
  
  df <- readxl::read_excel(path, sheet = sheet)
  
  # Verwijder brackets uit kolomnamen
  colnames(df) <- gsub("[", "", colnames(df), fixed = TRUE)
  colnames(df) <- gsub("]", "", colnames(df), fixed = TRUE)
  
  return(df)
}


#' Laad gemeente referentie data
#'
#' Leest gemeenten alfabetisch bestand voor provincie filtering.
#'
#' @param path Character, pad naar Excel bestand
#'
#' @return Data frame met GM_CODE en Provincienaam
#'
#' @examples
#' ref <- laad_gemeente_ref("data/Gemeenten alfabetisch 2023.xlsx")
#'
#' @export
laad_gemeente_ref <- function(path) {
  
  ref <- readxl::read_excel(path) |>
    dplyr::rename(GM_CODE = GemeentecodeGM)
  
  return(ref)
}


# -----------------------------------------------------------------------------
# FILTER FUNCTIES
# -----------------------------------------------------------------------------

#' Filter geo data op basis van config
#'
#' Past filter toe op geo data: op gemeente codes, provincie of geen filter.
#'
#' @param geo sf object met geo data
#' @param config List met configuratie (gebied$filter)
#' @param gemeente_ref Data frame met gemeente referenties (voor provincie filter)
#'
#' @return sf object, gefilterd
#'
#' @export
filter_geo_data <- function(geo, config, gemeente_ref = NULL) {
  
  filter_config <- config$gebied$filter
  
  # Geen filter (heel Nederland)
  if (is.null(filter_config)) {
    return(geo)
  }
  
  # Filter op gemeente codes
  if (!is.null(filter_config$gm_codes)) {
    geo <- geo |>
      dplyr::filter(GM_CODE %in% filter_config$gm_codes)
    return(geo)
  }
  
  # Filter op provincie
  if (!is.null(filter_config$provincie)) {
    if (is.null(gemeente_ref)) {
      stop("gemeente_ref is vereist voor provincie filter")
    }
    
    gm_codes <- gemeente_ref |>
      dplyr::filter(Provincienaam == filter_config$provincie) |>
      dplyr::pull(GM_CODE) |>
      unique()
    
    geo <- geo |>
      dplyr::filter(GM_CODE %in% gm_codes)
    return(geo)
  }
  
  return(geo)
}


# -----------------------------------------------------------------------------
# POSTCODE-WIJK MAPPING FUNCTIES
# -----------------------------------------------------------------------------

#' Maak postcode naar wijkcode mapping
#'
#' Leest CBS PC6-huisnummer koppeltabel en maakt unieke PC6 -> WK_CODE mapping.
#' Meerdere huisnummers per PC6 worden gededupliceerd (meest voorkomende wijk wint).
#'
#' @param path Character, pad naar PC6 CSV bestand (puntkomma-gescheiden)
#'
#' @return Data frame met PC6 en WK_CODE kolommen
#'
#' @export
maak_postcode_wijk_mapping <- function(path) {

  pc6_raw <- utils::read.csv2(path, colClasses = "character")

  # Dedupliceer: per PC6, neem de meest voorkomende wijk
  mapping <- pc6_raw |>
    dplyr::count(PC6, Wijk2025) |>
    dplyr::arrange(PC6, dplyr::desc(n)) |>
    dplyr::group_by(PC6) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(WK_CODE = paste0("WK", Wijk2025)) |>
    dplyr::select(PC6, WK_CODE)

  return(mapping)
}


# -----------------------------------------------------------------------------
# AGGREGATIE FUNCTIES
# -----------------------------------------------------------------------------

#' Aggregeer studenten per gebied
#'
#' Telt aantal studenten per gemeente of wijk.
#' Voor wijk niveau wordt postcode gebruikt om wijkcode op te zoeken
#' via een PC6-wijk mapping tabel.
#'
#' @param df Data frame met studenten data
#' @param niveau Character, "gemeente" of "wijk"
#' @param gemeente_kolom Character, naam van gemeente kolom in df
#' @param postcode_kolom Character, naam van postcode kolom in df
#' @param pc6_wijk_mapping Data frame met PC6-WK_CODE mapping (vereist voor wijk niveau)
#'
#' @return Data frame met code en aantal
#'
#' @export
aggregeer_studenten <- function(df, niveau,
                                 gemeente_kolom = "Gemeente",
                                 postcode_kolom = "Postcode",
                                 pc6_wijk_mapping = NULL) {

  if (niveau == "gemeente") {
    result <- df |>
      dplyr::select(dplyr::all_of(gemeente_kolom)) |>
      dplyr::rename(GM_NAAM = dplyr::all_of(gemeente_kolom)) |>
      dplyr::group_by(GM_NAAM) |>
      dplyr::summarise(aantal = dplyr::n(), .groups = "drop")
  } else {
    if (is.null(pc6_wijk_mapping)) {
      stop("pc6_wijk_mapping is vereist voor wijk niveau. Laad deze via maak_postcode_wijk_mapping().")
    }

    result <- df |>
      dplyr::select(dplyr::all_of(postcode_kolom)) |>
      dplyr::rename(PC6 = dplyr::all_of(postcode_kolom)) |>
      dplyr::left_join(pc6_wijk_mapping, by = "PC6") |>
      dplyr::filter(!is.na(WK_CODE)) |>
      dplyr::group_by(WK_CODE) |>
      dplyr::summarise(aantal = dplyr::n(), .groups = "drop")
  }

  return(result)
}


#' Voeg studenten aantallen toe aan geo data
#'
#' Merged geaggregeerde studenten data met geo data.
#' Vult NA waarden met 0.
#'
#' @param geo sf object met geo data
#' @param studenten_agg Data frame met geaggregeerde aantallen
#' @param niveau Character, "gemeente" of "wijk"
#'
#' @return sf object met aantal kolom
#'
#' @export
merge_studenten_geo <- function(geo, studenten_agg, niveau) {
  
  if (niveau == "gemeente") {
    result <- dplyr::left_join(geo, studenten_agg, by = "GM_NAAM")
  } else {
    result <- dplyr::left_join(geo, studenten_agg, by = "WK_CODE")
  }
  
  # Vul NA met 0
  result$aantal[is.na(result$aantal)] <- 0
  
  return(result)
}


# -----------------------------------------------------------------------------
# BREAKS / CATEGORISATIE FUNCTIES
# -----------------------------------------------------------------------------

#' Bepaal breaks op basis van data en preset
#'
#' Genereert automatische breaks of gebruikt preset waarden.
#'
#' @param data Numeric vector met aantallen
#' @param preset Character, "veel", "middel", "weinig" of "auto"
#'
#' @return List met breaks (numeric) en labels (character)
#'
#' @export
bepaal_breaks <- function(data, preset = "auto") {
  
  # Verwijder NA en 0 voor berekening
  data_clean <- data[!is.na(data) & data > 0]
  
  if (length(data_clean) == 0) {
    return(list(
      breaks = c(0, 1, 10, 100),
      labels = c("0", "1-10", "10+")
    ))
  }
  
  max_val <- max(data_clean, na.rm = TRUE)
  
  if (preset == "auto") {
    # Automatische breaks op basis van max waarde
    if (max_val <= 50) {
      breaks <- c(1, 5, 10, 15, 20, 30, 40, 50, Inf)
      labels <- c("1-5", "6-10", "11-15", "16-20", "21-30", "31-40", "41-50", "50+")
    } else if (max_val <= 200) {
      breaks <- c(1, 10, 20, 40, 60, 100, 150, 200, Inf)
      labels <- c("1-10", "11-20", "21-40", "41-60", "61-100", "101-150", "151-200", "200+")
    } else if (max_val <= 1000) {
      breaks <- c(1, 20, 50, 100, 200, 400, 600, 1000, Inf)
      labels <- c("1-20", "21-50", "51-100", "101-200", "201-400", "401-600", "601-1000", "1000+")
    } else if (max_val <= 5000) {
      breaks <- c(1, 50, 150, 500, 1000, 2000, 4000, 5000, Inf)
      labels <- c("1-50", "51-150", "151-500", "501-1000", "1001-2000", "2001-4000", "4001-5000", "5000+")
    } else {
      breaks <- c(1, 20, 150, 500, 2000, 9000, 10000, Inf)
      labels <- c("1-20", "21-150", "151-500", "501-2000", "2001-9000", "9001-10000", "10000+")
    }
  } else if (preset == "veel") {
    breaks <- c(1, 200, 500, 1000, 2000, 4000, 8000, 10000, Inf)
    labels <- c("1-200", "201-500", "501-1000", "1001-2000", "2001-4000", "4001-8000", "8001-10000", "10000+")
  } else if (preset == "middel") {
    breaks <- c(1, 20, 50, 100, 200, 500, 1000, 2000, Inf)
    labels <- c("1-20", "21-50", "51-100", "101-200", "201-500", "501-1000", "1001-2000", "2000+")
  } else if (preset == "weinig") {
    breaks <- c(1, 5, 10, 20, 40, 60, 80, 100, Inf)
    labels <- c("1-5", "6-10", "11-20", "21-40", "41-60", "61-80", "81-100", "100+")
  }
  
  return(list(breaks = breaks, labels = labels))
}


#' Voeg categorie toe aan data
#'
#' Categoriseert aantal kolom op basis van breaks.
#'
#' @param data sf object met aantal kolom
#' @param breaks_info List met breaks en labels (output van bepaal_breaks)
#'
#' @return sf object met aantal_cat kolom
#'
#' @export
categoriseer_aantallen <- function(data, breaks_info) {
  
  data <- data |>
    dplyr::mutate(
      aantal_cat = cut(
        aantal,
        breaks = breaks_info$breaks,
        labels = breaks_info$labels,
        include.lowest = TRUE,
        right = FALSE
      )
    )
  
  # Zet factor levels expliciet
  data$aantal_cat <- factor(data$aantal_cat, levels = breaks_info$labels)
  
  return(data)
}


# -----------------------------------------------------------------------------
# GEO VERWERKING FUNCTIES
# -----------------------------------------------------------------------------

#' Verwijder dubbele geometrieën (strand/zee)
#'
#' Houdt alleen de meest oostelijke geometrie per code.
#' Dit verwijdert strand/zee gedeelten van kustgemeenten.
#'
#' @param data sf object
#' @param code_kolom Character, naam van de code kolom (GM_CODE of WK_CODE)
#'
#' @return sf object met unieke geometrieën
#'
#' @export
verwijder_dubbele_geometrie <- function(data, code_kolom) {
  
  # Bereken gemiddelde longitude per geometrie
  get_longitude <- function(geom) {
    coords <- sf::st_coordinates(geom)
    return(mean(coords[, "X"]))
  }
  
  data <- data |>
    dplyr::mutate(longitude = purrr::map_dbl(geometry, get_longitude)) |>
    dplyr::arrange(dplyr::desc(longitude)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(code_kolom))) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-longitude)
  
  return(data)
}


# -----------------------------------------------------------------------------
# PLOT FUNCTIES
# -----------------------------------------------------------------------------
#' Maak choropleth plot
#'
#' Genereert ggplot2 choropleth kaart met optionele labels en locatie markers.
#'
#' @param data sf object met aantal_cat kolom
#' @param config List met configuratie
#' @param breaks_info List met breaks en labels
#' @param locaties_sf sf object met locatie punten (optioneel)
#' @param niveau Character, "gemeente" of "wijk"
#'
#' @return ggplot2 object
#'
#' @export
maak_choropleth <- function(data, config, breaks_info, locaties_sf = NULL, niveau) {
  
  # Bepaal kleuren
  n_cats <- length(breaks_info$labels)
  palet <- config$visualisatie$kleuren_palet
  colors <- RColorBrewer::brewer.pal(min(n_cats + 1, 9), palet)[2:(n_cats + 1)]
  
  # Als we meer categorieën hebben dan kleuren, interpoleer
  if (n_cats > length(colors)) {
    colors <- grDevices::colorRampPalette(colors)(n_cats)
  }
  
  # Basis plot
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = factor(aantal_cat)),
      color = config$visualisatie$border_kleur,
      size = config$visualisatie$border_dikte,
      show.legend = "polygon"
    ) +
    ggplot2::scale_fill_manual(
      values = colors,
      limits = breaks_info$labels,
      na.translate = FALSE
    ) +
    ggplot2::theme_void()
  
  # Voeg labels toe indien geconfigureerd
  if (isTRUE(config$visualisatie$toon_labels) && niveau == "gemeente") {
    centroids <- sf::st_centroid(data)
    naam_kolom <- if (niveau == "gemeente") "GM_NAAM" else "WK_NAAM"
    
    p <- p +
      ggplot2::geom_text(
        data = centroids,
        ggplot2::aes(
          label = toupper(.data[[naam_kolom]]),
          x = sf::st_coordinates(centroids)[, 1],
          y = sf::st_coordinates(centroids)[, 2]
        ),
        size = 4,
        check_overlap = TRUE,
        fontface = "bold"
      )
  }
  
  # Voeg locatie markers toe indien aanwezig
  if (!is.null(locaties_sf) && nrow(locaties_sf) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = locaties_sf,
        color = "black",
        size = 3
      )
  }
  
  # Voeg titel toe
  titel_delen <- c(config$titel)
  if (!is.null(config$peildatum) && nchar(trimws(config$peildatum)) > 0) {
    titel_delen <- c(titel_delen, config$peildatum)
  }
  titel <- paste(titel_delen, collapse = ": ")

  p <- p + ggplot2::labs(title = titel, fill = "")
  
  return(p)
}


#' Maak locaties sf object
#'
#' Converteert locaties uit config naar sf object met juiste CRS.
#'
#' @param config List met configuratie (bevat locaties)
#' @param target_crs CRS van de target data
#'
#' @return sf object of NULL als geen locaties
#'
#' @export
maak_locaties_sf <- function(config, target_crs) {
  
  if (is.null(config$locaties) || length(config$locaties) == 0) {
    return(NULL)
  }
  
  # Converteer naar data frame
  locaties_df <- do.call(rbind, lapply(config$locaties, function(x) {
    data.frame(naam = x$naam, lat = x$lat, lon = x$lon)
  }))
  
  # Maak sf object
  locaties_sf <- sf::st_as_sf(
    locaties_df,
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  # Transformeer naar target CRS
  locaties_sf <- sf::st_transform(locaties_sf, target_crs)
  
  return(locaties_sf)
}


# -----------------------------------------------------------------------------
# TABEL FUNCTIES
# -----------------------------------------------------------------------------

#' Maak tabel van top gebieden
#'
#' Genereert gt tabel met gebieden waar minimaal 10 studenten wonen.
#'
#' @param data sf object met naam en aantal kolommen
#' @param config List met configuratie
#' @param niveau Character, "gemeente" of "wijk"
#' @param min_aantal Integer, minimum aantal voor opname (default 10)
#'
#' @return gt object
#'
#' @export
maak_tabel <- function(data, config, niveau, min_aantal = 10) {
  
  naam_kolom <- if (niveau == "gemeente") "GM_NAAM" else "WK_NAAM"
  naam_label <- if (niveau == "gemeente") "Gemeente" else "Wijk"
  
  # Verwijder geometrie en filter/sorteer
  tabel_data <- data |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(naam_kolom), aantal) |>
    dplyr::filter(aantal >= min_aantal) |>
    dplyr::arrange(dplyr::desc(aantal))
  
  # Maak gt tabel
  titel_delen <- c(config$titel)
  if (!is.null(config$peildatum) && nchar(trimws(config$peildatum)) > 0) {
    titel_delen <- c(titel_delen, config$peildatum)
  }
  titel <- paste(titel_delen, collapse = ": ")

  subtitel <- paste0(
    "Alleen ", tolower(naam_label), "en waar ", min_aantal, " of meer studenten wonen"
  )
  
  tabel <- tabel_data |>
    gt::gt() |>
    gt::tab_header(title = titel, subtitle = subtitel) |>
    gt::cols_label(
      !!naam_kolom := naam_label,
      aantal = "Aantal"
    )
  
  return(tabel)
}


# -----------------------------------------------------------------------------
# OUTPUT FUNCTIES
# -----------------------------------------------------------------------------

#' Genereer output bestandsnaam
#'
#' Maakt bestandsnaam op basis van config en type.
#'
#' @param config List met configuratie
#' @param niveau Character, "gemeente" of "wijk"
#' @param type Character, "plot" of "tabel"
#' @param extensie Character, bestandsextensie (default "png")
#'
#' @return Character, bestandsnaam
#'
#' @export
genereer_bestandsnaam <- function(config, niveau, type, extensie = "png") {
  
  # Maak veilige naam
  gebied_naam <- gsub(" ", "_", config$gebied$naam)
  gebied_naam <- gsub("-", "_", gebied_naam)
  
  datum <- format(Sys.Date(), "%Y%m%d")
  
  bestandsnaam <- paste0(
    gebied_naam, "_",
    niveau, "_",
    type, "_",
    datum, ".",
    extensie
  )
  
  return(bestandsnaam)
}


#' Sla plot op
#'
#' Slaat ggplot2 plot op als PNG.
#'
#' @param plot ggplot2 object
#' @param path Character, volledig pad voor output
#' @param width Numeric, breedte in inches (default 13)
#' @param height Numeric, hoogte in inches (default 10)
#'
#' @export
sla_plot_op <- function(plot, path, width = 13, height = 10) {
  
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
  
  message(paste("Plot opgeslagen:", path))
}


#' Sla tabel op
#'
#' Slaat gt tabel op als PNG.
#'
#' @param tabel gt object
#' @param path Character, volledig pad voor output
#'
#' @export
sla_tabel_op <- function(tabel, path) {
  
  gt::gtsave(tabel, filename = path)
  
  message(paste("Tabel opgeslagen:", path))
}
