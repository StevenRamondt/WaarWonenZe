# =============================================================================
# functions.R
# Alle functies voor choropleth kaarten
# =============================================================================

# Standaard in R >= 4.4; definieer voor oudere versies
if (!exists("%||%", envir = baseenv())) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

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
laad_config <- function(profiel, config_path = here::here("config.yml")) {

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


#' Leesbare namen van alle profielen
#'
#' @param config_path Character, pad naar config.yml bestand
#'
#' @return Named character vector: namen = gebiedsnaam, waarden = profielnaam
#'
#' @export
profiel_keuzes <- function(config_path = here::here("config.yml")) {

  profielen <- setdiff(names(yaml::read_yaml(config_path)), "default")
  namen <- vapply(profielen, function(p) {
    config <- laad_config(p, config_path)
    paste0(config$gebied$naam, " (", config$niveau, ")")
  }, character(1))

  stats::setNames(profielen, namen)
}


# -----------------------------------------------------------------------------
# DATA LAAD FUNCTIES
# -----------------------------------------------------------------------------

# Referentiedata wordt één keer per sessie ingelezen
.referentie_cache <- new.env(parent = emptyenv())

laad_rds_cached <- function(pad) {
  if (is.null(.referentie_cache[[pad]])) {
    .referentie_cache[[pad]] <- readRDS(pad)
  }
  .referentie_cache[[pad]]
}


#' Controleer of de referentiedata aanwezig is
#'
#' @param config List met configuratie
#'
#' @return Character vector met ontbrekende bestanden (leeg als alles er is)
#'
#' @export
ontbrekende_referentiedata <- function(config) {

  paden <- unlist(config$data[c("gemeenten", "wijken", "pc6")])
  paden[!file.exists(here::here(paden))]
}


#' Laad geo data
#'
#' Laadt de voorbewerkte gemeente- of wijkgrenzen (zie scripts/prepare_data.R).
#'
#' @param config List met configuratie (data$gemeenten, data$wijken)
#' @param niveau Character, "gemeente" of "wijk"
#'
#' @return sf object met GM_CODE, GM_NAAM, PROVINCIE (en WK_CODE, WK_NAAM)
#'
#' @export
laad_geo_data <- function(config, niveau) {

  # readRDS laadt sf niet zelf; zonder sf werken dplyr-verbs niet op sf objecten
  requireNamespace("sf", quietly = TRUE)

  pad <- if (niveau == "gemeente") config$data$gemeenten else config$data$wijken
  laad_rds_cached(here::here(pad))
}


#' Laad PC6 koppeltabel
#'
#' @param config List met configuratie (data$pc6)
#'
#' @return Data frame met PC6, WK_CODE en GM_CODE
#'
#' @export
laad_pc6_mapping <- function(config) {
  laad_rds_cached(here::here(config$data$pc6))
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
#' df <- laad_studenten_data("data/voorbeeld/studenten_demo.xlsx")
#'
#' @export
laad_studenten_data <- function(path, sheet = 1) {

  df <- readxl::read_excel(path, sheet = sheet)

  # Verwijder brackets uit kolomnamen
  colnames(df) <- gsub("[", "", colnames(df), fixed = TRUE)
  colnames(df) <- gsub("]", "", colnames(df), fixed = TRUE)
  colnames(df) <- trimws(colnames(df))

  return(df)
}


#' Kolommen die geschikt zijn om op te filteren
#'
#' Kolommen met een beperkt aantal unieke waarden (bijv. IsBekostigd, Leerweg).
#'
#' @param df Data frame met studenten data
#' @param max_waarden Integer, maximaal aantal unieke waarden
#'
#' @return Character vector met kolomnamen
#'
#' @export
filterbare_kolommen <- function(df, max_waarden = 50) {

  n_uniek <- vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1))
  names(df)[n_uniek >= 2 & n_uniek <= max_waarden]
}


# -----------------------------------------------------------------------------
# FILTER FUNCTIES
# -----------------------------------------------------------------------------

#' Filter studenten op kenmerken
#'
#' Houdt alleen rijen over waarvan de kolom een van de toegestane waarden heeft.
#' Waarden worden als tekst vergeleken, zodat TRUE uit config.yml matcht met
#' een logische kolom in Excel. Kolommen die niet bestaan worden overgeslagen.
#'
#' @param df Data frame met studenten data
#' @param filters Named list: kolomnaam -> toegestane waarden
#'
#' @return Gefilterd data frame met attributen "overgeslagen" (ontbrekende
#'   kolommen) en "weggefilterd" (aantal verwijderde rijen per kolom, in volgorde)
#'
#' @examples
#' filter_studenten(df, list(IsBekostigd = TRUE, Leerweg = c("BBL")))
#'
#' @export
filter_studenten <- function(df, filters) {

  overgeslagen <- character(0)
  weggefilterd <- integer(0)

  for (kolom in names(filters)) {
    waarden <- filters[[kolom]]
    if (length(waarden) == 0) next

    if (!kolom %in% colnames(df)) {
      overgeslagen <- c(overgeslagen, kolom)
      next
    }

    n_voor <- nrow(df)
    df <- df[as.character(df[[kolom]]) %in% as.character(waarden), , drop = FALSE]
    weggefilterd[kolom] <- n_voor - nrow(df)
  }

  attr(df, "overgeslagen") <- overgeslagen
  attr(df, "weggefilterd") <- weggefilterd
  return(df)
}


#' Filter geo data op basis van config
#'
#' Past filter toe op geo data: op gemeente codes, provincie of geen filter.
#'
#' @param geo sf object met geo data (met GM_CODE en PROVINCIE)
#' @param config List met configuratie (gebied$filter)
#'
#' @return sf object, gefilterd
#'
#' @export
filter_geo_data <- function(geo, config) {

  filter_config <- config$gebied$filter

  # Geen filter (heel Nederland)
  if (is.null(filter_config)) {
    return(geo)
  }

  # Filter op gemeente codes
  if (!is.null(filter_config$gm_codes)) {
    return(dplyr::filter(geo, GM_CODE %in% filter_config$gm_codes))
  }

  # Filter op provincie
  if (!is.null(filter_config$provincie)) {
    return(dplyr::filter(geo, PROVINCIE == filter_config$provincie))
  }

  return(geo)
}


# -----------------------------------------------------------------------------
# KOPPEL FUNCTIES (studenten -> gebied)
# -----------------------------------------------------------------------------

# Gangbare alternatieve schrijfwijzen (genormaliseerd) -> CBS naam
GEMEENTE_ALIASSEN <- c(
  "den haag" = "'s-Gravenhage",
  "s-gravenhage" = "'s-Gravenhage",
  "the hague" = "'s-Gravenhage",
  "den bosch" = "'s-Hertogenbosch",
  "s-hertogenbosch" = "'s-Hertogenbosch"
)


#' Normaliseer postcodes naar PC6
#'
#' Verwijdert spaties en zet om naar hoofdletters. Ongeldige postcodes
#' (bijv. alleen 4 cijfers) worden NA.
#'
#' @param x Character vector met postcodes
#'
#' @return Character vector met PC6 (bijv. "2612PD") of NA
#'
#' @examples
#' normaliseer_postcode(c("2612 pd", "2612PD", "2612"))
#'
#' @export
normaliseer_postcode <- function(x) {

  pc <- toupper(gsub("\\s", "", as.character(x)))
  pc[!grepl("^[1-9][0-9]{3}[A-Z]{2}$", pc)] <- NA
  pc
}


normaliseer_gemeentenaam <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\\s*\\([^)]*\\)$", "", x)   # "rijswijk (zh.)" -> "rijswijk"
  gsub("\\s+", " ", x)
}


#' Koppel gemeentenamen aan GM-codes
#'
#' Probeert achtereenvolgens: exacte naam, bekende alias, en naam zonder
#' provincie-achtervoegsel (alleen als dat eenduidig is, dus niet bij
#' "Bergen (NH.)" vs "Bergen (L.)").
#'
#' @param namen Character vector met gemeentenamen uit de studentdata
#' @param gemeenten Data frame met GM_CODE en GM_NAAM
#'
#' @return Character vector met GM_CODE of NA
#'
#' @export
koppel_gemeente_code <- function(namen, gemeenten) {

  ref <- unique(sf::st_drop_geometry(gemeenten)[, c("GM_CODE", "GM_NAAM")])
  namen <- trimws(as.character(namen))

  code <- ref$GM_CODE[match(namen, ref$GM_NAAM)]

  genormaliseerd <- normaliseer_gemeentenaam(namen)
  alias <- GEMEENTE_ALIASSEN[genormaliseerd]
  code <- ifelse(is.na(code), ref$GM_CODE[match(alias, ref$GM_NAAM)], code)

  ref_norm <- normaliseer_gemeentenaam(ref$GM_NAAM)
  eenduidig <- !(ref_norm %in% ref_norm[duplicated(ref_norm)])
  code <- ifelse(
    is.na(code),
    ref$GM_CODE[eenduidig][match(genormaliseerd, ref_norm[eenduidig])],
    code
  )

  unname(code)
}


#' Koppel studenten aan een gebiedscode
#'
#' Gemeente niveau: via de gemeentenaam. Wijk niveau: via de postcode en de
#' CBS PC6 koppeltabel.
#'
#' @param df Data frame met studenten data
#' @param niveau Character, "gemeente" of "wijk"
#' @param gemeente_kolom Character, naam van gemeente kolom in df
#' @param postcode_kolom Character, naam van postcode kolom in df
#' @param gemeenten sf/data frame met GM_CODE en GM_NAAM
#' @param pc6_mapping Data frame met PC6 en WK_CODE (vereist voor wijk niveau)
#'
#' @return Data frame met kolommen bron (oorspronkelijke waarde), code en reden
#'
#' @export
koppel_studenten <- function(df, niveau,
                             gemeente_kolom = "Gemeente",
                             postcode_kolom = "Postcode",
                             gemeenten = NULL,
                             pc6_mapping = NULL) {

  if (niveau == "gemeente") {
    bron <- df[[gemeente_kolom]]
    code <- koppel_gemeente_code(bron, gemeenten)
  } else {
    if (is.null(pc6_mapping)) {
      stop("pc6_mapping is vereist voor wijk niveau.")
    }
    bron <- df[[postcode_kolom]]
    code <- pc6_mapping$WK_CODE[match(normaliseer_postcode(bron), pc6_mapping$PC6)]
  }

  bron <- as.character(bron)
  data.frame(
    bron = bron,
    code = code,
    reden = bepaal_reden_niet_herkend(bron, code, niveau),
    stringsAsFactors = FALSE
  )
}


#' Reden waarom een waarde niet gekoppeld kon worden
#'
#' @param bron Character vector met oorspronkelijke waarden
#' @param code Character vector met gekoppelde codes (NA = niet herkend)
#' @param niveau Character, "gemeente" of "wijk"
#'
#' @return Character vector met reden, NA voor gekoppelde waarden
bepaal_reden_niet_herkend <- function(bron, code, niveau) {

  schoon <- toupper(gsub("\\s", "", bron))

  reden <- if (niveau == "gemeente") {
    rep("gemeente onbekend", length(bron))
  } else {
    dplyr::case_when(
      grepl("^[0-9]{4}$", schoon) ~ "alleen 4 cijfers",
      is.na(normaliseer_postcode(bron)) ~ "ongeldige postcode",
      TRUE ~ "postcode niet gevonden"
    )
  }

  reden[is.na(bron) | trimws(bron) == ""] <- "leeg"
  reden[!is.na(code)] <- NA
  reden
}


#' Aggregeer gekoppelde studenten per gebied
#'
#' @param gekoppeld Data frame uit koppel_studenten()
#' @param niveau Character, "gemeente" of "wijk"
#'
#' @return Data frame met code kolom (GM_CODE of WK_CODE) en aantal
#'
#' @export
aggregeer_studenten <- function(gekoppeld, niveau) {

  code_kolom <- if (niveau == "gemeente") "GM_CODE" else "WK_CODE"

  gekoppeld |>
    dplyr::filter(!is.na(code)) |>
    dplyr::count(code, name = "aantal") |>
    dplyr::rename(!!code_kolom := code)
}


#' Voeg studenten aantallen toe aan geo data
#'
#' Merged geaggregeerde studenten data met geo data (op code).
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

  code_kolom <- if (niveau == "gemeente") "GM_CODE" else "WK_CODE"
  result <- dplyr::left_join(geo, studenten_agg, by = code_kolom)

  # Vul NA met 0
  result$aantal[is.na(result$aantal)] <- 0

  return(result)
}


#' Maak koppelrapport
#'
#' Telt hoeveel studenten op de kaart staan, buiten het gebied wonen of niet
#' herkend zijn, en welke waarden niet herkend werden.
#'
#' @param gekoppeld Data frame uit koppel_studenten()
#' @param geo_codes Character vector met codes van gebieden op de kaart
#' @param n_voor_filter Integer, aantal studenten vóór het filteren
#'
#' @return List met aantallen en een data frame niet_herkend_waarden (waarde, reden, aantal)
#'
#' @export
maak_koppelrapport <- function(gekoppeld, geo_codes, n_voor_filter = nrow(gekoppeld)) {

  herkend <- !is.na(gekoppeld$code)

  niet_herkend <- gekoppeld[!herkend, , drop = FALSE] |>
    dplyr::mutate(waarde = ifelse(is.na(bron) | bron == "", "(leeg)", bron)) |>
    dplyr::count(waarde, reden, name = "aantal", sort = TRUE)

  list(
    voor_filter = n_voor_filter,
    na_filter = nrow(gekoppeld),
    op_kaart = sum(herkend & gekoppeld$code %in% geo_codes),
    buiten_gebied = sum(herkend & !gekoppeld$code %in% geo_codes),
    niet_herkend = sum(!herkend),
    niet_herkend_waarden = as.data.frame(niet_herkend)
  )
}


# -----------------------------------------------------------------------------
# BREAKS / CATEGORISATIE FUNCTIES
# -----------------------------------------------------------------------------

# Klassegrenzen: c(1, 200, 500) betekent klassen 1-200, 201-500 (en > 500
# als er grotere aantallen voorkomen).
BREAKS_PRESETS <- list(
  veel   = c(1, 200, 500, 1000, 2000, 4000, 8000, 10000),
  middel = c(1, 20, 50, 100, 200, 500, 1000, 2000),
  weinig = c(1, 5, 10, 20, 40, 60, 80, 100)
)

BREAKS_AUTO <- list(
  c(1, 5, 10, 15, 20, 30, 40, 50),
  c(1, 10, 20, 40, 60, 100, 150, 200),
  c(1, 20, 50, 100, 200, 400, 600, 1000),
  c(1, 50, 150, 500, 1000, 2000, 4000, 5000),
  c(1, 20, 150, 500, 2000, 9000, 10000)
)


#' Lees klassegrenzen uit tekst
#'
#' @param tekst Character, bijv. "1, 20, 150, 500"
#'
#' @return Numeric vector met oplopende grenzen
#'
#' @export
parse_breaks <- function(tekst) {

  delen <- trimws(strsplit(tekst, "[,; ]+")[[1]])
  breaks <- suppressWarnings(as.numeric(delen[delen != ""]))

  if (length(breaks) < 2 || anyNA(breaks)) {
    stop("Klassegrenzen moeten minimaal twee getallen zijn, gescheiden door komma's (bijv. 1, 20, 150).")
  }
  if (is.unsorted(breaks, strictly = TRUE) || breaks[1] < 1) {
    stop("Klassegrenzen moeten oplopend zijn en beginnen bij 1 of hoger.")
  }
  breaks
}


#' Bepaal breaks op basis van data en preset
#'
#' @param data Numeric vector met aantallen
#' @param preset Character, "auto", "veel", "middel", "weinig" of "eigen"
#' @param eigen Numeric vector met eigen klassegrenzen (bij preset "eigen")
#'
#' @return List met breaks (numeric, incl. Inf als er een bovenklasse is) en labels
#'
#' @export
bepaal_breaks <- function(data, preset = "auto", eigen = NULL) {

  max_val <- max(c(0, data), na.rm = TRUE)

  grenzen <- switch(
    preset,
    eigen = eigen,
    auto = {
      past <- vapply(BREAKS_AUTO, function(b) max_val <= max(b), logical(1))
      if (any(past)) BREAKS_AUTO[[which(past)[1]]] else BREAKS_AUTO[[length(BREAKS_AUTO)]]
    },
    BREAKS_PRESETS[[preset]]
  )

  if (is.null(grenzen)) {
    stop(paste("Onbekende breaks preset:", preset))
  }

  # Bovenklasse alleen als er aantallen boven de hoogste grens zijn
  if (max_val > max(grenzen)) {
    grenzen <- c(grenzen, Inf)
  }

  return(list(breaks = grenzen, labels = maak_klasse_labels(grenzen)))
}


#' Maak labels bij klassegrenzen
#'
#' c(1, 200, 500, Inf) -> "1-200", "201-500", "> 500"
#'
#' @param breaks Numeric vector met klassegrenzen
#'
#' @return Character vector met labels
maak_klasse_labels <- function(breaks) {

  onder <- c(breaks[1], utils::head(breaks[-1], -1) + 1)
  boven <- breaks[-1]

  labels <- ifelse(onder == boven, as.character(boven), paste0(onder, "-", boven))
  labels[is.infinite(boven)] <- paste(">", breaks[length(breaks) - 1])
  labels
}


#' Voeg categorie toe aan data
#'
#' Klassen zijn rechts-gesloten: met breaks c(1, 200, 500) valt 200 in "1-200".
#' Gebieden zonder studenten krijgen de categorie "0".
#'
#' @param data sf object met aantal kolom
#' @param breaks_info List met breaks en labels (output van bepaal_breaks)
#'
#' @return sf object met aantal_cat kolom (factor, eerste level "0")
#'
#' @export
categoriseer_aantallen <- function(data, breaks_info) {

  klasse <- cut(
    data$aantal,
    breaks = breaks_info$breaks,
    labels = breaks_info$labels,
    include.lowest = TRUE,
    right = TRUE
  )

  klasse <- as.character(klasse)
  klasse[data$aantal == 0] <- "0"

  data$aantal_cat <- factor(klasse, levels = c("0", breaks_info$labels))

  return(data)
}


# -----------------------------------------------------------------------------
# PLOT FUNCTIES
# -----------------------------------------------------------------------------

#' Kies kleuren voor de klassen
#'
#' @param n Integer, aantal klassen (exclusief "0")
#' @param palet Character, RColorBrewer palet
#'
#' @return Character vector met n kleuren, van licht naar donker
kies_kleuren <- function(n, palet) {

  k <- max(3, n + 1)
  if (k <= 9) {
    utils::tail(RColorBrewer::brewer.pal(k, palet), n)
  } else {
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, palet)[-1])(n)
  }
}


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
  palet <- config$visualisatie$kleuren_palet
  kleuren <- stats::setNames(
    kies_kleuren(length(breaks_info$labels), palet),
    breaks_info$labels
  )

  # Klasse "0" alleen in de legenda als er gebieden zonder studenten zijn
  limits <- breaks_info$labels
  if (any(data$aantal_cat == "0", na.rm = TRUE)) {
    kleuren <- c("0" = config$visualisatie$nul_kleur %||% "#f0f0f0", kleuren)
    limits <- c("0", limits)
  }

  # Basis plot
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = aantal_cat),
      color = config$visualisatie$border_kleur,
      linewidth = config$visualisatie$border_dikte,
      show.legend = TRUE   # ook lege klassen een kleurvak in de legenda
    ) +
    ggplot2::scale_fill_manual(
      values = kleuren,
      limits = limits,
      drop = FALSE
    ) +
    ggplot2::theme_void()

  # Voeg labels toe indien geconfigureerd
  if (isTRUE(config$visualisatie$toon_labels)) {
    naam_kolom <- if (niveau == "gemeente") "GM_NAAM" else "WK_NAAM"
    punten <- sf::st_point_on_surface(sf::st_geometry(data))
    coords <- sf::st_coordinates(punten)

    # "Wijk 01 Oostduinen" -> "Oostduinen"
    labels <- sub("^Wijk \\d+\\s+", "", data[[naam_kolom]])

    p <- p +
      ggplot2::annotate(
        "text",
        x = coords[, 1],
        y = coords[, 2],
        label = toupper(labels),
        size = if (niveau == "gemeente") 4 else 2.5,
        fontface = "bold",
        check_overlap = TRUE
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

  p <- p + ggplot2::labs(title = maak_titel(config), fill = "")

  return(p)
}


#' Maak titel uit titel en peildatum
#'
#' @param config List met configuratie (titel, peildatum)
#'
#' @return Character
maak_titel <- function(config) {

  titel_delen <- c(config$titel)
  if (!is.null(config$peildatum) && nchar(trimws(config$peildatum)) > 0) {
    titel_delen <- c(titel_delen, config$peildatum)
  }
  paste(titel_delen, collapse = ": ")
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

#' Maak tabeldata van top gebieden
#'
#' @param data sf object met naam en aantal kolommen
#' @param niveau Character, "gemeente" of "wijk"
#' @param min_aantal Integer, minimum aantal voor opname
#'
#' @return Data frame met naam en aantal, aflopend gesorteerd
#'
#' @export
maak_tabel_data <- function(data, niveau, min_aantal = 10) {

  naam_kolom <- if (niveau == "gemeente") "GM_NAAM" else "WK_NAAM"

  data |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(naam_kolom), aantal) |>
    dplyr::filter(aantal >= min_aantal) |>
    dplyr::arrange(dplyr::desc(aantal))
}


#' Maak tabel van top gebieden
#'
#' Genereert gt tabel met gebieden waar minimaal min_aantal studenten wonen.
#'
#' @param tabel_data Data frame uit maak_tabel_data()
#' @param config List met configuratie
#' @param niveau Character, "gemeente" of "wijk"
#' @param min_aantal Integer, minimum aantal voor opname (voor de subtitel)
#'
#' @return gt object
#'
#' @export
maak_tabel <- function(tabel_data, config, niveau, min_aantal = 10) {

  naam_kolom <- if (niveau == "gemeente") "GM_NAAM" else "WK_NAAM"
  naam_label <- if (niveau == "gemeente") "Gemeente" else "Wijk"

  subtitel <- paste0(
    "Alleen ", tolower(naam_label), "en waar ", min_aantal, " of meer studenten wonen"
  )

  tabel_data |>
    gt::gt() |>
    gt::tab_header(title = maak_titel(config), subtitle = subtitel) |>
    gt::cols_label(
      !!naam_kolom := naam_label,
      aantal = "Aantal"
    )
}


# -----------------------------------------------------------------------------
# PIPELINE
# -----------------------------------------------------------------------------

#' Genereer kaart, tabel en koppelrapport
#'
#' Volledige pipeline: filteren, koppelen, aggregeren, categoriseren, plotten.
#' Wordt gebruikt door zowel app.R als main.R.
#'
#' @param studenten Data frame met studenten data
#' @param config List met configuratie (incl. eventuele overrides uit de app)
#' @param gemeente_kolom Character, naam van gemeente kolom
#' @param postcode_kolom Character, naam van postcode kolom
#' @param voortgang Functie(fractie, detail) voor voortgangsmeldingen (optioneel)
#'
#' @return List met plot, tabel, tabel_data, data, breaks_info, rapport, config
#'
#' @export
genereer_kaart <- function(studenten, config,
                           gemeente_kolom = "Gemeente",
                           postcode_kolom = "Postcode",
                           voortgang = function(fractie, detail) NULL) {

  niveau <- config$niveau
  min_aantal <- config$tabel$min_aantal %||% 10

  # [1] Studenten filteren
  voortgang(0.1, "Studenten filteren...")
  n_voor_filter <- nrow(studenten)
  studenten <- filter_studenten(studenten, config$filters)

  # [2] Geo data laden en filteren
  voortgang(0.2, "Geo data laden...")
  geo <- filter_geo_data(laad_geo_data(config, niveau), config)

  # [3] Koppelen en aggregeren
  voortgang(0.2, "Studenten koppelen...")
  gekoppeld <- koppel_studenten(
    df = studenten,
    niveau = niveau,
    gemeente_kolom = gemeente_kolom,
    postcode_kolom = postcode_kolom,
    gemeenten = laad_geo_data(config, "gemeente"),
    pc6_mapping = if (niveau == "wijk") laad_pc6_mapping(config)
  )

  code_kolom <- if (niveau == "gemeente") "GM_CODE" else "WK_CODE"
  rapport <- maak_koppelrapport(gekoppeld, geo[[code_kolom]], n_voor_filter)
  rapport$filters_overgeslagen <- attr(studenten, "overgeslagen")
  rapport$weggefilterd_per_filter <- attr(studenten, "weggefilterd")

  data <- merge_studenten_geo(geo, aggregeer_studenten(gekoppeld, niveau), niveau)

  # [4] Categoriseren
  voortgang(0.2, "Categoriseren...")
  breaks_info <- bepaal_breaks(
    data = data$aantal,
    preset = config$breaks_preset,
    eigen = unlist(config$breaks)
  )
  data <- categoriseer_aantallen(data, breaks_info)

  # [5] Plot
  voortgang(0.2, "Kaart genereren...")
  plot <- maak_choropleth(
    data = data,
    config = config,
    breaks_info = breaks_info,
    locaties_sf = maak_locaties_sf(config, sf::st_crs(data)),
    niveau = niveau
  )

  # [6] Tabel
  voortgang(0.1, "Tabel genereren...")
  tabel_data <- maak_tabel_data(data, niveau, min_aantal)

  list(
    plot = plot,
    tabel = maak_tabel(tabel_data, config, niveau, min_aantal),
    tabel_data = tabel_data,
    data = data,
    breaks_info = breaks_info,
    rapport = rapport,
    config = config
  )
}


#' Vat koppelrapport samen in één zin
#'
#' @param rapport List uit maak_koppelrapport()
#'
#' @return Character
#'
#' @export
beschrijf_koppelrapport <- function(rapport) {

  tekst <- sprintf(
    "%d van %d studenten op de kaart; %d wonen buiten het gebied; %d niet herkend.",
    rapport$op_kaart, rapport$na_filter, rapport$buiten_gebied, rapport$niet_herkend
  )
  if (rapport$na_filter < rapport$voor_filter) {
    per_filter <- rapport$weggefilterd_per_filter
    per_filter <- per_filter[per_filter > 0]
    door <- if (length(per_filter) == 1) {
      paste(" door filter", names(per_filter))
    } else if (length(per_filter) > 1) {
      paste(" door filters", paste0(names(per_filter), " (", per_filter, ")", collapse = ", "))
    } else {
      ""
    }
    tekst <- paste0(
      tekst, " (", rapport$voor_filter - rapport$na_filter, " weggefilterd", door, ")"
    )
  }
  if (length(rapport$filters_overgeslagen) > 0) {
    tekst <- paste0(
      tekst, " Filter overgeslagen, kolom ontbreekt: ",
      paste(rapport$filters_overgeslagen, collapse = ", "), "."
    )
  }
  tekst
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
#' Slaat gt tabel op als PNG. Vereist het package webshot2 en Chrome/Chromium.
#'
#' @param tabel gt object
#' @param path Character, volledig pad voor output
#'
#' @export
sla_tabel_op <- function(tabel, path) {

  gt::gtsave(tabel, filename = path)

  message(paste("Tabel opgeslagen:", path))
}
