# =============================================================================
# Shiny App - Choropleth Kaarten
# =============================================================================

library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(yaml)
library(gt)
library(RColorBrewer)
library(purrr)
library(here)

# Laad functies
source(here::here("R", "functions.R"))

# Laad config profielen
config_all <- yaml::read_yaml(here::here("config.yml"))
profielen <- setdiff(names(config_all), "default")

# Pre-load referentie data (altijd nodig)
gemeente_ref <- laad_gemeente_ref(
  path = here::here("data/reference/Gemeenten alfabetisch 2023.xlsx")
)


# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f5f5; }
      .sidebar-panel { background-color: white; border-radius: 8px; padding: 20px; }
      .main-panel { background-color: white; border-radius: 8px; padding: 20px; }
      h2 { color: #2c3e50; }
      .well { background-color: #ffffff; border: 1px solid #ddd; }
      .nav-tabs > li.active > a { background-color: #2c3e50; color: white; }
      #generate { margin-top: 15px; }
      .error-box { color: #c0392b; background-color: #fdedec; border: 1px solid #e74c3c;
                    padding: 10px; border-radius: 5px; margin: 10px 0; }
    "))
  ),

  titlePanel(
    div(
      h2("Choropleth Kaarten"),
      p("Visualisatie van studentenherkomst per gemeente of wijk",
        style = "color: #7f8c8d; margin-top: -5px;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Data"),
      fileInput("studenten_file", "Studenten data (.xlsx)",
                accept = c(".xlsx", ".xls")),

      hr(),
      h4("Kolom mapping"),
      uiOutput("gemeente_kolom_ui"),
      uiOutput("postcode_kolom_ui"),

      hr(),
      h4("Configuratie"),

      selectInput("profiel", "Profiel (gebied)",
                  choices = profielen,
                  selected = profielen[1]),

      uiOutput("niveau_ui"),

      selectInput("breaks_preset", "Breaks preset",
                  choices = c("auto", "veel", "middel", "weinig"),
                  selected = "veel"),

      selectInput("kleuren_palet", "Kleurenpalet",
                  choices = c("Blues", "Reds", "Greens", "Oranges", "Purples",
                              "YlOrRd", "YlGnBu", "RdPu", "BuGn", "BuPu"),
                  selected = "Blues"),

      checkboxInput("toon_labels", "Toon labels op kaart", value = FALSE),

      hr(),
      h4("Titel"),
      textInput("titel", "Titel kaart & tabel", value = "ROC Mondriaan studenten"),
      textInput("peildatum", "Peildatum (optioneel)", value = "1 okt 2025"),

      hr(),
      numericInput("min_aantal", "Min. aantal voor tabel", value = 10, min = 1),

      hr(),
      actionButton("generate", "Genereer kaart & tabel",
                   class = "btn-primary btn-block",
                   icon = icon("map"))
    ),

    mainPanel(
      width = 9,
      uiOutput("error_msg"),
      tabsetPanel(
        id = "tabs",
        type = "tabs",

        tabPanel(
          "Kaart",
          br(),
          div(
            downloadButton("download_plot", "Download kaart (PNG)"),
            style = "margin-bottom: 15px;"
          ),
          plotOutput("choropleth", height = "700px")
        ),

        tabPanel(
          "Tabel",
          br(),
          div(
            downloadButton("download_tabel_png", "Download tabel (PNG)"),
            downloadButton("download_tabel_csv", "Download tabel (CSV)"),
            style = "margin-bottom: 15px;"
          ),
          gt::gt_output("tabel")
        ),

        tabPanel(
          "Info",
          br(),
          h4("Samenvatting"),
          verbatimTextOutput("samenvatting")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --- Reactive: studenten data ---
  studenten_data <- reactive({
    req(input$studenten_file)
    laad_studenten_data(input$studenten_file$datapath)
  })

  # --- Reactive: beschikbare kolommen ---
  beschikbare_kolommen <- reactive({
    colnames(studenten_data())
  })

  # --- Dynamische kolom selectie UI ---
  output$gemeente_kolom_ui <- renderUI({
    kolommen <- beschikbare_kolommen()
    # Probeer beste default te vinden
    default <- if ("Gemeente" %in% kolommen) {
      "Gemeente"
    } else {
      matches <- grep("gemeente|municipality|gem", kolommen, ignore.case = TRUE, value = TRUE)
      if (length(matches) > 0) matches[1] else kolommen[1]
    }
    selectInput("gemeente_kolom", "Gemeente kolom",
                choices = kolommen, selected = default)
  })

  output$postcode_kolom_ui <- renderUI({
    kolommen <- beschikbare_kolommen()
    default <- if ("Postcode" %in% kolommen) {
      "Postcode"
    } else {
      matches <- grep("postcode|pc6|pc4", kolommen, ignore.case = TRUE, value = TRUE)
      if (length(matches) > 0) matches[1] else kolommen[1]
    }
    selectInput("postcode_kolom", "Postcode kolom (voor wijkniveau)",
                choices = kolommen, selected = default)
  })

  # --- Dynamisch niveau op basis van profiel ---
  output$niveau_ui <- renderUI({
    config <- laad_config(input$profiel)
    default_niveau <- config$niveau %||% "gemeente"
    selectInput("niveau", "Niveau",
                choices = c("gemeente", "wijk"),
                selected = default_niveau)
  })

  # --- Update defaults wanneer profiel wijzigt ---
  observeEvent(input$profiel, {
    config <- laad_config(input$profiel)

    updateSelectInput(session, "kleuren_palet",
                      selected = config$visualisatie$kleuren_palet %||% "Blues")
    updateCheckboxInput(session, "toon_labels",
                        value = isTRUE(config$visualisatie$toon_labels))
    updateSelectInput(session, "breaks_preset",
                      selected = config$breaks_preset %||% "veel")
  })

  # --- Bouw config op basis van UI inputs ---
  active_config <- reactive({
    req(input$niveau)
    config <- laad_config(input$profiel)

    # Override met UI waarden
    config$niveau <- input$niveau
    config$breaks_preset <- input$breaks_preset
    config$titel <- input$titel
    config$peildatum <- input$peildatum
    config$visualisatie$kleuren_palet <- input$kleuren_palet
    config$visualisatie$toon_labels <- input$toon_labels

    config
  })

  # --- Error state ---
  error_state <- reactiveVal(NULL)

  output$error_msg <- renderUI({
    err <- error_state()
    if (!is.null(err)) {
      div(class = "error-box", icon("exclamation-triangle"), " ", err)
    }
  })

  # --- Bereken resultaten ---
  resultaten <- eventReactive(input$generate, {
    req(input$niveau, input$gemeente_kolom, input$postcode_kolom)
    error_state(NULL)

    config <- active_config()
    studenten <- studenten_data()

    # Valideer kolommen
    if (config$niveau == "wijk") {
      if (!input$postcode_kolom %in% colnames(studenten)) {
        error_state(paste0(
          "Kolom '", input$postcode_kolom, "' niet gevonden in studenten data. ",
          "Beschikbare kolommen: ", paste(colnames(studenten), collapse = ", "),
          ". Pas de 'Postcode kolom' aan of kies niveau 'gemeente'."
        ))
        return(NULL)
      }
    } else {
      if (!input$gemeente_kolom %in% colnames(studenten)) {
        error_state(paste0(
          "Kolom '", input$gemeente_kolom, "' niet gevonden in studenten data. ",
          "Beschikbare kolommen: ", paste(colnames(studenten), collapse = ", "),
          ". Pas de 'Gemeente kolom' aan."
        ))
        return(NULL)
      }
    }

    tryCatch({
      withProgress(message = "Bezig met berekenen...", value = 0, {

        # [1] Geo data laden
        incProgress(0.1, detail = "Geo data laden...")
        geo <- laad_geo_data(
          gpkg_path = here::here(config$data$gpkg),
          niveau = config$niveau
        )

        # [2] Filteren
        incProgress(0.2, detail = "Data filteren...")
        geo_filtered <- filter_geo_data(
          geo = geo,
          config = config,
          gemeente_ref = gemeente_ref
        )

        code_kolom <- if (config$niveau == "gemeente") "GM_CODE" else "WK_CODE"
        geo_filtered <- verwijder_dubbele_geometrie(geo_filtered, code_kolom)

        # [3] Aggregeren
        incProgress(0.2, detail = "Studenten aggregeren...")

        # Laad PC6-wijk mapping indien wijk niveau
        pc6_mapping <- NULL
        if (config$niveau == "wijk") {
          pc6_mapping <- maak_postcode_wijk_mapping(
            path = here::here(config$data$pc6_wijk_ref)
          )
        }

        studenten_agg <- aggregeer_studenten(
          df = studenten,
          niveau = config$niveau,
          gemeente_kolom = input$gemeente_kolom,
          postcode_kolom = input$postcode_kolom,
          pc6_wijk_mapping = pc6_mapping
        )

        data <- merge_studenten_geo(
          geo = geo_filtered,
          studenten_agg = studenten_agg,
          niveau = config$niveau
        )

        # [4] Categoriseren
        incProgress(0.2, detail = "Categoriseren...")
        breaks_info <- bepaal_breaks(
          data = data$aantal,
          preset = config$breaks_preset
        )

        data <- categoriseer_aantallen(data, breaks_info)

        # [5] Plot
        incProgress(0.2, detail = "Kaart genereren...")
        locaties_sf <- maak_locaties_sf(config, sf::st_crs(data))

        plot <- maak_choropleth(
          data = data,
          config = config,
          breaks_info = breaks_info,
          locaties_sf = locaties_sf,
          niveau = config$niveau
        )

        # [6] Tabel
        incProgress(0.1, detail = "Tabel genereren...")
        tabel <- maak_tabel(
          data = data,
          config = config,
          niveau = config$niveau,
          min_aantal = input$min_aantal
        )

        # Tabel data voor CSV export
        naam_kolom <- if (config$niveau == "gemeente") "GM_NAAM" else "WK_NAAM"
        tabel_csv <- data |>
          sf::st_drop_geometry() |>
          dplyr::select(dplyr::all_of(naam_kolom), aantal) |>
          dplyr::filter(aantal >= input$min_aantal) |>
          dplyr::arrange(dplyr::desc(aantal))

        list(
          plot = plot,
          tabel = tabel,
          tabel_csv = tabel_csv,
          data = data,
          breaks_info = breaks_info,
          config = config
        )
      })
    }, error = function(e) {
      error_state(paste("Fout bij genereren:", e$message))
      NULL
    })
  })

  # --- Output: Choropleth ---
  output$choropleth <- renderPlot({
    req(resultaten())
    resultaten()$plot
  }, res = 96)

  # --- Output: Tabel ---
  output$tabel <- gt::render_gt({
    req(resultaten())
    resultaten()$tabel
  })

  # --- Output: Samenvatting ---
  output$samenvatting <- renderText({
    req(resultaten())
    res <- resultaten()
    config <- res$config
    data <- res$data

    paste0(
      "Profiel: ", input$profiel, "\n",
      "Gebied: ", config$gebied$naam, "\n",
      "Niveau: ", config$niveau, "\n",
      "Titel: ", config$titel, "\n",
      "Peildatum: ", config$peildatum, "\n",
      "Breaks preset: ", config$breaks_preset, "\n",
      "Kleurenpalet: ", config$visualisatie$kleuren_palet, "\n",
      "Gemeente kolom: ", input$gemeente_kolom, "\n",
      "Postcode kolom: ", input$postcode_kolom, "\n",
      "\n--- Data ---\n",
      "Aantal gebieden op kaart: ", nrow(data), "\n",
      "Totaal studenten in gebied: ", sum(data$aantal, na.rm = TRUE), "\n",
      "Gebieden met studenten: ", sum(data$aantal > 0, na.rm = TRUE), "\n",
      "Max studenten in 1 gebied: ", max(data$aantal, na.rm = TRUE), "\n",
      "\n--- Categorieën ---\n",
      paste(res$breaks_info$labels, collapse = ", "), "\n"
    )
  })

  # --- Downloads ---
  output$download_plot <- downloadHandler(
    filename = function() {
      config <- resultaten()$config
      genereer_bestandsnaam(config, config$niveau, "plot")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = resultaten()$plot,
        width = 13,
        height = 10,
        units = "in",
        bg = "white"
      )
    }
  )

  output$download_tabel_png <- downloadHandler(
    filename = function() {
      config <- resultaten()$config
      genereer_bestandsnaam(config, config$niveau, "tabel")
    },
    content = function(file) {
      gt::gtsave(resultaten()$tabel, filename = file)
    }
  )

  output$download_tabel_csv <- downloadHandler(
    filename = function() {
      config <- resultaten()$config
      genereer_bestandsnaam(config, config$niveau, "tabel", extensie = "csv")
    },
    content = function(file) {
      write.csv(resultaten()$tabel_csv, file, row.names = FALSE)
    }
  )
}

# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
