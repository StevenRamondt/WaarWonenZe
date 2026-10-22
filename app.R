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
profielen <- profiel_keuzes()
config_default <- laad_config("default")

# Controleer referentiedata (de app start ook als er iets ontbreekt)
ontbrekend <- ontbrekende_referentiedata(config_default)

BREAKS_KEUZES <- c(
  "Automatisch" = "auto",
  "Grote aantallen (1-200 ... 8001-10000)" = "veel",
  "Middelgrote aantallen (1-20 ... 1001-2000)" = "middel",
  "Kleine aantallen (1-5 ... 81-100)" = "weinig",
  "Eigen klassegrenzen" = "eigen"
)

filter_id <- function(kolom) paste0("filterwaarde_", make.names(kolom))


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
      .warning-box { color: #7d6608; background-color: #fef9e7; border: 1px solid #f1c40f;
                    padding: 10px; border-radius: 5px; margin: 10px 0; }
      .info-line { color: #7f8c8d; margin: 5px 0 10px 0; }
      .data-links { margin-top: -10px; margin-bottom: 10px; }
    "))
  ),

  titlePanel(
    div(
      h2("Choropleth Kaarten"),
      p("Visualisatie van studentenherkomst per gemeente of wijk",
        style = "color: #7f8c8d; margin-top: -5px;")
    ),
    windowTitle = "Choropleth Kaarten"
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Data"),
      fileInput("studenten_file", "Studenten data (.xlsx)",
                accept = c(".xlsx", ".xls")),
      div(
        class = "data-links",
        actionLink("gebruik_demo", "Gebruik demodata", icon = icon("flask")),
        " · ",
        downloadLink("download_template", "Download template")
      ),
      uiOutput("data_info"),

      hr(),
      h4("Gebied"),

      selectInput("profiel", "Profiel",
                  choices = profielen,
                  selected = profielen[1]),

      selectInput("niveau", "Niveau",
                  choices = c("gemeente", "wijk")),

      conditionalPanel(
        "input.niveau == 'gemeente'",
        uiOutput("gemeente_kolom_ui")
      ),
      conditionalPanel(
        "input.niveau == 'wijk'",
        uiOutput("postcode_kolom_ui")
      ),

      hr(),
      h4("Filters"),
      uiOutput("filter_kolommen_ui"),
      uiOutput("filter_waarden_ui"),

      hr(),
      h4("Weergave"),

      selectInput("breaks_preset", "Klassen",
                  choices = BREAKS_KEUZES,
                  selected = "auto"),
      conditionalPanel(
        "input.breaks_preset == 'eigen'",
        textInput("eigen_breaks", "Klassegrenzen (komma-gescheiden)",
                  placeholder = "1, 20, 150, 500, 2000")
      ),

      selectInput("kleuren_palet", "Kleurenpalet",
                  choices = c("Blues", "Reds", "Greens", "Oranges", "Purples",
                              "YlOrRd", "YlGnBu", "RdPu", "BuGn", "BuPu"),
                  selected = "Blues"),

      checkboxInput("toon_labels", "Toon namen op kaart", value = FALSE),

      textInput("titel", "Titel kaart & tabel"),
      textInput("peildatum", "Peildatum (optioneel)"),
      numericInput("min_aantal", "Min. aantal voor tabel", value = 10, min = 1),

      actionButton("generate", "Genereer kaart & tabel",
                   class = "btn-primary btn-block",
                   icon = icon("map"))
    ),

    mainPanel(
      width = 9,
      if (length(ontbrekend) > 0) {
        div(
          class = "error-box", icon("exclamation-triangle"),
          " Referentiedata ontbreekt: ", paste(ontbrekend, collapse = ", "),
          ". Run eerst ", code("Rscript scripts/prepare_data.R"), "."
        )
      },
      uiOutput("error_msg"),
      uiOutput("koppel_msg"),
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
          verbatimTextOutput("samenvatting"),
          h4("Niet herkende waarden"),
          p("Studenten met deze gemeente of postcode staan niet op de kaart.",
            class = "info-line"),
          downloadButton("download_niet_herkend", "Download (CSV)"),
          br(), br(),
          tableOutput("niet_herkend")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --- Studenten data: upload of demo ---
  bron <- reactiveVal(NULL)

  observeEvent(input$studenten_file, {
    tryCatch(
      bron(list(
        df = laad_studenten_data(input$studenten_file$datapath),
        naam = input$studenten_file$name
      )),
      error = function(e) error_state(paste("Kan bestand niet lezen:", e$message))
    )
  })

  observeEvent(input$gebruik_demo, {
    bron(list(
      df = laad_studenten_data(here::here(config_default$data$demo)),
      naam = "demodata (fictief)"
    ))
  })

  studenten_data <- reactive({
    req(bron())$df
  })

  output$data_info <- renderUI({
    req(bron())
    df <- bron()$df
    p(class = "info-line",
      icon("table"), " ", bron()$naam, ": ", format(nrow(df), big.mark = ".", decimal.mark = ","),
      " rijen, ", ncol(df), " kolommen")
  })

  output$download_template <- downloadHandler(
    filename = "template_studenten.xlsx",
    content = function(file) {
      file.copy(here::here("data", "voorbeeld", "template.xlsx"), file)
    }
  )

  # --- Beschikbare kolommen ---
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
    selectInput("postcode_kolom", "Postcode kolom",
                choices = kolommen, selected = default)
  })

  # --- Profiel config ---
  profiel_config <- reactive({
    laad_config(input$profiel)
  })

  # --- Update defaults wanneer profiel wijzigt ---
  observeEvent(input$profiel, {
    config <- profiel_config()

    updateSelectInput(session, "niveau", selected = config$niveau %||% "gemeente")
    updateSelectInput(session, "kleuren_palet",
                      selected = config$visualisatie$kleuren_palet %||% "Blues")
    updateCheckboxInput(session, "toon_labels",
                        value = isTRUE(config$visualisatie$toon_labels))
    updateSelectInput(session, "breaks_preset",
                      selected = config$breaks_preset %||% "auto")
    updateTextInput(session, "eigen_breaks",
                    value = paste(unlist(config$breaks), collapse = ", "))
    updateTextInput(session, "titel", value = config$titel %||% "")
    updateTextInput(session, "peildatum", value = config$peildatum %||% "")
    updateNumericInput(session, "min_aantal", value = config$tabel$min_aantal %||% 10)
  })

  # --- Filters ---
  output$filter_kolommen_ui <- renderUI({
    df <- studenten_data()
    keuzes <- filterbare_kolommen(df)
    standaard <- intersect(names(profiel_config()$filters), keuzes)

    selectizeInput("filter_kolommen", "Filter op kolommen",
                   choices = keuzes, selected = standaard, multiple = TRUE,
                   options = list(placeholder = "Geen filter"))
  })

  output$filter_waarden_ui <- renderUI({
    df <- studenten_data()
    config_filters <- profiel_config()$filters

    lapply(input$filter_kolommen, function(kolom) {
      waarden <- sort(unique(as.character(df[[kolom]])))
      waarden <- waarden[!is.na(waarden)]

      # Behoud bestaande keuze; anders config-default; anders alles
      geselecteerd <- isolate(input[[filter_id(kolom)]]) %||%
        intersect(as.character(config_filters[[kolom]]), waarden)
      if (length(geselecteerd) == 0) geselecteerd <- waarden

      selectizeInput(filter_id(kolom), kolom,
                     choices = waarden, selected = geselecteerd, multiple = TRUE)
    })
  })

  # --- Bouw config op basis van UI inputs ---
  active_config <- reactive({
    config <- profiel_config()

    # Override met UI waarden
    config$niveau <- input$niveau
    config$breaks_preset <- input$breaks_preset
    if (input$breaks_preset == "eigen") {
      config$breaks <- parse_breaks(input$eigen_breaks)
    }
    config$titel <- input$titel
    config$peildatum <- input$peildatum
    config$tabel$min_aantal <- input$min_aantal
    config$visualisatie$kleuren_palet <- input$kleuren_palet
    config$visualisatie$toon_labels <- input$toon_labels
    config$filters <- stats::setNames(
      lapply(input$filter_kolommen, function(k) input[[filter_id(k)]]),
      input$filter_kolommen
    )

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
    error_state(NULL)

    if (is.null(bron())) {
      error_state("Upload eerst studentendata of kies 'Gebruik demodata'.")
      return(NULL)
    }

    studenten <- studenten_data()
    kolom <- if (input$niveau == "wijk") input$postcode_kolom else input$gemeente_kolom
    req(kolom)

    tryCatch({
      config <- active_config()

      withProgress(message = "Bezig met berekenen...", value = 0, {
        genereer_kaart(
          studenten = studenten,
          config = config,
          gemeente_kolom = input$gemeente_kolom,
          postcode_kolom = input$postcode_kolom,
          voortgang = function(fractie, detail) incProgress(fractie, detail = detail)
        )
      })
    }, error = function(e) {
      error_state(paste("Fout bij genereren:", conditionMessage(e)))
      NULL
    })
  })

  # --- Koppelrapport melding ---
  output$koppel_msg <- renderUI({
    req(resultaten())
    rapport <- resultaten()$rapport
    tekst <- beschrijf_koppelrapport(rapport)

    aandeel_niet_herkend <- rapport$niet_herkend / max(rapport$na_filter, 1)
    if (aandeel_niet_herkend > 0.05 || length(rapport$filters_overgeslagen) > 0) {
      div(class = "warning-box", icon("exclamation-circle"), " ", tekst,
          " Zie tab Info voor details.")
    } else {
      p(class = "info-line", icon("info-circle"), " ", tekst)
    }
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
    rapport <- res$rapport

    filters <- if (length(config$filters) == 0) {
      "geen"
    } else {
      paste(names(config$filters), vapply(config$filters, paste, character(1), collapse = "/"),
            sep = " = ", collapse = "; ")
    }

    paste0(
      "Profiel: ", input$profiel, "\n",
      "Gebied: ", config$gebied$naam, "\n",
      "Niveau: ", config$niveau, "\n",
      "Titel: ", config$titel, "\n",
      "Peildatum: ", config$peildatum, "\n",
      "Filters: ", filters, "\n",
      "Klassen: ", config$breaks_preset, "\n",
      "Kleurenpalet: ", config$visualisatie$kleuren_palet, "\n",
      "\n--- Studenten ---\n",
      "In bestand: ", rapport$voor_filter, "\n",
      "Na filters: ", rapport$na_filter, "\n",
      "Op de kaart: ", rapport$op_kaart, "\n",
      "Buiten het gebied: ", rapport$buiten_gebied, "\n",
      "Niet herkend: ", rapport$niet_herkend, "\n",
      "\n--- Kaart ---\n",
      "Aantal gebieden: ", nrow(data), "\n",
      "Gebieden met studenten: ", sum(data$aantal > 0, na.rm = TRUE), "\n",
      "Max studenten in 1 gebied: ", max(data$aantal, na.rm = TRUE), "\n",
      "Categorieën: ", paste(res$breaks_info$labels, collapse = ", "), "\n"
    )
  })

  output$niet_herkend <- renderTable({
    req(resultaten())
    utils::head(resultaten()$rapport$niet_herkend_waarden, 50)
  })

  # --- Downloads ---
  output$download_plot <- downloadHandler(
    filename = function() {
      config <- resultaten()$config
      genereer_bestandsnaam(config, config$niveau, "plot")
    },
    content = function(file) {
      sla_plot_op(resultaten()$plot, file)
    }
  )

  output$download_tabel_png <- downloadHandler(
    filename = function() {
      config <- resultaten()$config
      genereer_bestandsnaam(config, config$niveau, "tabel")
    },
    content = function(file) {
      tryCatch(
        gt::gtsave(resultaten()$tabel, filename = file),
        error = function(e) {
          showNotification(
            "PNG van de tabel vereist het package webshot2 en Chrome. Gebruik anders de CSV.",
            type = "error", duration = 10
          )
          stop(e)
        }
      )
    }
  )

  output$download_tabel_csv <- downloadHandler(
    filename = function() {
      config <- resultaten()$config
      genereer_bestandsnaam(config, config$niveau, "tabel", extensie = "csv")
    },
    content = function(file) {
      write.csv(resultaten()$tabel_data, file, row.names = FALSE)
    }
  )

  output$download_niet_herkend <- downloadHandler(
    filename = "niet_herkend.csv",
    content = function(file) {
      write.csv(resultaten()$rapport$niet_herkend_waarden, file, row.names = FALSE)
    }
  )
}

# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
