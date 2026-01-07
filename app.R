# Packages
library(shiny)
library(shinyWidgets)
library(bslib)
library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Data connection
db_yearly <- open_dataset(sources = "data/banco_anual_tempmed.parquet")
db_daily <- open_dataset(sources = "data/banco_diario_tempmed.parquet")

# Read municipality data
mun_seats <- readRDS("data/mun_seats.rds")

# Municipality list for selector
mun_names <- mun_seats$code_muni
names(mun_names) <- paste0(mun_seats$name_muni, ", ", mun_seats$abbrev_state)

# Interface
ui <- page_navbar(
  title = "Ondas de Calor",
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'pin_obs_horizontal.png\' align=\'right\' height = \'57.5px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )
  ),

  # Translation
  tags$script(
    HTML(
      "
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    "
    )
  ),

  # Map page
  nav_panel(
    title = "Dados anuais",

    # Sidebar
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput(
          inputId = "municipality",
          label = "Município",
          choices = NULL
        ),
        selectInput(
          inputId = "intensity",
          label = "Intensidade da onda de calor",
          choices = c("95", "92.5", "97.5")
        )
      ),

      # Yearly graph
      card(
        full_screen = TRUE,
        plotOutput(outputId = "yearly_graph")
      )
    )
  ),

  # Graphs page
  nav_panel(
    title = "Dados diários",

    layout_sidebar(
      sidebar = sidebar(),

      # Graphs card
      card(
        full_screen = TRUE,
        card_header("Card header"),
        card_body()
      )
    )
  ),

  # About page
  nav_panel(
    title = "Sobre o projeto",
    card(
      card_header("Card title"),
      p("Bla bla bla.")
    ),
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Título A",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título B",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título C",
        p("Bla bla bla.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Municipality selector
  updateSelectizeInput(
    session = session,
    inputId = "municipality",
    label = "Município",
    choices = mun_names,
    server = TRUE
  )

  # Daily data
  daily_data <- reactive({
    req(input$municipality)
    req(input$intensity)

    db_daily |>
      mutate(CODMUNRES = substr(as.character(CODMUNRES), 0, 6)) |>
      filter(CODMUNRES == input$municipality) |>
      filter(intensidade == input$intensity) |>
      collect()
  })

  # Yearly data
  yearly_data <- reactive({
    req(input$municipality)
    req(input$intensity)

    db_yearly |>
      mutate(CODMUNRES = substr(as.character(CODMUNRES), 0, 6)) |>
      filter(CODMUNRES == input$municipality) |>
      filter(intensidade == input$intensity) |>
      mutate(mes_mais_frequente = as.numeric(mes_mais_frequente)) |>
      collect()
  })

  # Yearly graph
  output$yearly_graph <- renderPlot({
    # intensidade, n_eventos, duracao_media, duracao_maxima, mes_mais_frequente, tmed_media_ondas, tmed_max_ondas, intensidade_media

    res <- yearly_data() |>
      select(
        ano,
        n_eventos,
        duracao_media,
        intensidade_media
      ) |>
      pivot_longer(
        cols = c(
          n_eventos,
          duracao_media,
          intensidade_media
        )
      ) |>
      mutate(
        name = case_match(
          name,
          "n_eventos" ~ "N. de eventos",
          "duracao_media" ~ "Duração média (dias consecutivos)",
          "intensidade_media" ~ "Intensidade média (°C)"
        )
      ) |>
      arrange(ano, name)

    ggplot(data = res, aes(x = ano, y = value, colour = name)) +
      geom_line() +
      facet_wrap(~name, scales = "free_y", ncol = 1) +
      theme_bw() +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
