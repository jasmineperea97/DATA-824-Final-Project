# app.R
# Alaska Rural Water & Wastewater Infrastructure Explorer

required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "readr", "stringr",
  "ggplot2", "leaflet", "DT", "scales", "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# If clean data are missing, download them automatically.
needed_files <- c(
  file.path("data", "pws_sources_clean.csv"),
  file.path("data", "cwa_facilities_clean.csv"),
  file.path("data", "npdes_outfalls_clean.csv"),
  file.path("data", "boil_water_notices_clean.csv")
)

if (!all(file.exists(needed_files))) {
  message("Clean data files were not found. Running 02_download_clean_data.R ...")
  source("02_download_clean_data.R")
}

read_app_csv <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }
}

ensure_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df
}

has_coords <- function(df) {
  df %>%
    filter(is.finite(latitude), is.finite(longitude))
}

pws <- read_app_csv(file.path("data", "pws_sources_clean.csv")) %>%
  ensure_cols(c("row_id", "pws_id", "system_name", "source_type", "federal_type",
                "population_served", "latitude", "longitude"))

cwa <- read_app_csv(file.path("data", "cwa_facilities_clean.csv")) %>%
  ensure_cols(c("row_id", "npdes_id", "facility_name", "city", "county",
                "permit_status", "permit_type", "total_design_flow",
                "actual_average_flow", "inspections", "pct_low_income_3mi",
                "pct_people_of_color", "population_density",
                "has_tribal_context", "has_current_violation",
                "latitude", "longitude"))

outfalls <- read_app_csv(file.path("data", "npdes_outfalls_clean.csv")) %>%
  ensure_cols(c("row_id", "npdes_id", "facility_name", "county", "permit_status",
                "state_water_body_name", "total_design_flow", "has_tribal_context",
                "has_current_violation", "latitude", "longitude"))

notices <- read_app_csv(file.path("data", "boil_water_notices_clean.csv")) %>%
  ensure_cols(c("row_id", "pws_id", "system_name", "notice_type", "notice_status",
                "latitude", "longitude"))

choice_all <- function(x) {
  vals <- sort(unique(stats::na.omit(as.character(x))))
  vals <- vals[vals != ""]
  c("All", vals)
}

pws_pop_max <- suppressWarnings(max(pws$population_served, na.rm = TRUE))
if (!is.finite(pws_pop_max)) pws_pop_max <- 1000
pws_pop_max <- max(1000, ceiling(pws_pop_max))

theme_clean <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

empty_plot <- function(message) {
  plot.new()
  text(0.5, 0.5, message, cex = 1.1)
}

value_card <- function(title, output_id) {
  div(
    class = "value-card",
    h4(title),
    h2(textOutput(output_id, inline = TRUE))
  )
}

ui <- navbarPage(
  title = "Alaska Water & Wastewater Explorer",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

  header = tags$head(
    tags$style(HTML("
      .value-card {
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        padding: 16px;
        margin-bottom: 12px;
        background: #f8fbfd;
        min-height: 115px;
      }
      .value-card h4 {
        margin-top: 0;
        color: #345;
        font-size: 16px;
      }
      .value-card h2 {
        font-weight: 700;
        margin-top: 8px;
      }
      .note-box {
        border-left: 5px solid #2c7fb8;
        padding: 12px 16px;
        background: #eef7fb;
        margin-bottom: 15px;
      }
    "))
  ),

  tabPanel(
    "Overview",
    fluidPage(
      br(),
      div(
        class = "note-box",
        strong("Project purpose: "),
        "This app explores public water-system sources, wastewater/NPDES facilities, discharge points, and active water notices in Alaska. It is designed for a final data product project using public real-world data."
      ),
      fluidRow(
        column(3, value_card("Water system source records", "n_pws")),
        column(3, value_card("Wastewater/NPDES facilities", "n_cwa")),
        column(3, value_card("NPDES outfall records", "n_outfalls")),
        column(3, value_card("Open boil-water / do-not-drink notices", "n_notices"))
      ),
      fluidRow(
        column(6, plotOutput("overview_pws_bar", height = 350)),
        column(6, plotOutput("overview_cwa_bar", height = 350))
      )
    )
  ),

  tabPanel(
    "Interactive Map",
    sidebarLayout(
      sidebarPanel(
        h4("Map filters"),
        checkboxGroupInput(
          "map_layers",
          "Layers to show",
          choices = c(
            "Public water system sources" = "pws",
            "CWA/NPDES wastewater facilities" = "cwa",
            "NPDES outfalls / discharge points" = "outfalls",
            "Open boil-water / do-not-drink notices" = "notices"
          ),
          selected = c("pws", "cwa")
        ),
        sliderInput(
          "population_range",
          "Population served by water system",
          min = 0,
          max = pws_pop_max,
          value = c(0, pws_pop_max),
          step = max(1, round(pws_pop_max / 100))
        ),
        selectInput("pws_type", "Water system classification", choices = choice_all(pws$federal_type)),
        selectInput("pws_source_type", "Water source type", choices = choice_all(pws$source_type)),
        selectInput("permit_status", "Wastewater/NPDES permit status", choices = choice_all(cwa$permit_status)),
        checkboxInput("tribal_context_only", "Wastewater layer: only records with tribal-context flag", FALSE),
        checkboxInput("violation_only", "Wastewater layer: only records with current violation/SNC flag", FALSE),
        helpText("Tip: Click points on the map for details.")
      ),
      mainPanel(
        leafletOutput("main_map", height = 720)
      )
    )
  ),

  tabPanel(
    "Water Systems",
    sidebarLayout(
      sidebarPanel(
        h4("Water-system filters"),
        selectInput("water_fed_filter", "Classification", choices = choice_all(pws$federal_type)),
        selectInput("water_source_filter", "Source type", choices = choice_all(pws$source_type)),
        sliderInput(
          "water_population_range",
          "Population served",
          min = 0,
          max = pws_pop_max,
          value = c(0, pws_pop_max),
          step = max(1, round(pws_pop_max / 100))
        )
      ),
      mainPanel(
        h3("Public water system source locations"),
        fluidRow(
          column(6, plotOutput("water_source_bar", height = 360)),
          column(6, plotOutput("water_population_hist", height = 360))
        ),
        h4("Filtered water-system records"),
        DT::DTOutput("water_table")
      )
    )
  ),

  tabPanel(
    "Wastewater / NPDES",
    sidebarLayout(
      sidebarPanel(
        h4("Wastewater filters"),
        selectInput("waste_status_filter", "Permit status", choices = choice_all(cwa$permit_status)),
        selectInput("waste_county_filter", "County", choices = choice_all(cwa$county)),
        checkboxInput("waste_tribal_context_only", "Only tribal-context records", FALSE),
        checkboxInput("waste_violation_only", "Only violation/SNC records", FALSE)
      ),
      mainPanel(
        h3("Wastewater / Clean Water Act facility patterns"),
        fluidRow(
          column(6, plotOutput("waste_status_bar", height = 360)),
          column(6, plotOutput("waste_flow_scatter", height = 360))
        ),
        fluidRow(
          column(6, plotOutput("waste_county_bar", height = 360)),
          column(6, plotOutput("outfall_waterbody_bar", height = 360))
        ),
        h4("Filtered wastewater/NPDES records"),
        DT::DTOutput("waste_table")
      )
    )
  ),

  tabPanel(
    "County Profile",
    fluidPage(
      br(),
      div(
        class = "note-box",
        "This tab summarizes wastewater/NPDES data by county. It adds a heat map and PCA-style view to show multivariable patterns, which supports the course emphasis on clustering/PCA-style visualization."
      ),
      fluidRow(
        column(6, plotOutput("county_heatmap", height = 560)),
        column(6, plotOutput("county_pca", height = 560))
      ),
      h4("County-level summary table"),
      DT::DTOutput("county_table")
    )
  ),

  tabPanel(
    "Data Quality",
    sidebarLayout(
      sidebarPanel(
        h4("Missingness explorer"),
        selectInput(
          "missing_dataset",
          "Choose dataset",
          choices = c(
            "Water system sources" = "pws",
            "Wastewater/NPDES facilities" = "cwa",
            "NPDES outfalls" = "outfalls",
            "Boil-water notices" = "notices"
          )
        ),
        helpText("Missing values are common in real public datasets. This tab helps show what information is complete and what should be interpreted carefully.")
      ),
      mainPanel(
        plotOutput("missingness_plot", height = 560),
        DT::DTOutput("missingness_table")
      )
    )
  ),

  tabPanel(
    "About",
    fluidPage(
      br(),
      h2("About this application"),
      p("This Shiny app maps and summarizes public data about Alaska drinking-water and wastewater infrastructure."),
      h3("What to expect from the app"),
      tags$ul(
        tags$li("An interactive map of water-system sources, wastewater/NPDES facilities, outfalls, and active water notices."),
        tags$li("Filters for population served, source type, permit status, tribal-context indicators, and violation indicators."),
        tags$li("Charts showing distributions by source type, permit status, county, and discharge location."),
        tags$li("A data-quality tab showing missing values and limitations.")
      ),
      h3("Data sources"),
      tags$ul(
        tags$li("Alaska DEC SDWIS active public water system source locations."),
        tags$li("EPA ECHO Clean Water Act/NPDES facility locations."),
        tags$li("EPA ECHO NPDES outfalls/permitted features."),
        tags$li("Alaska DEC open boil-water or do-not-drink notices.")
      ),
      h3("Important limitation"),
      p("The app does not label every location as an Alaska Native Village. Instead, it uses official water and wastewater datasets that are relevant to rural and tribal Alaska contexts. EPA ECHO includes some tribal-spatial-association variables, but public infrastructure datasets do not always perfectly identify every Alaska Native village or service area."),
      h3("How this connects to the final project rubric"),
      tags$ul(
        tags$li("Background: explains why sanitation infrastructure in rural Alaska matters."),
        tags$li("Data: uses multiple official public datasets."),
        tags$li("Insight: lets users explore where systems, permits, notices, and compliance issues appear."),
        tags$li("Principles: uses maps, bar charts, scatter plots, heat maps, and tables in ways that match the data types."),
        tags$li("Story: focuses on infrastructure access, public health, and rural/tribal environmental context.")
      )
    )
  )
)

server <- function(input, output, session) {

  # Value boxes ---------------------------------------------------------------

  output$n_pws <- renderText(scales::comma(nrow(pws)))
  output$n_cwa <- renderText(scales::comma(nrow(cwa)))
  output$n_outfalls <- renderText(scales::comma(nrow(outfalls)))
  output$n_notices <- renderText(scales::comma(nrow(notices)))

  # Shared filtered data ------------------------------------------------------

  pws_filtered <- reactive({
    df <- pws %>%
      filter(
        is.na(population_served) |
          (population_served >= input$population_range[1] &
             population_served <= input$population_range[2])
      )

    if (!is.null(input$pws_type) && input$pws_type != "All") {
      df <- df %>% filter(federal_type == input$pws_type)
    }
    if (!is.null(input$pws_source_type) && input$pws_source_type != "All") {
      df <- df %>% filter(source_type == input$pws_source_type)
    }

    df
  })

  water_tab_filtered <- reactive({
    df <- pws %>%
      filter(
        is.na(population_served) |
          (population_served >= input$water_population_range[1] &
             population_served <= input$water_population_range[2])
      )

    if (!is.null(input$water_fed_filter) && input$water_fed_filter != "All") {
      df <- df %>% filter(federal_type == input$water_fed_filter)
    }
    if (!is.null(input$water_source_filter) && input$water_source_filter != "All") {
      df <- df %>% filter(source_type == input$water_source_filter)
    }

    df
  })

  cwa_filtered_map <- reactive({
    df <- cwa

    if (!is.null(input$permit_status) && input$permit_status != "All") {
      df <- df %>% filter(permit_status == input$permit_status)
    }
    if (isTRUE(input$tribal_context_only)) {
      df <- df %>% filter(isTRUE(has_tribal_context) | has_tribal_context == TRUE)
    }
    if (isTRUE(input$violation_only)) {
      df <- df %>% filter(isTRUE(has_current_violation) | has_current_violation == TRUE)
    }

    df
  })

  waste_filtered <- reactive({
    df <- cwa

    if (!is.null(input$waste_status_filter) && input$waste_status_filter != "All") {
      df <- df %>% filter(permit_status == input$waste_status_filter)
    }
    if (!is.null(input$waste_county_filter) && input$waste_county_filter != "All") {
      df <- df %>% filter(county == input$waste_county_filter)
    }
    if (isTRUE(input$waste_tribal_context_only)) {
      df <- df %>% filter(has_tribal_context == TRUE)
    }
    if (isTRUE(input$waste_violation_only)) {
      df <- df %>% filter(has_current_violation == TRUE)
    }

    df
  })

  # Overview plots ------------------------------------------------------------

  output$overview_pws_bar <- renderPlot({
    df <- pws %>%
      count(source_type, sort = TRUE) %>%
      slice_head(n = 10)

    if (nrow(df) == 0) return(empty_plot("No water-system source records available."))

    ggplot(df, aes(x = reorder(source_type, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Most common public water source types",
        x = NULL,
        y = "Number of source records"
      ) +
      theme_clean
  })

  output$overview_cwa_bar <- renderPlot({
    df <- cwa %>%
      count(permit_status, sort = TRUE) %>%
      slice_head(n = 10)

    if (nrow(df) == 0) return(empty_plot("No wastewater/NPDES facility records available."))

    ggplot(df, aes(x = reorder(permit_status, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Wastewater/NPDES facilities by permit status",
        x = NULL,
        y = "Number of facilities"
      ) +
      theme_clean
  })

  # Map -----------------------------------------------------------------------

  output$main_map <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -152, lat = 64, zoom = 4)

    groups <- character(0)

    if ("pws" %in% input$map_layers) {
      df <- has_coords(pws_filtered())
      groups <- c(groups, "Water system sources")
      if (nrow(df) > 0) {
        popup <- paste0(
          "<b>", df$system_name, "</b><br>",
          "PWS ID: ", df$pws_id, "<br>",
          "Source type: ", df$source_type, "<br>",
          "Classification: ", df$federal_type, "<br>",
          "Population served: ", scales::comma(df$population_served)
        )
        m <- m %>%
          addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            radius = 5,
            stroke = TRUE,
            weight = 1,
            fillOpacity = 0.65,
            color = "#2c7fb8",
            popup = popup,
            group = "Water system sources"
          )
      }
    }

    if ("cwa" %in% input$map_layers) {
      df <- has_coords(cwa_filtered_map())
      groups <- c(groups, "Wastewater/NPDES facilities")
      if (nrow(df) > 0) {
        popup <- paste0(
          "<b>", df$facility_name, "</b><br>",
          "NPDES ID: ", df$npdes_id, "<br>",
          "County: ", df$county, "<br>",
          "Permit status: ", df$permit_status, "<br>",
          "Tribal-context flag: ", ifelse(df$has_tribal_context, "Yes", "No"), "<br>",
          "Current violation/SNC flag: ", ifelse(df$has_current_violation, "Yes", "No")
        )
        m <- m %>%
          addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            radius = ifelse(df$has_current_violation, 7, 5),
            stroke = TRUE,
            weight = 1,
            fillOpacity = 0.70,
            color = ifelse(df$has_current_violation, "#d95f0e", "#31a354"),
            popup = popup,
            group = "Wastewater/NPDES facilities"
          )
      }
    }

    if ("outfalls" %in% input$map_layers) {
      df <- has_coords(outfalls)
      groups <- c(groups, "NPDES outfalls")
      if (nrow(df) > 0) {
        popup <- paste0(
          "<b>", df$facility_name, "</b><br>",
          "NPDES ID: ", df$npdes_id, "<br>",
          "Permit feature: ", df$permit_feature_number, "<br>",
          "Waterbody: ", df$state_water_body_name, "<br>",
          "Permit status: ", df$permit_status
        )
        m <- m %>%
          addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            radius = 4,
            stroke = FALSE,
            fillOpacity = 0.50,
            color = "#756bb1",
            popup = popup,
            group = "NPDES outfalls"
          )
      }
    }

    if ("notices" %in% input$map_layers) {
      df <- has_coords(notices)
      groups <- c(groups, "Open water notices")
      if (nrow(df) > 0) {
        popup <- paste0(
          "<b>", df$system_name, "</b><br>",
          "PWS ID: ", df$pws_id, "<br>",
          "Notice type: ", df$notice_type, "<br>",
          "Notice status: ", df$notice_status
        )
        m <- m %>%
          addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            radius = 8,
            stroke = TRUE,
            weight = 2,
            fillOpacity = 0.85,
            color = "#e31a1c",
            popup = popup,
            group = "Open water notices"
          )
      }
    }

    if (length(groups) > 0) {
      m <- m %>%
        addLayersControl(
          overlayGroups = unique(groups),
          options = layersControlOptions(collapsed = FALSE)
        )
    }

    m
  })

  # Water systems tab ---------------------------------------------------------

  output$water_source_bar <- renderPlot({
    df <- water_tab_filtered() %>%
      count(source_type, sort = TRUE) %>%
      slice_head(n = 12)

    if (nrow(df) == 0) return(empty_plot("No records match the selected filters."))

    ggplot(df, aes(x = reorder(source_type, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Water source types",
        x = NULL,
        y = "Number of source records"
      ) +
      theme_clean
  })

  output$water_population_hist <- renderPlot({
    df <- water_tab_filtered() %>%
      filter(is.finite(population_served), population_served >= 0)

    if (nrow(df) == 0) return(empty_plot("No population-served values available for the selected filters."))

    ggplot(df, aes(x = population_served)) +
      geom_histogram(bins = 30) +
      scale_x_continuous(labels = scales::comma) +
      labs(
        title = "Distribution of population served",
        x = "Population served",
        y = "Number of source records"
      ) +
      theme_clean
  })

  output$water_table <- DT::renderDT({
    water_tab_filtered() %>%
      select(system_name, pws_id, source_type, federal_type, population_served, latitude, longitude) %>%
      arrange(system_name)
  }, options = list(pageLength = 10, scrollX = TRUE))

  # Wastewater tab ------------------------------------------------------------

  output$waste_status_bar <- renderPlot({
    df <- waste_filtered() %>%
      count(permit_status, sort = TRUE) %>%
      slice_head(n = 12)

    if (nrow(df) == 0) return(empty_plot("No records match the selected filters."))

    ggplot(df, aes(x = reorder(permit_status, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Wastewater/NPDES permit status",
        x = NULL,
        y = "Number of facilities"
      ) +
      theme_clean
  })

  output$waste_flow_scatter <- renderPlot({
    df <- waste_filtered() %>%
      filter(is.finite(total_design_flow), is.finite(actual_average_flow))

    if (nrow(df) < 3) return(empty_plot("Not enough complete flow data for a scatter plot."))

    ggplot(df, aes(x = total_design_flow, y = actual_average_flow)) +
      geom_point(aes(shape = has_current_violation), alpha = 0.75) +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Design flow vs. actual average flow",
        x = "Total design flow",
        y = "Actual average flow",
        shape = "Violation/SNC flag"
      ) +
      theme_clean
  })

  output$waste_county_bar <- renderPlot({
    df <- waste_filtered() %>%
      count(county, sort = TRUE) %>%
      filter(!is.na(county), county != "Unknown county") %>%
      slice_head(n = 15)

    if (nrow(df) == 0) return(empty_plot("No county information available for the selected filters."))

    ggplot(df, aes(x = reorder(county, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Counties with the most wastewater/NPDES facilities",
        x = NULL,
        y = "Number of facilities"
      ) +
      theme_clean
  })

  output$outfall_waterbody_bar <- renderPlot({
    df <- outfalls %>%
      filter(!is.na(state_water_body_name), state_water_body_name != "") %>%
      count(state_water_body_name, sort = TRUE) %>%
      slice_head(n = 12)

    if (nrow(df) == 0) return(empty_plot("No waterbody names available in the outfall dataset."))

    ggplot(df, aes(x = reorder(state_water_body_name, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Most common receiving waterbodies in outfall data",
        x = NULL,
        y = "Number of outfall records"
      ) +
      theme_clean
  })

  output$waste_table <- DT::renderDT({
    waste_filtered() %>%
      select(facility_name, npdes_id, city, county, permit_status, permit_type,
             total_design_flow, actual_average_flow, has_tribal_context,
             has_current_violation, latitude, longitude) %>%
      arrange(county, facility_name)
  }, options = list(pageLength = 10, scrollX = TRUE))

  # County profile tab --------------------------------------------------------

  county_profile <- reactive({
    cwa %>%
      group_by(county) %>%
      summarise(
        n_facilities = n(),
        n_with_tribal_context = sum(has_tribal_context == TRUE, na.rm = TRUE),
        n_with_violation_flag = sum(has_current_violation == TRUE, na.rm = TRUE),
        total_design_flow = sum(total_design_flow, na.rm = TRUE),
        mean_actual_flow = mean(actual_average_flow, na.rm = TRUE),
        mean_population_density = mean(population_density, na.rm = TRUE),
        mean_low_income_pct = mean(pct_low_income_3mi, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        mean_actual_flow = ifelse(is.nan(mean_actual_flow), NA, mean_actual_flow),
        mean_population_density = ifelse(is.nan(mean_population_density), NA, mean_population_density),
        mean_low_income_pct = ifelse(is.nan(mean_low_income_pct), NA, mean_low_income_pct)
      ) %>%
      filter(!is.na(county), county != "Unknown county")
  })

  output$county_heatmap <- renderPlot({
    df <- county_profile() %>%
      arrange(desc(n_facilities)) %>%
      slice_head(n = 20)

    if (nrow(df) < 2) return(empty_plot("Not enough county-level data for a heat map."))

    metric_cols <- c("n_facilities", "n_with_tribal_context", "n_with_violation_flag",
                     "total_design_flow", "mean_actual_flow",
                     "mean_population_density", "mean_low_income_pct")

    scaled <- df %>%
      select(county, all_of(metric_cols)) %>%
      mutate(across(all_of(metric_cols), ~ as.numeric(scale(.x)))) %>%
      pivot_longer(-county, names_to = "metric", values_to = "standardized_value")

    ggplot(scaled, aes(x = metric, y = reorder(county, standardized_value, FUN = mean, na.rm = TRUE), fill = standardized_value)) +
      geom_tile(color = "white") +
      labs(
        title = "County profile heat map",
        subtitle = "Values are standardized so different variables can be compared",
        x = NULL,
        y = NULL,
        fill = "Standardized value"
      ) +
      theme_clean +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
  })

  output$county_pca <- renderPlot({
    df <- county_profile()

    metric_cols <- c("n_facilities", "n_with_tribal_context", "n_with_violation_flag",
                     "total_design_flow", "mean_actual_flow",
                     "mean_population_density", "mean_low_income_pct")

    numeric_data <- df %>%
      select(all_of(metric_cols))

    # Keep variables with real variation.
    keep <- vapply(numeric_data, function(x) {
      x <- x[is.finite(x)]
      length(x) >= 3 && stats::sd(x) > 0
    }, logical(1))

    numeric_data <- numeric_data[, keep, drop = FALSE]

    complete_rows <- stats::complete.cases(numeric_data)
    if (ncol(numeric_data) < 2 || sum(complete_rows) < 3) {
      return(empty_plot("Not enough complete numeric county data for PCA."))
    }

    pca <- stats::prcomp(numeric_data[complete_rows, , drop = FALSE], center = TRUE, scale. = TRUE)

    scores <- tibble::as_tibble(pca$x[, 1:2, drop = FALSE]) %>%
      mutate(county = df$county[complete_rows])

    var_exp <- round(100 * summary(pca)$importance[2, 1:2], 1)

    ggplot(scores, aes(x = PC1, y = PC2, label = county)) +
      geom_point(size = 3) +
      geom_text(vjust = -0.8, size = 3) +
      labs(
        title = "PCA-style county profile plot",
        subtitle = "Counties near each other have similar wastewater/NPDES profiles",
        x = paste0("PC1 (", var_exp[1], "%)"),
        y = paste0("PC2 (", var_exp[2], "%)")
      ) +
      theme_clean
  })

  output$county_table <- DT::renderDT({
    county_profile() %>%
      arrange(desc(n_facilities))
  }, options = list(pageLength = 10, scrollX = TRUE))

  # Data quality tab ----------------------------------------------------------

  selected_missing_df <- reactive({
    switch(
      input$missing_dataset,
      "pws" = pws,
      "cwa" = cwa,
      "outfalls" = outfalls,
      "notices" = notices,
      pws
    )
  })

  missing_summary <- reactive({
    df <- selected_missing_df()
    if (nrow(df) == 0) {
      return(tibble::tibble(variable = character(), missing_count = integer(), missing_percent = numeric()))
    }

    tibble::tibble(
      variable = names(df),
      missing_count = vapply(df, function(x) sum(is.na(x) | trimws(as.character(x)) == ""), integer(1)),
      total_rows = nrow(df)
    ) %>%
      mutate(missing_percent = missing_count / total_rows) %>%
      arrange(desc(missing_percent))
  })

  output$missingness_plot <- renderPlot({
    df <- missing_summary() %>%
      slice_head(n = 20)

    if (nrow(df) == 0) return(empty_plot("No records available for this dataset."))

    ggplot(df, aes(x = reorder(variable, missing_percent), y = missing_percent)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Variables with the most missing data",
        x = NULL,
        y = "Percent missing"
      ) +
      theme_clean
  })

  output$missingness_table <- DT::renderDT({
    missing_summary() %>%
      mutate(missing_percent = scales::percent(missing_percent, accuracy = 0.1))
  }, options = list(pageLength = 12, scrollX = TRUE))
}

shinyApp(ui = ui, server = server)
