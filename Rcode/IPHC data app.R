library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

ui <- page_sidebar(
  title = "Length Composition Analysis",
  sidebar = sidebar(
    fileInput(
      "datafile",
      "Upload Data File",
      accept = c(".csv", ".txt", ".rds")
    ),
    hr(),
    uiOutput("year_selector"),
    actionButton("select_all_years", "Select All Years", class = "btn-sm"),
    checkboxInput("show_density", "Show Density Curve", FALSE),
    hr(),
    uiOutput("port_selector"),
    hr(),
    sliderInput("bins", "Number of Bins", min = 10, max = 100, value = 30),
    hr(),
    checkboxInput("show_reference_line", "Show Reference Line", FALSE),
    numericInput("reference_size", "Reference Size", value = NULL, min = 0)
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Length Composition Plot"),
      plotOutput("length_plot", height = "600px")
    ),
    card(
      card_header("Data Summary"),
      verbatimTextOutput("data_summary")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store uploaded data
  data <- reactive({
    req(input$datafile)

    ext <- tools::file_ext(input$datafile$name)

    # Read data based on file extension
    df <- switch(
      ext,
      csv = read.csv(input$datafile$datapath, stringsAsFactors = FALSE),
      txt = read.table(
        input$datafile$datapath,
        header = TRUE,
        stringsAsFactors = FALSE
      ),
      rds = readRDS(input$datafile$datapath),
      stop("Unsupported file type. Please upload .csv, .txt, or .rds file")
    )

    # Standardize column names (convert to lowercase for easier matching)
    names(df) <- tolower(names(df))

    # Validate required columns
    required_cols <- c("fish_length", "sample_year")
    if (!all(required_cols %in% names(df))) {
      showNotification(
        paste(
          "Data must contain 'fish_length' and 'sample_year' columns. Found columns:",
          paste(names(df), collapse = ", ")
        ),
        type = "error",
        duration = NULL
      )
      return(NULL)
    }

    df
  })

  # Dynamic UI for year selection
  output$year_selector <- renderUI({
    req(data())
    years <- sort(unique(data()$sample_year))
    selectInput(
      "year",
      "Select Year(s)",
      choices = years,
      selected = years[1],
      multiple = TRUE
    )
  })

  # Observer for "Select All Years" button
  observeEvent(input$select_all_years, {
    req(data())
    years <- sort(unique(data()$sample_year))
    updateSelectInput(session, "year", selected = years)
  })

  # Dynamic UI for sex selection
  output$sex_selector <- renderUI({
    req(data())
    if ("sex" %in% names(data())) {
      sexes <- unique(data()$sex)
      sexes <- sexes[!is.na(sexes)]
      selectInput(
        "sex",
        "Select Sex",
        choices = c("All", sexes),
        selected = "All",
        multiple = TRUE
      )
    }
  })

  # Dynamic UI for port selection
  output$port_selector <- renderUI({
    req(data())
    if ("port_name" %in% names(data())) {
      ports <- sort(unique(data()$port_name))
      ports <- ports[!is.na(ports)]
      selectInput(
        "port",
        "Select Port",
        choices = c("All", ports),
        selected = "All",
        multiple = TRUE
      )
    }
  })

  # Filter data based on selections
  filtered_data <- reactive({
    req(data(), input$year)

    df <- data()

    # Filter by year
    df <- df %>% filter(sample_year %in% input$year)

    # Filter by sex if column exists and selection is made
    if ("sex" %in% names(df) && !is.null(input$sex) && !"All" %in% input$sex) {
      df <- df %>% filter(sex %in% input$sex)
    }

    # Filter by port_name if column exists and selection is made
    if (
      "port_name" %in%
        names(df) &&
        !is.null(input$port) &&
        !"All" %in% input$port
    ) {
      df <- df %>% filter(port_name %in% input$port)
    }

    df
  })

  # Create length composition plot
  output$length_plot <- renderPlot({
    req(filtered_data())

    df <- filtered_data()

    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected filters", cex = 1.5)
      return()
    }

    # Create base plot
    p <- ggplot(df, aes(x = fish_length)) +
      geom_histogram(
        bins = input$bins,
        fill = "#2C3E50",
        color = "white",
        alpha = 0.7
      ) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Length Composition",
        x = "Fish Length",
        y = "Frequency"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        panel.grid.minor = element_blank()
      )

    # Add density curve if requested
    if (input$show_density) {
      p <- p +
        geom_density(
          aes(
            y = after_stat(count) *
              (max(df$fish_length) - min(df$fish_length)) /
              input$bins
          ),
          color = "#E74C3C",
          linewidth = 1.2
        )
    }

    # Add reference line if requested
    if (
      input$show_reference_line &&
        !is.null(input$reference_size) &&
        !is.na(input$reference_size)
    ) {
      p <- p +
        geom_vline(
          xintercept = input$reference_size,
          color = "#27AE60",
          linetype = "dashed",
          linewidth = 1.2
        ) +
        annotate(
          "text",
          x = input$reference_size,
          y = Inf,
          label = paste("Size:", input$reference_size),
          vjust = 1.5,
          hjust = -0.1,
          color = "#27AE60",
          size = 4,
          fontface = "bold"
        )
    }

    # Add faceting if multiple years, sexes, or ports are selected
    facet_vars <- c()
    if (length(input$year) > 1) {
      facet_vars <- c(facet_vars, "sample_year")
    }
    if (
      "sex" %in%
        names(df) &&
        !is.null(input$sex) &&
        (length(input$sex) > 1 || !"All" %in% input$sex)
    ) {
      facet_vars <- c(facet_vars, "sex")
    }
    if (
      "port_name" %in%
        names(df) &&
        !is.null(input$port) &&
        (length(input$port) > 1 || !"All" %in% input$port)
    ) {
      facet_vars <- c(facet_vars, "port_name")
    }

    if (length(facet_vars) > 0) {
      facet_formula <- as.formula(paste(
        "~",
        paste(facet_vars, collapse = " + ")
      ))
      p <- p + facet_wrap(facet_formula, scales = "free_y")
    }

    p
  })

  # Data summary
  output$data_summary <- renderPrint({
    req(filtered_data())

    df <- filtered_data()

    cat("Data Summary\n")
    cat("============\n\n")
    cat("Total observations:", nrow(df), "\n\n")

    cat("Overall fish length statistics:\n")
    print(summary(df$fish_length))
    cat("\n")

    cat("Fish length statistics by year:\n")
    yearly_stats <- df %>%
      group_by(sample_year) %>%
      summarise(
        n = n(),
        min = min(fish_length, na.rm = TRUE),
        q25 = quantile(fish_length, 0.25, na.rm = TRUE),
        median = median(fish_length, na.rm = TRUE),
        mean = round(mean(fish_length, na.rm = TRUE), 2),
        q75 = quantile(fish_length, 0.75, na.rm = TRUE),
        max = max(fish_length, na.rm = TRUE),
        sd = round(sd(fish_length, na.rm = TRUE), 2),
        .groups = "drop"
      )
    print(as.data.frame(yearly_stats), row.names = FALSE)
    cat("\n")

    if ("sex" %in% names(df)) {
      cat("Sex distribution:\n")
      print(table(df$sex))
      cat("\n")
    }

    if ("port_name" %in% names(df)) {
      cat("Port distribution:\n")
      print(table(df$port_name))
      cat("\n")
    }
  })
}

shinyApp(ui = ui, server = server)
