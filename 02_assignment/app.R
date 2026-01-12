# Load packages for Shiny app
pacman::p_load(
  shiny,       # For building the dashboard
  tidyverse,   # For data manipulation
  lubridate,   # For date handling
  plotly,      # For interactive plots
  shinythemes  # For better UI themes
)

# Load the cleaned data
migration_data <- readRDS("guardian_migration_clean.rds")

# Define UI (User Interface)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Guardian Migration Coverage Analysis (2025)"),
  
  tabsetPanel(
    
    # TAB 1: Temporal Patterns (Media Agenda-Setting)
    tabPanel("Coverage Over Time",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 dateRangeInput("date_range",
                                "Select Date Range:",
                                start = min(migration_data$date),
                                end = max(migration_data$date)),
                 selectInput("time_unit",
                             "Time Aggregation:",
                             choices = c("Daily" = "day", 
                                         "Weekly" = "week", 
                                         "Monthly" = "month")),
                 checkboxGroupInput("sections_filter",
                                    "Filter by Section:",
                                    choices = unique(migration_data$section),
                                    selected = unique(migration_data$section))
               ),
               mainPanel(
                 plotlyOutput("timeline_plot", height = "500px"),
                 hr(),
                 h4("Sociological Interpretation:"),
                 textOutput("timeline_interpretation")
               )
             )
    ),
    
    # TAB 2: Framing Analysis
    tabPanel("Discourse Framing",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 dateRangeInput("framing_date_range",
                                "Select Date Range:",
                                start = min(migration_data$date),
                                end = max(migration_data$date)),
                 radioButtons("framing_type",
                              "View By:",
                              choices = c("Absolute Count" = "count",
                                          "Percentage" = "percent"))
               ),
               mainPanel(
                 plotlyOutput("framing_plot", height = "500px"),
                 hr(),
                 h4("Sociological Interpretation:"),
                 textOutput("framing_interpretation")
               )
             )
    ),
    
    # TAB 3: Institutional Analysis (Sections)
    tabPanel("Institutional Coverage",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 sliderInput("top_n_sections",
                             "Number of Top Sections:",
                             min = 5, max = 15, value = 10),
                 checkboxInput("show_wordcount",
                               "Show Average Word Count",
                               value = FALSE)
               ),
               mainPanel(
                 plotlyOutput("section_plot", height = "500px"),
                 hr(),
                 h4("Sociological Interpretation:"),
                 textOutput("section_interpretation")
               )
             )
    )
  )
)

# Define Server (Logic)
server <- function(input, output) {
  
  # VISUALIZATION 1: Temporal Coverage (Agenda-Setting)
  output$timeline_plot <- renderPlotly({
    
    filtered_data <- migration_data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2],
             section %in% input$sections_filter)
    
    # Aggregate by time unit
    time_data <- filtered_data %>%
      mutate(time_period = floor_date(date, unit = input$time_unit)) %>%
      count(time_period)
    
    plot_ly(time_data, x = ~time_period, y = ~n, type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = 'steelblue'),
            marker = list(color = 'steelblue')) %>%
      layout(title = "Media Coverage Intensity Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Number of Articles"),
             hovermode = "x unified")
  })
  
  output$timeline_interpretation <- renderText({
    "This visualization reveals media agenda-setting patterns. Spikes indicate critical events or moral panics around migration. Sustained coverage suggests institutionalized discourse, while gaps may reflect competing news cycles or editorial priorities."
  })
  
  # VISUALIZATION 2: Framing Analysis
  output$framing_plot <- renderPlotly({
    
    filtered_data <- migration_data %>%
      filter(date >= input$framing_date_range[1],
             date <= input$framing_date_range[2])
    
    # Count framing mentions
    framing_counts <- data.frame(
      Frame = c("Refugee", "Asylum", "Border", "Immigration", "Crisis"),
      Count = c(
        sum(filtered_data$mentions_refugee),
        sum(filtered_data$mentions_asylum),
        sum(filtered_data$mentions_border),
        sum(filtered_data$mentions_immigration),
        sum(filtered_data$mentions_crisis)
      )
    )
    
    if(input$framing_type == "percent") {
      framing_counts <- framing_counts %>%
        mutate(Count = (Count / nrow(filtered_data)) * 100)
      y_title <- "Percentage of Articles"
    } else {
      y_title <- "Number of Articles"
    }
    
    plot_ly(framing_counts, x = ~Frame, y = ~Count, type = 'bar',
            marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd'))) %>%
      layout(title = "Discursive Framing of Migration",
             xaxis = list(title = "Frame Type"),
             yaxis = list(title = y_title))
  })
  
  output$framing_interpretation <- renderText({
    "This reveals dominant frames in migration discourse. 'Refugee' vs 'migrant' reflects humanitarian vs. political framing. 'Border' and 'crisis' language suggest securitization. These frames shape public perception and policy legitimation."
  })
  
  # VISUALIZATION 3: Institutional Coverage by Section
  output$section_plot <- renderPlotly({
    
    section_data <- migration_data %>%
      count(section, sort = TRUE) %>%
      head(input$top_n_sections)
    
    if(input$show_wordcount) {
      avg_wordcount <- migration_data %>%
        group_by(section) %>%
        summarise(avg_words = mean(word_count, na.rm = TRUE)) %>%
        filter(section %in% section_data$section)
      
      section_data <- left_join(section_data, avg_wordcount, by = "section")
      
      plot_ly(section_data, x = ~reorder(section, n), y = ~n, type = 'bar',
              text = ~paste("Avg Words:", round(avg_words, 0)),
              marker = list(color = ~avg_words, colorscale = 'Viridis',
                            colorbar = list(title = "Avg Words"))) %>%
        layout(title = "Coverage by Institutional Section",
               xaxis = list(title = ""),
               yaxis = list(title = "Number of Articles"))
    } else {
      plot_ly(section_data, x = ~reorder(section, n), y = ~n, type = 'bar',
              marker = list(color = 'steelblue')) %>%
        layout(title = "Coverage by Institutional Section",
               xaxis = list(title = ""),
               yaxis = list(title = "Number of Articles"))
    }
  })
  
  output$section_interpretation <- renderText({
    "This shows institutional gatekeeping in migration discourse. Politics/World sections indicate state-centric framing. Opinion sections reveal ideological contestation. The distribution reflects power dynamics in shaping migration narratives."
  })
}

# Run the application
shinyApp(ui = ui, server = server)