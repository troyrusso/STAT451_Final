# --- Load Libraries ---
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(viridis)
library(stringr)
library(scales)

# --- Load the CLEAN data ---
# This reads the .Rds file you created with the prep script
plot_data <- readRDS("eurostat_clean.Rds")
load("scatterplotDataSam.Rdata")
load("lineplotDataSam.Rdata")

#=========================================================
# UI (User Interface)
#=========================================================
ui <- navbarPage(
  title = "STAT 451 Project Draft (Troy Russo)",
  
  # --- Troy Tab ---
  tabPanel("Car Safety Analysis",
           
           sidebarLayout(
             
             # --- Sidebar for Inputs ---
             sidebarPanel(
               h3("Car Commuting vs. Public Safety"),
               
               helpText("This dashboard explores the relationship between a city's",
                        "reliance on car commuting and its public safety record.",
                        "Does a higher share of car commuters correlate with",
                        "a higher rate of road accident fatalities?"),
               
               br(), # A line break
               
               # Interactive slider input
               sliderInput("yearSlider",
                           "Select Year Range:",
                           min = 2015,
                           max = 2024,
                           value = c(2015, 2024), # Default to the full range
                           step = 1,
                           sep = "") # Removes the comma
             ),
             
             # --- Main Panel for Outputs ---
             mainPanel(
               h4("Scatter Plot of All Cities in Range"),
               plotOutput("scatterPlot"), # This will show the scatter plot
               
               br(), # A line break
               
               h4("Analysis of Data in Range (Grouped by Quartile)"),
               plotOutput("barChart") # This will show the bar chart
             )
           )
  ),

  # --- Sam Tab ---
  tabPanel(
    "Public Transportation Analysis",
      titlePanel("Public transportation in European cities"),
    fluidRow(
      headerPanel("Public transit usage vs. fare costs"),
      column(
        4,
        radioButtons("country_scatter", "Country",
                    choices = c("Germany",
                                setdiff(unique(s_data$country), "Germany")),
                    selected = "Germany"),
        uiOutput("year_selector_scatter"),
        checkboxGroupInput("trendline_scatter", "Additional Options",
                          choices = "Enable trend line")
      ),
      column(
        8,
        verbatimTextOutput("plot_hoverinfo"),
        plotOutput("scatter",
                  hover = hoverOpts(id = "plot_hover",
                                    delayType = "throttle"),
                  click = "plot_click"),
        tableOutput("plot_clickinfo")
      )
    ),
    fluidRow(
      headerPanel("Public transit usage and fare costs over time"),
      column(
        4,
        radioButtons("country_line", "Country",
                    choices = unique(l_data$country),
                    selected = "Switzerland"),
        uiOutput("city_selector_line"),
        uiOutput("year_selector_line")
      ),
      column(
        8,
        plotOutput("line_trips"),
        plotOutput("line_costs")
      )
    )
  )
  # --- Add other team member tabs below as 'tabPanel(...)', ---
)

#=========================================================
# SERVER (The "Engine")
#=========================================================
server <- function(input, output) {
  
  # --- 1. Create Reactive Data ---
  reactive_data <- reactive({
    plot_data %>%
      filter(Year >= input$yearSlider[1] & Year <= input$yearSlider[2])
  })
  
  
  # --- 2. Render the Scatter Plot ---
  output$scatterPlot <- renderPlot({
    
    ggplot(reactive_data(), aes(x = TT1003V, y = TT1060I)) +
      geom_point(alpha = 0.6, color = "#2c7fb8") +
      geom_smooth(method = "lm", col = "red", se = FALSE) +
      labs(
        title = "Higher Car Commuting Correlates with More Road Fatalities",
        subtitle = "Each point is a European city-year. Red line shows the least squares regression line.",
        x = "Share of Commuters Using a Car (%)",
        y = "Road Fatalities per 10,000 People"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.subtitle = element_text(size=10))
    
  })
  
  # --- 3. Render the Bar Chart ---
  output$barChart <- renderPlot({
    
    filtered_data <- reactive_data()
    
    if(nrow(filtered_data) > 0) {
      quartile_data <- filtered_data %>%
        mutate(car_use_group = factor(
          ntile(TT1003V, 4), 
          labels = c("Lowest 25% (Q1)", "26-50% (Q2)", "51-75% (Q3)", "Highest 25% (Q4)")
        ))
      
      summary_data <- quartile_data %>%
        group_by(car_use_group) %>%
        summarize(
          avg_deaths = mean(TT1060I, na.rm = TRUE),
          n = n(),
          sd = sd(TT1060I, na.rm = TRUE),
          se = sd / sqrt(n),
          margin_of_error = qt(0.975, df = n-1) * se,
          ci_low = pmax(0, avg_deaths - margin_of_error), # pmax ensures CI isn't negative
          ci_high = avg_deaths + margin_of_error,
          .groups = 'drop'
        ) %>%
        mutate(highlight_group = case_when(
          car_use_group == "Lowest 25% (Q1)" ~ "Extremes",
          car_use_group == "Highest 25% (Q4)" ~ "Extremes",
          TRUE ~ "Mid-range"
        ))
      
      ggplot(summary_data, aes(x = car_use_group, y = avg_deaths, fill = highlight_group)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_errorbar(
          aes(ymin = ci_low, ymax = ci_high),
          width = 0.2,   
          linewidth = 0.5, 
          color = "black" 
        ) +
        scale_fill_manual(values = c("Extremes" = "#d95f02", "Mid-range" = "grey")) +
        expand_limits(y = max(summary_data$ci_high) * 1.15) +
        labs(
          title = "Average Road Fatalities Rise with Car Commuting Rates",
          subtitle = "Cities grouped by car commuting quartile. Error bars show 95% confidence interval.",
          x = "Car Commuting Group (Quartile)",
          y = "Average Road Fatalities per 10,000 People"
        ) +
        theme_minimal(base_size = 14) +
        geom_text(aes(label = round(avg_deaths, 3)), 
                  vjust = 4.5, 
                  color = "white", 
                  size = 4)
    }
    
  })
  
    output$year_selector_scatter <- renderUI({
    years <- sort(unique(filter(s_data, country == input$country_scatter)$year))
    selectInput("year_scatter", "Year", choices = years, selected = 2016)
  })
  output$city_selector_line <- renderUI({
    cities <- sort(unique(filter(l_data, country == input$country_line)$city))
    selectInput("city_line", "Choose cities",
                choices = cities, multiple = TRUE,
                selected = c("Zurich", "Geneva", "Basel", "Lausanne", "Bern"))
  })
  output$year_selector_line <- renderUI({
    years <- sort(unique(filter(l_data, country == input$country_line)$year))
    sliderInput("year_line", "Year", min = min(years), max = max(years),
                value = c(min(years), max(years)), step = 1,
                sep = "", ticks = FALSE)
  })

  highlighted_points <- reactiveVal(NA)
  printed_points <- reactiveVal(NA)

  filtered_s_data <- reactive({
    validate(need(!is.na(input$country_scatter),
                  message = "Loading...", label = "country"),
             need(!is.na(input$year_scatter),
                  message = "Loading...", label = "year"))
    s_data %>%
      filter(country == input$country_scatter) %>%
      filter(year == input$year_scatter)
  })

  observeEvent(input$plot_click, {
    highlighted_points(
      nearPoints(
        filtered_s_data(),
        input$plot_click,
        "transport_cost",
        "percent_journeys_transport"
      )
    )
  }
  )

  observeEvent(input$plot_hover, {
    printed_points(
      nearPoints(
        filtered_s_data(),
        input$plot_hover,
        "transport_cost",
        "percent_journeys_transport"
      )
    )
  }
  )

  output$scatter <- renderPlot({
    # https://www.reddit.com/r/rstats/comments/y8yib8/load_order_of_outputs_in_r_shiny/
    # https://mastering-shiny.org/action-graphics.html#modifying-the-plot
    scatter_plot <- filtered_s_data() %>%
      ggplot(mapping = aes(x = transport_cost,
                           y = percent_journeys_transport)) +
      geom_point() +
      labs(title = "Journeys made with public transport vs. transport costs",
           subtitle = paste0("For cities in ", input$country_scatter,
                             " in ", input$year_scatter),
           x = paste0("Cost of a combined monthly public transport ticket ",
                      "for 5-10km in the city centre (Euros)"),
           y = "Percentage of journeys made with public transport") +
      theme_bw()
    if (length(input$trendline_scatter) == 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "lm")
    }
    if (all(!is.na(highlighted_points()))) {
      scatter_plot <- scatter_plot +
        geom_point(data = highlighted_points() %>%
                            filter(country == input$country_scatter) %>%
                            filter(year == input$year_scatter),
                   mapping = aes(x = transport_cost,
                                 y = percent_journeys_transport,
                                 color = city)) +
        labs(title = "Journeys made with public transport vs. transport costs",
           subtitle = paste0("For cities in ", input$country_scatter,
                             " in ", input$year_scatter,
                             ". Shaded region corresponds",
                             " to a 95% confidence interval"),
           x = paste0("Cost of a combined monthly public transport ticket ",
                      "for 5-10km in the city centre (Euros)"),
           y = "Percentage of journeys made with public transport")
    }
    scatter_plot
  })

  output$plot_hoverinfo <- renderText({
    if (any(!is.na(printed_points())) && nrow(printed_points()) > 0)
      paste(c("Hovering over:", paste(printed_points()$city, collapse = ", ")))
    else
      "Hover over or click on a point for more information."
  })

  output$plot_clickinfo <- renderTable({
    if (any(!is.na(highlighted_points()))) {
      highlighted_points() %>%
        filter(country == input$country_scatter) %>%
        rename("transport_cost_euros" = "transport_cost")
    } else {
      highlighted_points()
    }
  })

  filtered_l_data <- reactive({
    validate(need(!is.na(input$city_line),
                  message = "Please select one or more cities", label = "city"),
             need(!is.na(input$year_line),
                  message = "Loading...", label = "year"))
    l_data %>%
      filter(city %in% input$city_line) %>%
      filter(year <= max(input$year_line)) %>%
      filter(year >= min(input$year_line))
  })

  colors <- reactive({
    if (length(input$city_line) > 0) {
      l_cities <- filtered_l_data() %>%
        pull(city) %>%
        unique()
      palette <- hue_pal()(length(l_cities))
      names(palette) <- l_cities # https://stackoverflow.com/questions/53194184
      palette
    }
  })

  output$line_trips <- renderPlot({
    sorted_cities <- filtered_l_data() %>%
      filter(year == max(input$year_line)) %>%
      arrange(desc(percent_journeys_transport)) %>%
      pull(city) %>%
      unname() # https://stackoverflow.com/questions/53194184

    filtered_l_data() %>%
      ggplot(mapping = aes(x = year, y = percent_journeys_transport,
                           color = city)) +
      geom_point() +
      geom_line(mapping = aes(group = city)) +
      ylim(0, NA) +
      labs(title = "Transit ridership over time",
          subtitle = paste0(input$country_line, " ", min(input$year_line),
                            " to ", max(input$year_line)),
          x = "Year",
          y = "Percentage of journeys made with public transport",
          color = "City") +
      theme_bw() +
      scale_color_manual(values = colors(), breaks = sorted_cities) +
      scale_x_continuous(breaks = min(input$year_line):max(input$year_line))
      # https://ggplot2-book.org/scales-position.html#sec-position-continuous-breaks 
  })

  output$line_costs <- renderPlot({
    sorted_cities <- filtered_l_data() %>%
      filter(year == max(input$year_line)) %>%
      arrange(desc(transport_cost)) %>%
      pull(city) %>%
      unname() # https://stackoverflow.com/questions/53194184
    filtered_l_data() %>%
      ggplot(mapping = aes(x = year, y = transport_cost,
                           color = city)) +
      geom_point() +
      geom_line(mapping = aes(group = city)) +
      ylim(0, NA) +
      labs(title = "Transport costs over time",
          subtitle = paste0(input$country_line, " ", min(input$year_line),
                            " to ", max(input$year_line)),
          x = "Year",
          y = "Cost of a combined monthly public transport ticket (Euros)",
          color = "City") +
      theme_bw() +
      scale_color_manual(values = colors(), breaks = sorted_cities) +
      scale_x_continuous(breaks = min(input$year_line):max(input$year_line))
  })
}

#=========================================================
# Run the App
#=========================================================
shinyApp(ui = ui, server = server)