# --- Load Libraries ---
library(shiny)
library(shinydashboard)
library(tidyverse)
library(viridis)
library(scales)
library(ggrepel)

# --- Load the CLEAN data ---
# This reads the .Rds and .Rdata files you created with the prep script
plot_data <- readRDS("eurostat_clean.Rds")
load("scatterplotDataSam.Rdata")
load("lineplotDataSam.Rdata")
load("bikes1.rData")
load("viz1_data.Rdata")
load("df2.Rdata")

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
                           value = c(2015, 2024), 
                           step = 1,
                           sep = ""), 
               br(),
               h4(strong("Regression Statistics")),
               helpText("Statistics update based on the selected year range."),
               
  
               uiOutput("troy_stats"),
               br(),
               p(style="font-size: 0.9em; font-style: italic;",
                 "Conclusion: While many factors influence safety, car dependency is a highly significant predictor (p < 0.001), explaining over 10% of the variance in road deaths."),
               
               hr(), # Adds a horizontal line to separate inputs from results
               
               h4(strong("Key Conclusions")), # Bold Header
               
               p("Based on the analysis of European cities:"),
               
               tags$ul(
                 tags$li(strong("Positive Correlation:"), " The scatter plot reveals that as car dependency increases, road fatalities per 10,000 residents also tend to increase."),
                 br(),
                 tags$li(strong("Statistically Significant:"), " The bar chart shows that the top 25% of cities for car usage (Q4) have a significantly higher average death rate than the lowest group, as indicated by the non-overlapping confidence intervals.")
               )
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
    h5(paste0("This dashboard explores public transit usage, public transit user costs, ",
              "their relationship, and how they evolve over time. How does transit ridership ",
              "correlate with transit costs?")),
    fluidRow(
      headerPanel("Public transit usage vs. fare costs"),
      column(
        4,
        radioButtons("country_scatter_sam", "Country",
                    choices = c("Germany",
                                setdiff(unique(s_data$country), "Germany")),
                    selected = "Germany"),
        uiOutput("year_selector_scatter_sam"),
        checkboxGroupInput("trendline_scatter_sam", "Additional Options",
                          choices = "Enable trend line")
      ),
      column(
        8,
        verbatimTextOutput("plot_hoverinfo_sam"),
        plotOutput("scatter_sam",
                  hover = hoverOpts(id = "plot_hover_sam",
                                    delayType = "throttle"),
                  click = "plot_click_sam"),
        tableOutput("plot_clickinfo_sam")
      )
    ),
    fluidRow(
      headerPanel("Public transit usage and fare costs over time"),
      column(
        4,
        radioButtons("country_line_sam", "Country",
                    choices = unique(l_data$country),
                    selected = "Switzerland"),
        uiOutput("city_selector_line_sam"),
        uiOutput("year_selector_line_sam")
      ),
      column(
        8,
        plotOutput("line_trips_sam"),
        plotOutput("line_costs_sam")
      )
    )
  ),
  
  # --- David Tab ---
  tabPanel("Bicycle Use/Infrastructure Analysis",
           titlePanel("Bicycle Use Infrastructure Data"),
           
           sidebarLayout(
             sidebarPanel(
               checkboxInput("match",
                             "Scatterplot: Filter to Match?",
                             value = FALSE)
             ),
             
             mainPanel(
               plotOutput("scatterPlot_david"),
               fluidRow(
                 splitLayout(cellWidths = c("40%", "60%"),
                             plotOutput("sharePlot_david"),
                             plotOutput("networkPlot_david"))
               )
               #plotOutput("sharePlot_david"),
               #plotOutput("networkPlot_david")
             )
           )
        ),
  
  # --- Kevin Tab ---

  tabPanel("Road Accident Trends",
  titlePanel("Road-accident fatalities by city size"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("linear1",
                    "Add linear trend line to time-series plot",
                    value = FALSE)
    ),
    
    mainPanel(
      h3("Time series by city size"),
      plotOutput("plot1", height = "350px"),
      br(),
      h3("Distribution by city size"),
      plotOutput("plot2", height = "350px")
    )
  )
  
  )
  # --- Add other team member tabs below as 'tabPanel(...)', ---
  
)

#=========================================================
# SERVER (The "Engine")
#=========================================================
server <- function(input, output) {
  
  ## TROY ##
  
  # --- 1. Create Reactive Data ---
  reactive_data <- reactive({
    plot_data %>%
      filter(Year >= input$yearSlider[1] & Year <= input$yearSlider[2])
  })
  
  
  # --- 2. Render the Scatter Plot ---
  output$scatterPlot <- renderPlot({
    
    ggplot(reactive_data(), aes(x = TT1003V, y = TT1060I)) +
      geom_point(alpha = 0.6, color = "#2c7fb8") +
      
      # --- UPDATE: se = TRUE adds the shaded confidence interval ---
      geom_smooth(method = "lm", col = "red", se = TRUE) + 
      
      labs(
        title = "Higher Car Commuting Correlates with More Road Fatalities",
        subtitle = "Each point is a European city-year. Red line shows regression with 95% confidence interval.",
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
          
          # --- UPDATE: Subtitle now defines the groups ---
          subtitle = "Groups defined by car usage \n(Q1 = Cities with lowest % car commuters; Q4 = Highest).\nError bars show 95% confidence interval of the mean fatality rate.",
          
          x = "Car Commuting Group (Quartile)",
          y = "Average Road Fatalities per 10,000 People"
        ) +
        theme_minimal(base_size = 14) +
        geom_text(aes(label = round(avg_deaths, 3)), 
                  vjust = 1.5, 
                  color = "white", 
                  size = 4)
    }
    
  })
  output$troy_stats <- renderUI({
    
    df <- reactive_data()
    
    # We need at least 2 points to do stats
    if(nrow(df) > 2) {
      
      # Fit a linear model: Deaths ~ Car_Usage
      model <- lm(TT1060I ~ TT1003V, data = df)
      
      # Extract key statistics
      r_sq <- summary(model)$r.squared
      correlation <- cor(df$TT1003V, df$TT1060I, use = "complete.obs")
      
      # Extract the P-value (significance of the trend)
      p_val <- summary(model)$coefficients[2, 4]
      p_text <- ifelse(p_val < 0.001, "< 0.001 (Highly Significant)", round(p_val, 3))
      
      # Create the HTML output
      tagList(
        tags$ul(
          tags$li(strong("R-Squared: "), round(r_sq, 3), 
                  tags$br(), span(style="font-size: 0.8em; color: gray;", "(Explains % of variance in deaths)")),
          
          tags$li(strong("Correlation (r): "), round(correlation, 3),
                  tags$br(), span(style="font-size: 0.8em; color: gray;", "(Strength of relationship)")),
          
          tags$li(strong("P-Value: "), p_text)
        )
      )
    } else {
      p("Not enough data to calculate statistics.")
    }
  })
  
  ## SAM ##
  
  output$year_selector_scatter_sam <- renderUI({
    years <- sort(unique(filter(s_data, country == input$country_scatter_sam)$year))
    selectInput("year_scatter_sam", "Year", choices = years, selected = 2016)
  })
  output$city_selector_line_sam <- renderUI({
    cities <- sort(unique(filter(l_data, country == input$country_line_sam)$city))
    selectInput("city_line_sam", "Choose cities",
                choices = cities, multiple = TRUE,
                selected = c("Zurich", "Geneva", "Basel", "Lausanne", "Bern"))
  })
  output$year_selector_line_sam <- renderUI({
    years <- sort(unique(filter(l_data, country == input$country_line_sam)$year))
    sliderInput("year_line_sam", "Year", min = min(years), max = max(years),
                value = c(min(years), max(years)), step = 1,
                sep = "", ticks = FALSE)
  })

  highlighted_points_sam <- reactiveVal(NA)
  printed_points_sam <- reactiveVal(NA)

  filtered_s_data_sam <- reactive({
    validate(need(!is.na(input$country_scatter_sam),
                  message = "Loading...", label = "country"),
             need(!is.na(input$year_scatter_sam),
                  message = "Loading...", label = "year"))
    s_data %>%
      filter(country == input$country_scatter_sam) %>%
      filter(year == input$year_scatter_sam)
  })

  observeEvent(input$plot_click_sam, {
    highlighted_points_sam(
      nearPoints(
        filtered_s_data_sam(),
        input$plot_click_sam,
        "transport_cost",
        "percent_journeys_transport"
      )
    )
  }
  )

  observeEvent(input$plot_hover_sam, {
    printed_points_sam(
      nearPoints(
        filtered_s_data_sam(),
        input$plot_hover_sam,
        "transport_cost",
        "percent_journeys_transport"
      )
    )
  }
  )

  output$scatter_sam <- renderPlot({
    scatter_plot <- filtered_s_data_sam() %>%
      ggplot(mapping = aes(x = transport_cost,
                           y = percent_journeys_transport)) +
      geom_point() +
      labs(title = "Journeys made with public transport vs. transport costs",
           subtitle = paste0("For cities in ", input$country_scatter_sam,
                             " in ", input$year_scatter_sam),
           x = paste0("Cost of a combined monthly public transport ticket ",
                      "for 5-10km in the city centre (Euros)"),
           y = "Percentage of journeys made with public transport") +
      theme_bw()
    if (length(input$trendline_scatter_sam) == 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "lm")
    }
    if (all(!is.na(highlighted_points_sam()))) {
      scatter_plot <- scatter_plot +
        geom_point(data = highlighted_points_sam() %>%
                            filter(country == input$country_scatter_sam) %>%
                            filter(year == input$year_scatter_sam),
                   mapping = aes(x = transport_cost,
                                 y = percent_journeys_transport,
                                 color = city)) +
        labs(title = "Journeys made with public transport vs. transport costs",
           subtitle = paste0("For cities in ", input$country_scatter_sam,
                             " in ", input$year_scatter_sam,
                             ". Shaded region corresponds",
                             " to a 95% confidence interval"),
           x = paste0("Cost of a combined monthly public transport ticket ",
                      "for 5-10km in the city centre (Euros)"),
           y = "Percentage of journeys made with public transport")
    }
    scatter_plot
  })

  output$plot_hoverinfo_sam <- renderText({
    if (any(!is.na(printed_points_sam())) && nrow(printed_points_sam()) > 0)
      paste(c("Hovering over:", paste(printed_points_sam()$city, collapse = ", ")))
    else
      "Hover over or click on a point for more information."
  })

  output$plot_clickinfo_sam <- renderTable({
    if (any(!is.na(highlighted_points_sam()))) {
      highlighted_points_sam() %>%
        filter(country == input$country_scatter_sam) %>%
        filter(year == input$year_scatter_sam) %>%
        rename("transport_cost_euros" = "transport_cost")
    } else {
      highlighted_points_sam()
    }
  })

  filtered_l_data_sam <- reactive({
    validate(need(!is.na(input$city_line_sam),
                  message = "Please select one or more cities", label = "city"),
             need(!is.na(input$year_line_sam),
                  message = "Loading...", label = "year"))
    l_data %>%
      filter(city %in% input$city_line_sam) %>%
      filter(year <= max(input$year_line_sam)) %>%
      filter(year >= min(input$year_line_sam))
  })

  colors_sam <- reactive({
    if (length(input$city_line_sam) > 0) {
      l_cities <- filtered_l_data_sam() %>%
        pull(city) %>%
        unique()
      palette <- hue_pal()(length(l_cities))
      names(palette) <- l_cities
      palette
    }
  })

  output$line_trips_sam <- renderPlot({
    sorted_cities <- filtered_l_data_sam() %>%
      filter(year == max(input$year_line_sam)) %>%
      arrange(desc(percent_journeys_transport)) %>%
      pull(city) %>%
      unname()

    filtered_l_data_sam() %>%
      ggplot(mapping = aes(x = year, y = percent_journeys_transport,
                           color = city)) +
      geom_point() +
      geom_line(mapping = aes(group = city)) +
      ylim(0, NA) +
      labs(title = "Transit ridership over time",
          subtitle = paste0(input$country_line_sam, " ", min(input$year_line_sam),
                            " to ", max(input$year_line_sam)),
          x = "Year",
          y = "Percentage of journeys made with public transport",
          color = "City") +
      theme_bw() +
      scale_color_manual(values = colors_sam(), breaks = sorted_cities) +
      scale_x_continuous(breaks = min(input$year_line_sam):max(input$year_line_sam))
  })

  output$line_costs_sam <- renderPlot({
    sorted_cities <- filtered_l_data_sam() %>%
      filter(year == max(input$year_line_sam)) %>%
      arrange(desc(transport_cost)) %>%
      pull(city) %>%
      unname()
    filtered_l_data_sam() %>%
      ggplot(mapping = aes(x = year, y = transport_cost,
                           color = city)) +
      geom_point() +
      geom_line(mapping = aes(group = city)) +
      ylim(0, NA) +
      labs(title = "Transport costs over time",
          subtitle = paste0(input$country_line_sam, " ", min(input$year_line_sam),
                            " to ", max(input$year_line_sam)),
          x = "Year",
          y = "Cost of a combined monthly public transport ticket (Euros)",
          color = "City") +
      theme_bw() +
      scale_color_manual(values = colors_sam(), breaks = sorted_cities) +
      scale_x_continuous(breaks = min(input$year_line_sam):max(input$year_line_sam))
  })
  
  ## DAVID ##
  
  output$scatterPlot_david = renderPlot({
    plot1 = ggplot(data = bikes, mapping = aes(x = length_km, y = share_pct))
    
    if(input$match){
      plot1 = ggplot(data = bikes1, mapping = aes(x = length_km, y = share_pct,
                                                  colour = City))
    }
    plot1 = plot1 + geom_point() + 
      xlab("Network Length (km)") + ylab("Share (%)") +
      ggtitle("Bicycle Network Length vs. Share of Work Journeys") +
      xlim(0,1800) + ylim(0,40) +
      coord_fixed(ratio = 1800/40) +
      theme(legend.position = c(.8, .8))
    
    print(plot1)
  })
  
  output$sharePlot_david <- renderPlot({
    plot2 = ggplot(data = bikes1, mapping = aes(x = Year, y = length_km,
                                                colour = City)) +
      geom_point() +
      geom_line() +
      ylim(0,1250) +
      coord_fixed(4/1250) +
      ggtitle("Bicycle Network Length Over Time") +
      ylab("Network Length (km)") +
      scale_color_discrete(breaks=c("Helsinki/Helsingfors",
                                    "Tampere/Tammerfors",
                                    "Tallinn", "Tartu")) +
      theme(legend.position = "none")
    print(plot2)
  })
  
  output$networkPlot_david <- renderPlot({
    plot3 = ggplot(data = bikes1, 
                   mapping = aes(x = Year, y = share_pct, colour = City)) +
      geom_point() +
      geom_line() +
      ylim(0,18) +
      coord_fixed(4/18) +
      ggtitle("Share Of Journeys To Work By Bicycles Over Time") +
      ylab("Share (%)") +
      scale_color_discrete(breaks=c("Helsinki/Helsingfors",
                                    "Tampere/Tammerfors",
                                    "Tartu", "Tallinn"))
    print(plot3)
  })

  ## KEVIN ##
  # PLOT 1
  output$plot1 <- renderPlot({
    
    thePlot <- ggplot(
      viz1_data %>% filter(Size != "Unknown"),
      aes(x = Year, y = MeanRate, color = Size, group = Size)
    ) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = unique(df2$Year)) +
      labs(
        title = "Average road accidents fatality rate by city-size group (2015–2024)",
        x = "Year",
        y = "Deaths per 100,000 residents",
        caption = "Large >400k | Medium 200k–80k | Small <80k"
      ) +
      scale_color_manual(
        values = c(
          "Large" = "#1f77b4",
          "Medium" = "#2ca02c",
          "Small" = "#d62728",
          "National" = "black"   # <-- Highlight National in black
        )
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    if (input$linear1) {
      thePlot <- thePlot + geom_smooth(method = "lm", se = FALSE)
    }
    
    thePlot
  })
  
  # PLOT 2
  output$plot2 <- renderPlot({
    
    thePlot <- ggplot(
      df2 %>% filter(Size != "Unknown"),
      aes(x = Size, y = Value, fill = Size)
    ) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.15, alpha = 0.2, size = 1) +
      labs(
        title = "Distribution of Road Fatalities by City Size",
        x = "City Size",
        y = "Deaths per 100,000",
        caption = "Large >400k | Medium 200k–80k | Small <80k"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    thePlot
  })
}

#=========================================================
# Run the App
#=========================================================
shinyApp(ui = ui, server = server)
