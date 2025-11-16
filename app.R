# --- Load Libraries ---
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(viridis)

# --- Load the CLEAN data ---
# This reads the .Rds file you created with the prep script
plot_data <- readRDS("eurostat_clean.Rds")

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
  
}

#=========================================================
# Run the App
#=========================================================
shinyApp(ui = ui, server = server)