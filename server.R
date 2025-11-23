library(shiny)
server <- function(input, output) {
  
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
    
    if (input$linear2) {
      thePlot <- thePlot + geom_smooth(method = "lm", se = FALSE)
    }
    
    thePlot
  })
}