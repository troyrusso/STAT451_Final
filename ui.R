ui <- page_navbar(
  title = "Road Accident Fatality Rate in Europe (2015-2024)",
  
  nav_panel("Trend by City Size",
            sidebarLayout(
              sidebarPanel(
                checkboxInput("linear1", "Add trend line?", FALSE)
              ),
              mainPanel(
                plotOutput("plot1")
              )
            )
  ),
  
  nav_panel("Distribution by City Size",
            sidebarLayout(
              sidebarPanel(
                checkboxInput("linear2", "Add trend line?", FALSE)
              ),
              mainPanel(
                plotOutput("plot2")
              )
            )
  )
)