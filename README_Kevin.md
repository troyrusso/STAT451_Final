Project Description

My project is an interactive R Shiny dashboard designed to explore whether city size is associated with differences in road-traffic fatality rates across major cities in Europe.
The dashboard provides two main visualizations:
 1. Trend of fatality rates over time by city size category
 2. Distribution of fatality values (boxplot) grouped by city size
The app allows users to toggle linear trend lines and visually compare Large, Medium, Small, and National averages.
_______________________________________________________________________________________________________________________________________________________
Research Question
Does city population size influence the rate of road-traffic fatalities? Is traffic in large metropolitan areas safer or more dangerous than smaller cities?

Specifically:
1. Do larger cities experience higher or lower fatality rates compared with medium or small cities?
2. Are national fatality levels noticeably different from urban averages?
3. How do these rates change over time?

Our application provides interactive tools to visually investigate this question.
_______________________________________________________________________________________________________________________________________________________
Dataset

We used the Eurostat Urban Audit dataset, specifically:
1. Indicator: Road fatalities (number per 100,000 inhabitants)
2. Geographic scope: Euro cities
3. Time coverage: 2015-2024
4. Source: Eurostat Urban Audit Database (downloaded from ec.europa.eu) https://ec.europa.eu/eurostat/databrowser/view/urb_ctran/default/table?lang=en&category=urb.urb_cgc

The raw spreadsheet was cleaned and restructured in global.R to create an analysis-ready dataset with the fields:
1. City
2. Year
3. Value (fatalities per 100k)
4. Size (Large, Medium, Small, National)
