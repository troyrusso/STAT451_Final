Project Description

My project is an interactive R Shiny dashboard designed to explore whether city size is associated with differences in road-traffic fatality rates across major cities in Europe.
The dashboard provides two main visualizations:
 1. Trend of fatality rates over time by city size category
 2. Distribution of fatality values (boxplot) grouped by city size
The app allows users to toggle linear trend lines and visually compare Large, Medium, Small, and National averages.
_________________________________________________________________________________________________________________________________________________
Research Question
Does city population size influence the rate of road-traffic fatalities? Is traffic in large metropolitan areas safer or more dangerous than smaller cities?

Specifically:
1. Do larger cities experience higher or lower fatality rates compared with medium or small cities?
2. Are national fatality levels noticeably different from urban averages?
3. How do these rates change over time?

Our application provides interactive tools to visually investigate this question.
_________________________________________________________________________________________________________________________________________________
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
_________________________________________________________________________________________________________________________________________________
Dataset Download Process Guideline (Eurostat – Road Fatalities Dataset)

Follow the steps below to correctly download the dataset from Eurostat so that it works with our Shiny application.

1. Open the Dataset Page

Go to the official Eurostat page for the dataset:

Link:
https://ec.europa.eu/eurostat/databrowser/view/urb_ctran/default/table?lang=en&category=urb.urb_cgc

This dataset contains urban transport statistics, including road traffic fatalities for European cities.

2. Locate the Download Button

On the top-right section of the table interface, find the download icon (usually a downward arrow).

3. Choose File Format

Click the download button, and in the download window: Set File format → .xlsx

4. Adjust Download Options

Before downloading, change one default setting:

Unselect the Option: “Add summary as a separate sheet”

5. Download the File

After modifying the settings:
 • Click “Download”
 • Your computer will download a file named similar to: urb_ctran$defaultview_spreadsheet.xlsx

6. Save and Use the File

1. Place the downloaded .xlsx file:
Inside your Shiny project folder
(same folder as app.R)
2. And make sure the file path in app.R matches the actual filename.
