STAT 451 Project - Data Download Instructions

This application uses data from the Eurostat database.

DATASET:
  Transport - cities and greater cities [urb_ctran]

HOW TO DOWNLOAD:
  1.  Go to the dataset's website:
    https://ec.europa.eu/eurostat/databrowser/view/urb_ctran/default/table?lang=en

2.  Click the "Download" icon (cloud with an arrow).

3.  Select the "Excel file (.xlsx)" format.

4.  Save the downloaded file in the *same directory* as the Shiny application.

5.  IMPORTANT: Rename the downloaded file to exactly:
    urb_ctran.xlsx

6.  Run the `prep_data.R` script once to generate the clean 'eurostat_clean.Rds' file.
    The Shiny app will run from this .Rds file.