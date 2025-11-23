STAT 451 Project - Data Download Instructions

This application uses data from the Eurostat database.

DATASET:
  Transport - cities and greater cities [urb_ctran]

HOW TO DOWNLOAD:
1.  Go to the dataset's website:
    https://ec.europa.eu/eurostat/databrowser/view/urb_ctran/default/table?lang=en

2.  In the top right corner, click on the pencil icon in the box with the text
    "Urban audit indicator:   People killed in road accidents per 10000 pop."

3.  Click the "Select all" button under the search bar, and click 
    "Save and go to data view" in the bottom right corner.

4.  Click the "Download" icon (cloud with an arrow).

5.  Select the "Excel file (.xlsx)" format.

6.  Save the downloaded file in the *same directory* as the Shiny application.

7.  IMPORTANT: Rename the downloaded file to exactly:
    urb_ctran.xlsx

8.  Go to https://tinyurl.com/5bd4tn7v, which will automatically download the
    dataset in a slightly different format (used by a different student).
    The link is shortened via tinyurl as the full link is very long.

9.  Save this downloaded file to the same directory as the Shiny application.

10.  Ensure that the newly downloaded file has the name `estat_urb_ctran_filtered.csv`.

11.  Run the `prep_data.R` script once to generate the clean 'eurostat_clean.Rds',
     'scatterplotDataSam.Rdata', and 'lineplotDataSam.Rdata' files.
     The Shiny app will run from these files.