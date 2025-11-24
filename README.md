STAT 451 Project - Instructions for downloading data and running the app

This application uses data from the Eurostat database.
There are three different datasets you need to download.
All three datasets should already be in the repository.
If they are not, follow the instructions to download all three.

DATASET:
  Transport - cities and greater cities [urb_ctran]

HOW TO DOWNLOAD THE DATA (if files are not already present):
1.  Go to the dataset's website:
    [Dataset 1](https://ec.europa.eu/eurostat/databrowser/view/urb_ctran/default/table?lang=en)

2.  In the top right corner, click on the pencil icon in the box with the text
    "Urban audit indicator:   People killed in road accidents per 10000 pop."

3.  Click the "Select all" button under the search bar, and click 
    "Save and go to data view" in the bottom right corner.

4.  Click the "Download" icon (cloud with an arrow).

5.  Select the "Excel file (.xlsx)" format.

6.  Save the downloaded file in the *same directory* as the Shiny application.

7.  IMPORTANT: Rename the downloaded file to exactly:
    urb_ctran.xlsx

8.  Go to this website: [Dataset 2](https://tinyurl.com/5bd4tn7v).
    This will automatically download the dataset in 
    a slightly different format (used by a different student).
    The link is shortened via tinyurl as the full link is very long.

9.  Save this downloaded file to the same directory as the Shiny application.

10. Ensure that the newly downloaded file has the name `estat_urb_ctran_filtered_en.csv`.

11. Go to the following website:
    [Dataset 3](https://ec.europa.eu/eurostat/databrowser/view/urb_ctran/default/table?lang=en&category=urb.urb_cgc)

12. Click the download button.

13. In the download window, Set File format → .xlsx

14. Before downloading, change one default setting. 
    Unselect the Option: “Add summary as a separate sheet”

15. Download the file, and ensure it has the name "urb_ctran$defaultview_spreadsheet.xlsx".

16. Move the file into the same directory as the Shiny application.

17. Confirm that you have the files "urb_ctran.xlsx", "urb_ctran$defaultview_spreadsheet.xlsx",
    and "estat_urb_ctran_filtered_en.csv" in the same directory as the Shiny application.

HOW TO RUN THE APP (after files are in the same directory):

18. Run the `prep_data.R` script once to generate one .Rds file and six .Rdata files.
    The Shiny app will run from these files.

19. Run `app.R` to run the Shiny dashboard.