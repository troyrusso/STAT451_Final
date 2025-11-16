# --- 1. Install/Load required libraries ---
# (You may need to run these install lines once in your console)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")

library(readxl)
library(dplyr)
library(tidyr)

# --- 2. Define File Path ---
# IMPORTANT: Download the Excel file and place it in this project folder
# Rename it to "urb_ctran.xlsx" to keep it simple
file_path <- "urb_ctran.xlsx"

# --- 3. Read and "Tidy" Sheet 1 (Car Share) ---
car_data_wide <- read_excel(file_path, sheet = "Sheet 1", skip = 7)

car_data_tidy <- car_data_wide %>%
  select(TIME, "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024") %>%
  rename(City = TIME) %>%
  pivot_longer(
    cols = -City, 
    names_to = "Year", 
    values_to = "TT1003V"
  )

# --- 4. Read and "Tidy" Sheet 17 (Road Deaths) ---
deaths_data_wide <- read_excel(file_path, sheet = "Sheet 17", skip = 7)

deaths_data_tidy <- deaths_data_wide %>%
  select(TIME, "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024") %>%
  rename(City = TIME) %>%
  pivot_longer(
    cols = -City,
    names_to = "Year",
    values_to = "TT1060I"
  )

# --- 5. Join the Two Datasets ---
merged_data <- inner_join(car_data_tidy, deaths_data_tidy, by = c("City", "Year"))

# --- 6. Clean Data for Plotting ---
plot_data <- merged_data %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(suppressWarnings(as.numeric(TT1003V))) & 
           !is.na(suppressWarnings(as.numeric(TT1060I)))) %>%
  mutate(
    TT1003V = as.numeric(TT1003V),
    TT1060I = as.numeric(TT1060I)
  )

# --- 7. FINAL STEP: Save the clean data ---
# Your Shiny app will load this fast, clean file.
saveRDS(plot_data, "eurostat_clean.Rds")

print("--- Data preparation complete! 'eurostat_clean.Rds' has been created. ---")

