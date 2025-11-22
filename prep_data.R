# NOTE: Different team members got their data in slightly different formats.

# Data processing for Student 1

# --- 1. Install/Load required libraries ---
# (You may need to run these install lines once in your console)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

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

# --- 7. Save the clean data ---
# Your Shiny app will load this fast, clean file.
saveRDS(plot_data, "eurostat_clean.Rds")

print("--- First part of data preparation complete! 'eurostat_clean.Rds' has been created. ---")



# Data processing for Student 2

file_path <- "urb_ctran_public_transit.csv"

clean_city_name_sam <- function(x) {
  if (str_detect(x, "(greater city)"))
    return(substr(x, 1, nchar(x) - nchar(" (greater city)")))
  else if (str_detect(x, "Stadt"))
    return(substr(x, 1, nchar(x) - nchar(", Stadt")))
  else if (str_detect(x, "(Oldenburg)"))
    return(substr(x, 1, nchar(x) - nchar(" (Oldenburg)")))
  else
    return(x)
}

get_country_sam <- function(x) {
  code <- substr(x, 1, 2)
  code_reference <- list("CH" = "Switzerland", "DE" = "Germany",
                         "EE" = "Estonia", "FI" = "Finland", "SI" = "Slovenia")
  code_reference[[code]]
}

fetch_data_sam <- function() {
  raw_data <- read.csv(file_path)
  transport <- raw_data %>% 
    filter(Urban.audit.indicator == paste0("Share of journeys to work ",
                                           "by public transport ",
                                           "(rail, metro, bus, tram) -%")) %>%
    filter(!is.na(OBS_VALUE))
  cost <- raw_data %>% 
    filter(Urban.audit.indicator == paste0("Cost of a combined monthly ticket ",
                                           "(all modes of public transport) ",
                                           "for 5-10 km in the central zone ",
                                           "- EUR")) %>%
    filter(!is.na(OBS_VALUE))
  filtered <- transport %>%
    inner_join(cost, by = c("cities" = "cities",
                            "TIME_PERIOD" = "TIME_PERIOD",
                            "Geopolitical.entity..declaring." =
                              "Geopolitical.entity..declaring.")) %>%
    select("cities", "TIME_PERIOD", "Geopolitical.entity..declaring.",
           "OBS_VALUE.x", "OBS_VALUE.y") %>%
    rename("year" = "TIME_PERIOD", "city_id" = "cities",
           "city" = "Geopolitical.entity..declaring.",
           "percent_journeys_transport" = "OBS_VALUE.x",
           "transport_cost" = "OBS_VALUE.y") %>%
    mutate(city = sapply(city, clean_city_name_sam)) %>%
    mutate(country = sapply(city_id, get_country_sam))
  countries_count <- filtered %>%
    group_by(country) %>%
    summarize(count = n())
  final <- filtered %>%
    inner_join(countries_count, by = c("country" = "country")) %>%
    filter(count > 1) %>%
    select(-count)
}

cut_for_scatterplot_sam <- function(x) {
  countries_years_count <- x %>%
    group_by(country, year) %>%
    summarize(count = n())
  final <- x %>%
    inner_join(countries_years_count,
               by = c("country" = "country", "year" = "year")) %>%
    filter(count > 4) %>%
    select(-count)
}

cut_for_lineplot_sam <- function(x) {
  cities_count <- x %>%
    group_by(city_id) %>%
    summarize(count = n())
  intermediate <- x %>%
    inner_join(cities_count,
               by = c("city_id" = "city_id")) %>%
    filter(count > 1) %>%
    select(-count)
  countries_years_count <- intermediate %>%
    group_by(country, year) %>%
    summarize(count = n())
  final <- intermediate %>%
    inner_join(countries_years_count,
               by = c("country" = "country", "year" = "year")) %>%
    filter(count > 1) %>%
    select(-count)
}

get_data_sam <- function(x) {
  s_data <- fetch_data_sam() %>% cut_for_scatterplot_sam()
  l_data <- fetch_data_sam() %>% cut_for_lineplot_sam()
  save(s_data, file = "scatterplotDataSam.Rdata")
  save(l_data, file = "lineplotDataSam.Rdata")
}

get_data_sam()

print("--- Second part of data preparation complete! ---")