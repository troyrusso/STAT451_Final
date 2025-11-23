pkg <- c("readxl","dplyr","tidyr","ggplot2","stringr","shiny","bslib")
new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
if(length(new.pkg) > 0) install.packages(new.pkg, dependencies = TRUE)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(shiny)
library(bslib)

# LOAD EXCEL FILE
file_path <- "urb_ctran$defaultview_spreadsheet (1).xlsx"
sheet_name <- "Sheet 1"
raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)

# FIND YEAR ROW (HEADER)
yr_row_idx <- which(apply(raw, 1, function(r) any(grepl("^TIME$", as.character(r), ignore.case = TRUE))))
if (length(yr_row_idx) == 0) {
  yr_row_idx <- which(apply(raw, 1, function(r) any(grepl("^\\s*\\d{4}\\s*$", as.character(r)))))
}
yr_row_idx <- yr_row_idx[1]

# DETECT YEAR COLUMNS
year_row_vals <- as.character(raw[yr_row_idx, ])
year_cols <- which(grepl("^\\s*\\d{4}\\s*$", year_row_vals))
years <- as.integer(str_trim(year_row_vals[year_cols]))

# FIND FIRST CITY ROW
start_row <- yr_row_idx + 1
while (start_row <= nrow(raw) &&
       (is.na(raw[[start_row,1]]) || str_trim(as.character(raw[[start_row,1]])) == "")) {
  start_row <- start_row + 1
}

# BUILD DATA: City | Year | Value
records <- list()
k <- 1

for (r in seq(start_row, nrow(raw))) {
  city_cell <- raw[[r,1]]
  if (is.na(city_cell) || str_trim(as.character(city_cell)) == "") next
  
  city <- str_trim(as.character(city_cell))
  
  for (i in seq_along(year_cols)) {
    col_idx <- year_cols[i]
    yr <- years[i]
    val <- suppressWarnings(as.numeric(raw[[r, col_idx]]))
    
    if (!is.na(val)) {
      records[[k]] <- data.frame(
        City = city,
        Year = yr,
        Value = val,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
}

df <- bind_rows(records) %>% arrange(City, Year)

# CITY SIZE CLASSIFICATION
city_size_df <- data.frame(
  City = c("Belgium", "Bruxelles/Brussel (greater city)", "Antwerpen (greater city)",
           "Gent", "Charleroi (greater city)", "Liège (greater city)", "Brugge",
           "Namur", "Leuven", "Mons (greater city)"),
  Size = c("National", "Large", "Large", "Large", "Medium", "Large",
           "Medium", "Small", "Medium", "Small")
)

df2 <- df %>%
  left_join(city_size_df, by = "City") %>%
  mutate(Size = ifelse(is.na(Size), "Unknown", Size))

df2$Size <- factor(df2$Size, levels = c("Large","Medium","Small","National","Unknown"))

# PREPARE VISUALIZATION DATA
viz1_data <- df2 %>%
  filter(!is.na(Value)) %>%
  group_by(Size, Year) %>%
  summarise(MeanRate = mean(Value), .groups = "drop")