library("tidyverse")

data <- read_csv("data_raw/wits-trade-summary.csv")

# About the data: ####################################################################################################
#
###### wits-trade-summary.csv:
# Dataset downloaded directly from the WITS website.
#
# Country Growth (%)
#   The growth rate of a country’s trade value (exports plus imports) relative to the previous year.
# HH Market concentration index
#   Hirschman Herfindahl index is a measure of the dispersion of trade value across an exporter’s partners. A country
#   with trade (export or import) that is concentrated in a very few markets will have an index value close to 1.
#   Similarly, a country with a perfectly diversified trade portfolio will have an index close to zero.
#
####### wits-imports.csv
#
# For whatever reason, wits-trade-summary does not include imports disaggregated by product category. This file does.
# It is downloaded from the WITS API using the file `utility/download-wits-imports.py`.
#
######################################################################################################################

# Move from years as columns to Year being a single column
data <- data %>%
  pivot_longer(cols = starts_with("20") | starts_with("19"),
               names_to = "Year", values_to = "Value")
data$Year <- as.numeric(data$Year)

# Fill in Reporter where it is NA with the region name from the file name.
# These are ECS, LCN, NAC, SAS, and SSF.
data <- data %>%
  mutate(Reporter = ifelse(is.na(Reporter), 
                           str_extract(File, "(?<=en_)[^_]*"), 
                           Reporter))

# Get a table of data for individual countries (no Partner column)
country_data <- data %>%
  filter(`Indicator Type` == "Development" | `Indicator Type` == "Trade Indicator" | `Partner` == "World")

# Add a new column 'Indicator_new' which combines 'Indicator' and 'Product categories'
# for rows where 'Partner' is 'World'. We will then pivot on this.
country_data <- country_data %>%
  mutate(Indicator_new = ifelse(Partner == "World" & `Product categories` != "All Products",
                                paste(Indicator, `Product categories`, sep = "_"),
                                Indicator))

# Now pivot wider using 'Indicator_new'
country_data <- country_data %>%
  pivot_wider(names_from = Indicator_new, values_from = Value, id_cols = c("Reporter", "Year"))

# Rename some columnns
country_data <- rename(country_data,
  `Country_Name` = `Reporter`,
  `Country Trade Growth (%)` = `Country Growth (%)`
)

# Define trade balance change
country_data <- country_data %>%
  arrange(Year) %>%
  group_by(Country_Name) %>%
  mutate(Trade_Balance_Change_Perc = (`Trade Balance (current US$ Mil)` -
    lag(`Trade Balance (current US$ Mil)`)) /
    abs(lag(`Trade Balance (current US$ Mil)`)) * 100)

#### Merge in the WITS imports data
imports <- read_csv("data_raw/wits-imports.csv")

# Make new indicators to pivot wider. Go from thousands to millions.
imports <- imports %>%
  mutate(Indicator_new = ifelse(`ProductCode` != "All Products",
                                paste("Import(US$ Thousand)", `ProductCode`, sep = "_"),
                                Indicator))

imports <- imports %>%
  mutate(Value = Value / 1000) %>%
  mutate(Indicator_new = str_replace(Indicator_new, "US\\$ Thousand", "US$ Mil")) %>%
  select(!Indicator)

imports <- imports %>%
  pivot_wider(names_from = Indicator_new, values_from = Value, id_cols = c("Reporter", "Year"))

imports <- imports %>%
  mutate(`Import(US$ Mil)_Metals` = `Import(US$ Mil)_Metals` + `Import(US$ Mil)_Ores and Metals`) %>%
  mutate(`Import(US$ Mil)_Transportation` = `Import(US$ Mil)_Transportation` + `Import(US$ Mil)_Machinery and Transport Equipment`) %>%
  mutate(`Import(US$ Mil)_Textiles and Clothing` = `Import(US$ Mil)_Textiles and Clothing` + `Import(US$ Mil)_Textiles`) %>%
  mutate(`Import(US$ Mil)_Food Products` = `Import(US$ Mil)_Food Products` + `Import(US$ Mil)_Food`) %>%
  mutate(`Import(US$ Mil)_Fuels` = `Import(US$ Mil)_Fuels` + `Import(US$ Mil)_Fuel`) %>%
  mutate(`Import(US$ Mil)_Mach and Elec` = `Import(US$ Mil)_Mach and Elec` + `Import(US$ Mil)_Manufactures`) %>%
  select(!`Import(US$ Mil)_Ores and Metals` & !`Import(US$ Mil)_Machinery and Transport Equipment` &
         !`Import(US$ Mil)_Textiles` & !`Import(US$ Mil)_Food` & !`Import(US$ Mil)_Fuel` &
         !`Import(US$ Mil)_Manufactures`)

imports <- imports %>%
  rename(`Country_Name` = `Reporter`)

categories_in_both <- c("Raw materials", "Intermediate goods", "Consumer goods", "Captial goods")

imports <- imports %>%
  select(`Country_Name`, `Year`, starts_with("Import")) %>%
  select(!ends_with("Raw materials") & !ends_with("Intermediate goods") & !ends_with("Consumer goods") & !ends_with("Capital goods"))

imports <- imports %>%
  mutate(`Import_category_total (US$ Mil)` = rowSums(select(., starts_with("Import(US$ Mil)"))))

imports <- imports %>%
  mutate(across(starts_with("Import(US$ Mil)"),
                ~ . / `Import_category_total (US$ Mil)` * 100,
                .names = "Import Product share(%)_{.col}"))

# Now remove the "Import(US$ Mil)_" part from new column names
names(imports) <- stringr::str_replace_all(names(imports), "_Import\\(US\\$ Mil\\)", "")

# Merge the imports data with the country data
combined_data <- country_data %>%
  left_join(imports, by = c("Country_Name", "Year"))

write_csv(combined_data, "./data_clean/wits-country-data.csv")
