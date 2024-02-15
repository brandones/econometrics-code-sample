# Install packages
if (!require(plotly)) install.packages("plotly")
if (!require(xlsx)) install.packages("xlsx")

library(tidyverse)
library(xlsx)
library(plotly)

data <- read_csv("./data_2_merged/wits-with-complexity.csv")

commodities <- c("Animal", "Fuels", "Food Products", "Minerals", "Wood", "Vegetable")

export_commodity_cols <- paste0("Export Product share(%)_", commodities)
# Ensure these columns are in the dataset
export_commodity_cols <- export_commodity_cols[export_commodity_cols %in% colnames(data)]
if (length(commodities) != length(export_commodity_cols)) {
  stop(paste("It appears that some commodity in 'commodities' listed above does not have ",
             "a corresponding 'Export Product share' column in the dataset.\n\t commodities:  ",
             paste0(commodities, collapse = ", "),
             "\n\t export_commodity_cols:  ",
             paste0(export_commodity_cols, collapse = ", ")))
}

# Calculate total commodity export for each country and year
data <- data %>% mutate(export_share_commodities = rowSums(.[, export_commodity_cols], na.rm = TRUE))

# Calculate capital + consumer goods import for each country and year
data <- data %>% mutate(import_share_capital_consumer = rowSums(
  select(., `Import Product share(%)_Capital goods`, `Import Product share(%)_Consumer goods`),
  na.rm = TRUE))

####################################################################################################
# Calculate HH indices

# List of export and import categories
categories <- c("Animal", "Chemicals", "Food Products", "Footwear", "Fuels",
                "Hides and Skins", "Mach and Elec", "Metals", "Minerals",
                "Miscellaneous", "Plastic or Rubber", "Stone and Glass",
                "Textiles and Clothing", "Transportation", "Vegetable", "Wood")
names(data)
import_categories <- paste0("Import Product share(%)_", categories)
export_categories <- paste0("Export Product share(%)_", categories)

# Calculate HHI for Exports
exports_HHI <- data %>%
  select(c("Country_Name", "Year", all_of(export_categories))) %>%
  gather(category, share, -c("Country_Name", "Year")) %>%
  mutate(share = (share)^2) %>%
  group_by(Country_Name, Year) %>%
  summarize(exports_HHI = sum(share, na.rm = TRUE))

# Calculate HHI for Imports
imports_HHI <- data %>%
  select(c("Country_Name", "Year", all_of(import_categories))) %>%
  gather(category, share, -c("Country_Name", "Year")) %>%
  mutate(share = (share)^2) %>%
  group_by(Country_Name, Year) %>%
  summarize(imports_HHI = sum(share, na.rm = TRUE))

# Join the datasets
data <- data %>%
  full_join(exports_HHI, by = c("Country_Name", "Year")) %>%
  full_join(imports_HHI, by = c("Country_Name", "Year"))

####################################################################################################

# Define year groups
data <- data %>%
  mutate(Year_Group = ifelse(Year >= 1988 & Year <= 2004, 0, 
                             ifelse(Year >= 2005 & Year <= 2023, 1, NA)))

# Calculate the average of the specified columns for each country and year group
grouped_data <- data %>%
  group_by(Country_Name, Year_Group) %>%
  summarise(`general_HHI` = mean(`HH Market concentration index`, na.rm = TRUE),
            export_share_commodities = mean(export_share_commodities, na.rm = TRUE),
            import_share_capital_consumer = mean(import_share_capital_consumer, na.rm = TRUE),
            economic_complexity_index_sitc = mean(economic_complexity_index_sitc, na.rm = TRUE),
            exports_HHI = mean(exports_HHI, na.rm = TRUE),
            imports_HHI = mean(imports_HHI, na.rm = TRUE))

# Calculate a score for each country and year group. The score is the sum of the normalized
# values of the selected indicator columns. Many countries are missing complexity data; for these
# countries, we set the complexity index to 0.8, a fairly high value, which is sort of based on
# the assumption that the countries missing complexity data, which tend to be very small countries,
# are likely to have low economic complexity.
grouped_data <- grouped_data %>%
  ungroup() %>%
  mutate(across(where(is.numeric) & !Year_Group,
                ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
                .names = "normalized_{.col}")) %>%
  mutate(normalized_economic_complexity_index_sitc = ifelse(is.na(normalized_economic_complexity_index_sitc),
                                                            0.8,
                                                            1 - normalized_economic_complexity_index_sitc)) %>%
  mutate(score = rowSums(select(., starts_with("normalized_")),
                         na.rm = TRUE))

# View the table
# View(grouped_data)

export_data <- grouped_data
export_data$Year_Group[export_data$Year_Group == 0] <- "1988-2004"
export_data$Year_Group[export_data$Year_Group == 1] <- "2005-2023"

write_csv(export_data, "./data_output/real_resource_dependency.csv", na = "")

# Create exportable annual data
annual_data <- data %>%
  select(Country_Name,
         Year,
         export_share_commodities,
         import_share_capital_consumer,
         economic_complexity_index_sitc,
         `HH Market concentration index`,
         `Imports (in US$ Mil)`,
         `Exports (in US$ Mil)`,
         `Trade Balance (% of GDP)`,
         `GDP (current US$ Mil)`,
         `Import Product share(%)_Capital goods`,
         `Import Product share(%)_Consumer goods`,
         `Import Product share(%)_Intermediate goods`,
         `Import Product share(%)_Raw materials`,
         `Export Product share(%)_Capital goods`,
         `Export Product share(%)_Consumer goods`,
         `Export Product share(%)_Intermediate goods`,
         `Export Product share(%)_Raw materials`,
         imports_HHI,
         exports_HHI
  )

write_csv(annual_data, "./data_output/real_resource_dependency_annual.csv", na = "")