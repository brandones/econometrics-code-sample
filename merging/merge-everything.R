library(tidyverse)

wdi_gem_imfpubfi <- read_csv("./wdi-and-gem-with-imf-pub-fi.csv")
wits_complexity <- read_csv("./wits-with-complexity.csv")

wits_complexity <- wits_complexity %>%
  rename(Time = "Year")

# Merge by codes where codes exist, or names where codes do not exist
data_by_codes <- wdi_gem_imfpubfi %>%
  filter(!is.na(Country_Code)) %>%
  full_join(wits_complexity %>% filter(!is.na(Country_Code)), by = c("Country_Code", "Time"))
data_by_names <- wdi_gem_imfpubfi %>%
  filter(is.na(Country_Code)) %>%
  full_join(wits_complexity %>% filter(is.na(Country_Code)), by = c("Country_Name", "Time"))
data <- as_tibble(bind_rows(data_by_codes, data_by_names))

data <- data %>%
  mutate(Country_Name = coalesce(Country_Name.x, Country_Name.y)) %>%
  select(-Country_Name.x, -Country_Name.y, -Country_Code.x, -Country_Code.y)

# Ok now let's merge in the country metadata—regions, income, etc.
country_class = read_csv("./data_raw/country-class.csv")

country_class <- country_class %>%
  rename(Country_Name = "Economy",
         Country_Code = "Code")

data <- data %>%
  left_join(country_class, by = c("Country_Name"))

data <- data %>%
  mutate(Country_Code = coalesce(Country_Code.x, Country_Code.y)) %>%
  select(-Country_Code.x, -Country_Code.y)

# And now wdi-bop.csv
wdi_bop <- read_csv("./data_raw/wb-wdi-bop.csv")
wdi_bop <- wdi_bop %>%
  rename(Country_Code = "Country Code",
         Country_Name = "Country Name",
         Time_Code = "Time Code")

data <- data %>%
  left_join(wdi_bop, by = c("Country_Code", "Time"))

write_csv(data, "./data-merged.csv")
