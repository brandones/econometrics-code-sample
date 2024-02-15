library("tidyverse")

wits <- read_csv("./data_1_clean/wits-country-data.csv")
complexity <- read_csv("./data_1_clean/harvard-complexity.csv")
country_codes <- read_csv("./data_0_raw/country-codes.csv")

# Normalize country names to those in country-codes (and WB WDI)
# Note to reviewers: These mappings are constructed using a combination of AI tools,
# heuristic fuzzy matching, and (brief) manual review.
wits$Country_Name[wits$Country_Name == "Brunei"] <- "Brunei Darussalam"
wits$Country_Name[wits$Country_Name == "Cape Verde"] <- "Cabo Verde"
wits$Country_Name[wits$Country_Name == "Ethiopia(excludes Eritrea)"] <- "Ethiopia"
wits$Country_Name[wits$Country_Name == "Serbia] <- FR(Serbia/Montenegro)"] <- "Serbia"
wits$Country_Name[wits$Country_Name == "Czech Republic"] <- "Czechia"
wits$Country_Name[wits$Country_Name == "ECS"] <- "Europe & Central Asia"
wits$Country_Name[wits$Country_Name == "Ethiopia(excludes Eritrea)"] <- "Ethiopia"
wits$Country_Name[wits$Country_Name == "Faeroe Islands"] <- "Faroe Islands"
wits$Country_Name[wits$Country_Name == "Hong Kong, China"] <- "Hong Kong SAR, China"
wits$Country_Name[wits$Country_Name == "LCN"] <- "Latin America & Caribbean"
wits$Country_Name[wits$Country_Name == "Macao"] <- "Macao SAR, China"
wits$Country_Name[wits$Country_Name == "NAC"] <- "North America"
wits$Country_Name[wits$Country_Name == "SAS"] <- "South Asia"
wits$Country_Name[wits$Country_Name == "Fm Sudan"] <- "Sudan"
wits$Country_Name[wits$Country_Name == "Serbia, FR(Serbia/Montenegro)"] <- "Serbia"
wits$Country_Name[wits$Country_Name == "SSF"] <- "Small states"
wits$Country_Name[wits$Country_Name == "Turks and Caicos Isl."] <- "Turks and Caicos Islands"
wits$Country_Name[wits$Country_Name == "East Timor"] <- "Timor-Leste"
wits$Country_Name[wits$Country_Name == "Turkey"] <- "Turkiye"
wits$Country_Name[wits$Country_Name == "Venezuela"] <- "Venezuela, RB"
wits$Country_Name[wits$Country_Name == "Yemen"] <- "Yemen, Rep."

# Merge the two datasets
wits_with_codes <- wits %>% left_join(country_codes, by = "Country_Name")
data <- wits_with_codes %>% left_join(complexity, by = c("Country_Code", "Year"))

data <- data %>%
  arrange(Country_Name, desc(Year)) %>%
  group_by(Country_Name) %>%
  fill(Country_Code, .direction = "downup") %>%
  ungroup()

write_csv(data, "data_2_merged/wits-with-complexity.csv")
