library("tidyverse")

wits <- read_csv("./wits-clean-country-data.csv")
complexity <- read_csv("./harvard-complexity.csv")
country_codes <- read_csv("./country-codes.csv")

# Normalize country names to those in country-codes (and WB WDI)
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

write_csv(data, "wits-with-complexity.csv")
