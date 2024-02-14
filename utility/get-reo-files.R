library(tidyverse)

# Western Hemisphere
whd = read_csv("./data_raw/WHDREO_08-18-2023 23-17-18-40_panel.csv")
# Africa
afr = read_csv("./data_raw/AFRREO_08-18-2023 23-16-54-70_panel.csv")
# Middle East and Central Asia
mcd = read_csv("./data_raw/MCDREO_08-18-2023 23-16-39-88_panel.csv")
# Asia and Pacific
apd = read_csv("./data_raw/APDREO_08-18-2023 22-59-02-44_panel.csv")

whd <- whd %>% select(!starts_with("Status") & !starts_with("..."))
afr <- afr %>% select(!starts_with("Status") & !starts_with("..."))
mcd <- mcd %>% select(!starts_with("Status") & !starts_with("..."))
apd <- apd %>% select(!starts_with("Status") & !starts_with("..."))

whd <- whd %>% rename(
  "Country_Name" = "Country Name",
  "Time" = "Time Period")
afr <- afr %>% rename(
  "Country_Name" = "Country Name",
  "Time" = "Time Period")
mcd <- mcd %>% rename(
  "Country_Name" = "Country Name",
  "Time" = "Time Period")
apd <- apd %>% rename(
  "Country_Name" = "Country Name",
  "Time" = "Time Period")

write_csv(whd, "./data_raw/imf-reo-whd.csv")
write_csv(afr, "./data_raw/imf-reo-afr.csv")
write_csv(mcd, "./data_raw/imf-reo-mcd.csv")
write_csv(apd, "./data_raw/imf-reo-apd.csv")

