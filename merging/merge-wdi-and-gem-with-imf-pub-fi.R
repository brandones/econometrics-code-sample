library("tidyverse")

wdi <- read_csv("./wb-wdi-clean.csv")
gem <- read_csv("./wb-gem-annual-clean.csv")
pubfi <- read_csv("./data_raw/imf-pub-fi-hist.csv")
country_codes <- read_csv("./country-codes.csv")

# Normalize country names and merge WDI and GEM. WDI names are already the cannonical ones,
# matching country_codes.csv. (this is probably not needed any more)
country_codes <- country_codes %>%
  rename(Country_Name_Cannonical = Country_Name)
wdi <- wdi %>%
  left_join(country_codes, by = "Country_Code")
gem <- gem %>%
  rename(Country_Code = "CountryCode")
gem <- gem %>%
  left_join(country_codes, by = "Country_Code") %>%
  mutate(Country_Name = coalesce(Country_Name_Cannonical, Country_Name)) %>%
  select(-Country_Name_Cannonical)

# Merge by codes where codes exist, or names where codes do not exist
wdi_and_gem_by_codes <- wdi %>%
  filter(!is.na(Country_Code)) %>%
  full_join(gem %>% filter(!is.na(Country_Code)), by = c("Country_Code", "Time"))
wdi_and_gem_by_names <- wdi %>%
  filter(is.na(Country_Code)) %>%
  full_join(gem %>% filter(is.na(Country_Code)), by = c("Country_Name", "Time"))
wdi_gem <- as_tibble(bind_rows(wdi_and_gem_by_codes, wdi_and_gem_by_names))

wdi_gem <- wdi_gem %>%
  mutate(Country_Name = coalesce(Country_Name.x, Country_Name.y)) %>%
  select(-Country_Name.x, -Country_Name.y, -Country_Code.x, -Country_Code.y)

# Now prepare pubfi to merge
pubfi <- rename(pubfi,
               Time = "year",
               Country_Name = "country")

pubfi$Country_Name[pubfi$Country_Name == "Côte d'Ivoire"] <- "Cote d'Ivoire"
pubfi$Country_Name[pubfi$Country_Name == "Czech Republic"] <- "Czechia"
pubfi$Country_Name[pubfi$Country_Name == "Hong Kong SAR"] <- "Hong Kong SAR, China"
pubfi$Country_Name[pubfi$Country_Name == "Iran"] <- "Iran, Islamic Rep."
pubfi$Country_Name[pubfi$Country_Name == "Korea"] <- "Korea, Rep."
pubfi$Country_Name[pubfi$Country_Name == "Micronesia"] <- "Micronesia, Fed. Sts."
pubfi$Country_Name[pubfi$Country_Name == "Republic of Congo"] <- "Congo, Rep."
pubfi$Country_Name[pubfi$Country_Name == "Russia"] <- "Russian Federation"
pubfi$Country_Name[pubfi$Country_Name == "The Bahamas"] <- "Bahamas, The"
pubfi$Country_Name[pubfi$Country_Name == "Türkiye"] <- "Turkiye"
pubfi$Country_Name[pubfi$Country_Name == "Venezuela"] <- "Venezuela, RB"
pubfi$Country_Name[pubfi$Country_Name == "Yemen"] <- "Yemen, Rep."

data <- as_tibble(merge(wdi_gem, pubfi, by = c("Country_Name", "Time"), all.x = TRUE, all.y = TRUE))

data$Time <- as.numeric(data$Time)

data <- data %>%
  arrange(Country_Name, desc(Time)) %>%
  group_by(Country_Name) %>%
  fill(Country_Code, .direction = "downup") %>%
  ungroup()

data <- data[!apply(is.na(data), 1, all),]

write_csv(data, "wdi-and-gem-with-imf-pub-fi.csv")


# Make a plot, just for fun
# plot_data <- data %>%
#   filter(Country_Name %in% c("India", "China", "United States"))
# plot_data <- plot_data %>%
#   filter(Time >= 1950)
# ggplot(plot_data, aes(x = Time, group = 1)) +
#   geom_line(aes(y = Govt_Balance_Change_Pct/10, colour = "Govt Balance Change %")) +
#   geom_line(aes(y = GDP_Growth, colour = "GDP Growth")) +
#   facet_wrap(~Country_Name, scales = "free_y") +
#   scale_y_continuous(limits = c(-15, 15)) +
#   scale_color_manual(values = c("Govt Balance Change %" = "red", "GDP Growth" = "blue")) +
#   labs(title = "Govt Balance Change % and GDP Growth Over Time",
#        x = "Time",
#        y = "Percentage",
#        color = "Indicator") +
#   theme(aspect.ratio = 0.5)
