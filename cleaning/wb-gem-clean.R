library(tidyverse)

### Not putting the original wb-gem file in the drive because it's 25MB.
### Instead just putting the annual version in the drive.
# data <- read_csv("~/Downloads/Levy Datasets Scratch/wb-gem.csv")
# data_annual <- data[grepl("^[0-9]+$", data$Time), ]
# write_csv(data_annual, "./data_raw/wb-gem-annual.csv")

data_annual <- read_csv("./data_raw/wb-gem-annual.csv")

name_mapping <- c(
  CoreCPI = "Core CPI,seas.adj,,, [CORESA]",
  CPIPrice = "CPI Price, seas. adj.,,, [CPTOTSAXN]",
  ExchangeRateNewLCU = "Exchange rate, new LCU per USD extended backward, period average,, [DPANUSSPB]",
  ExchangeRateOldLCU = "Exchange rate, old LCU per USD extended forward, period average,, [DPANUSSPF]",
  ExportsMerchandise = "Exports Merchandise, Customs, current US$, millions, seas. adj. [DXGSRMRCHSACD]",
  ExportsMerchandisePrice = "Exports Merchandise, Customs, Price, US$, seas. adj. [DXGSRMRCHSAXD]",
  ImportsMerchandise = "Imports Merchandise, Customs, current US$, millions, seas. adj. [DMGSRMRCHSACD]",
  ImportsMerchandisePrice = "Imports Merchandise, Customs, Price, US$, seas. adj. [DMGSRMRCHSAXD]",
  IndustrialProduction = "Industrial Production, constant US$, seas. adj.,, [IPTOTSAKD]",
  ReservesInMonthsOfImportCover = "Months Import Cover of Foreign Reserves,,,, [IMPCOV]",
  NominalEffeciveExchangeRate = "Nominal Effecive Exchange Rate,,,, [NEER]",
  OfficialExchangeRate = "Official exchange rate, LCU per USD, period average,, [DPANUSLCU]",
  RealEffectiveExchangeRate = "Real Effective Exchange Rate,,,, [REER]",
  RetailSalesVolume = "Retail Sales Volume,Index,,, [RETSALESSA]",
  StockMarkets = "Stock Markets, US$,,, [DSTKMKTXD]",
  TotalReserves = "Total Reserves,,,, [TOTRESV]",
  UnemploymentRate = "Unemployment rate,Percent,,, [UNEMPSA_]"
)

name_mapping_df <- data.frame(old_name = names(name_mapping), new_name = unname(name_mapping), stringsAsFactors = FALSE)
write.table(name_mapping_df, file = "wb-gem-name-mapping.csv", sep = ",", row.names = FALSE)
# name_mapping_file <- read.table("wb-gem-name-mapping.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
# name_mapping <- setNames(as.vector(name_mapping_file$new_name), name_mapping_file$old_name)

data_annual <- rename(data_annual, 
               Country_Name = "Country",
               CountryCode = "Country Code",
               Time = "Time",
               TimeCode = "Time Code")

data_annual <- data_annual %>% rename(!!!name_mapping)

# Normalize country names
country_codes <- read_csv("./country-codes.csv")
country_codes <- country_codes %>%
  rename(Country_Name_Cannonical = Country_Name)
data_annual <- data_annual %>%
  rename(Country_Code = "CountryCode")
data_annual <- data_annual %>%
  left_join(country_codes, by = "Country_Code") %>%
  mutate(Country_Name = coalesce(Country_Name_Cannonical, Country_Name)) %>%
  select(-Country_Name_Cannonical)

data_annual <- data_annual %>% mutate(across(everything(), ~ replace(., . == ".." | . == 0 | . == "0", NA)))

# make all columns numeric except Country_Name and Country_Code
data_annual <- data_annual %>%
  mutate(across(c(-Country_Name, -Country_Code), as.numeric))

write_csv(data_annual, "./data_clean/wb-gem-annual.csv")
