# The IMF GFS download is available from
# https://data.imf.org/?sk=a0867067-d23c-4ebc-ad23-d3b015045405&sid=1390030350675
# It is 143MB and so not uploaded to the drive.

library(tidyverse)
library(plotly)

# Load the data
imf_gfs <- read_csv("/home/brandon/Downloads/Levy Datasets Scratch/imf-gfs.csv")

imf_gfs <- imf_gfs %>% select(!starts_with("Status") &
                              !starts_with("Valuation") &
                              !starts_with("Bases of recording") &
                              !starts_with("Nature of data"))

View(imf_gfs %>% filter(`Country Name` == "Pakistan") %>% filter(`Time Period` == 2015))

# Filter blank rows
exclude_columns <- c("Country Name", "Country Code", "Sector Name", "Sector Code", "Unit Name", "Unit Code", "Time Period")
imf_gfs <- imf_gfs %>% filter(if_any(setdiff(names(.), exclude_columns), ~ !is.na(.)))

names(imf_gfs)

View(imf_gfs %>%
  filter(`Country Name` == "Pakistan",
         `Sector Name` == "Budgetary central government",
         `Unit Name` == "Percent of GDP"))

pakistan_central <- imf_gfs %>%
  filter(`Country Name` == "Pakistan",
         `Sector Name` == "Budgetary central government")

write_csv(pakistan_central, "./data_raw/pakistan_central.csv")

# Plot "Net operating balance (GNOB|_Z)" over time for Pakistan
p <- imf_gfs %>%
  filter(`Country Name` == "Pakistan",
         `Sector Name` == "Budgetary central government",
         `Unit Name` == "Percent of GDP") %>%
  filter(`Time Period` >= 2006) %>%
  ggplot(aes(x = `Time Period`, y = `Net lending (+) / Net borrowing (-) (GNLB|_Z)`)) +
  geom_line() +
  geom_point(aes(text = `Net lending (+) / Net borrowing (-) (GNLB|_Z)`)) +
  theme(aspect.ratio = 1/4)

ggplotly(p, tooltip = "text")

imf_gfs <- imf_gfs %>%
  rename(Country_Name = "Country Name",
         "Time" = "Time Period")

imf_gfs %>% select(Country_Name) %>% unique()

imf_gfs$Country_Name <- gsub(", Rep\\.? of$", "", imf_gfs$Country_Name)
imf_gfs$Country_Name <- gsub(" of$", "", imf_gfs$Country_Name)
imf_gfs$Country_Name[imf_gfs$Country_Name == "Afghanistan, Islamic Rep. of"] <- "Afghanistan"
imf_gfs$Country_Name[imf_gfs$Country_Name == "Bahrain, Kingdom"] <- "Bahrain"
imf_gfs$Country_Name[imf_gfs$Country_Name == "China, P.R.: Mainland"] <- "China"
imf_gfs$Country_Name[imf_gfs$Country_Name == "Dominican Rep."] <- "Dominican Republic"
imf_gfs$Country_Name[imf_gfs$Country_Name == "Ethiopia, The Federal Dem. Rep."] <- "Ethiopia"
imf_gfs$Country_Name[imf_gfs$Country_Name == "Lao People's Dem. Rep."] <- "Lao PDR"
imf_gfs$Country_Name[imf_gfs$Country_Name == "Tanzania, United Rep. of"] <- "Tanzania"
imf_gfs$Country_Name[imf_gfs$Country_Name == "Türkiye"] <- "Turkiye"
imf_gfs$Country_Name[imf_gfs$Country_Name == "Yemen"] <- "Yemen, Rep."

write_csv(imf_gfs, "./data_clean/imf-gfs.csv")
