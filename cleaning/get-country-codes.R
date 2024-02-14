library("tidyverse")

data <- read_csv("./wb-wdi-clean.csv")

data <- data %>%
  select(Country_Name, Country_Code) %>%
  distinct()

write_csv(data, "./country-codes.csv")
