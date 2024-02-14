# Gov rev, gov exp, gov def, ca, pvt sec nafa, GDP, GDP real, growth, unemp, exchange rate
# Interest rates: Cb borrowing fecility rate, tsy long term, mon policy related, money market, foreign currency
#
# Just missing "private sector NAFA"
#
# Data available quarterly:
#  From GEM: GDP, GDP Real, Exchange Rates (monthly), Unemployment Rate (monthly)
#  From IMF IFS: CA, Interest Rates
#  Nothing from GFS or WDI.
#

library(tidyverse)
library(arrow)

names_imf_gfs = c(
  gov_revenue_pct_gdp = "Revenue (G1|_Z)",
  gov_expenditure_pct_gdp = "Expenditure (G2M|_Z)",
  gov_balance_pct_gdp = "Net lending (+) / Net borrowing (-) (GNLB|_Z)"
)

names_wb_wdi = c(
  GDP = "GDP",
  GDP_Real = "GDP_Real",
  GDP_Growth = "GDP_Growth",
  CAB = "Current_Account_Balance",
  WDI_Inflation = "Inflation",
  Savings = "Gross_Domestic_Savings_Pct_GDP",
  Investment = "GFCF_Private_Pct_GDP",
  NAFA_Perc_GDP = "NAFA_Perc_GDP",
  GDP_Deflator = "GDP_Deflator",
  Imports = "Imports",
  Exports = "Exports",
  KA = "Net_Capital_Account",
  FA = "Net_Financial_Account_Cur_USD",
  External_Balance = "External_Balance",
  Net_Primary_Income = "Net_Primary_Income",
  Net_Secondary_Income = "Net_Secondary_Income",
  Govt_Debt = "Govt_Debt",
  External_Debt_Cur_USD = "External_Debt_Cur_USD",
  Private_External_Debt_Cur_USD = "Private_External_Debt_Cur_USD",
  Public_External_Debt_Cur_USD = "Public_External_Debt_Cur_USD",
  Present_Value_External_Debt_Cur_USD = "Present_Value_External_Debt_Cur_USD",
  C_Total = "C_Total",
  C_Gov = "C_Gov",
  C_Household = "C_Household",
  FDI_Net_Outflows = "FDI_Net_Outflows",
  FDI_Net_Inflows = "FDI_Net_Inflows",
  FDI_Net = "FDI_Net")

names_wb_gem = c(
  CoreCPI = "CoreCPI",
  CPIPrice = "CPIPrice",
  NominalEffeciveExchangeRate = "NominalEffeciveExchangeRate",
  RealEffectiveExchangeRate = "RealEffectiveExchangeRate",
  TotalReserves = "TotalReserves",
  UnemploymentRate = "UnemploymentRate")

names_imf_ifs = c(
  r_3mo_interbank = "Financial, Interest Rates, 3-Month Interbank Interest, Percent per annum",
  r_avg_cost_of_funds = "Financial, Interest Rates, Average Cost of Funds, Percent per Annum",
  r_cb = "Financial, Interest Rates, Central Bank Borrowing Facility Rate",
  r_cbcert = "Financial, Interest Rates, Central Bank Certificates",
  r_cert_dep = "Financial, Interest Rates, Certificates of Deposit, Percent per annum",
  r_paper = "Financial, Interest Rates, Corporate Paper Rate",
  r_overnight_dep = "Financial, Interest Rates, Deposit, Overnight",
  r_bond_short_to_medium = "Financial, Interest Rates, Government Bond Yields, Short- to Medium-Term, Percent per Annum",
  r_bill_3mo = "Financial, Interest Rates, Government Securities, Treasury Bills, 3-month, Percent per annum",
  r_overnight_lending = "Financial, Interest Rates, Lending Rate, Overnight",
  r_tsy = "Financial, Interest Rates, Government Securities, Treasury Bills, Percent per annum",
  r_bond = "Financial, Interest Rates, Government Securities, Government Bonds, Percent per annum",
  r_monetary_polily = "Financial, Interest Rates, Monetary Policy-Related Interest Rate, Percent per annum",
  r_money_market = "Financial, Interest Rates, Money Market, Percent per annum",
  r_foreign_currency = "Financial, Interest Rates, Money Market, Foreign Currency, Percent per Annum",
  reserves = "Supplementary Items, Reserves and Related items, US Dollars",
  reserves_total_international = "Total International Reserves, US Dollars (gold at 35 SDRs per ounce)",
  reserves_total = "Total Reserves, US Dollars (Gold at Market Price)"
)

gfs <- read_csv("./data_clean/imf-gfs.csv")
wdi <- read_csv("./data_clean/wb-wdi.csv")
gem <- read_csv("./data_clean/wb-gem-annual.csv")
ifs <- as_tibble(read_parquet("data_clean/imf-ifs-annual.parquet", as_tibble = TRUE))

gfs <- gfs %>% filter(`Sector Name` == "Budgetary central government" &
                      `Unit Name` == "Percent of GDP")

gfs <- gfs %>% select(all_of(c("Country_Name", "Time", names_imf_gfs)))
wdi <- wdi %>% select(all_of(c("Country_Name", "Time", names_wb_wdi)))
gem <- gem %>% select(all_of(c("Country_Name", "Time", names_wb_gem)))
ifs <- ifs %>% select(all_of(c("Country_Name", "Time", names_imf_ifs)))

data <- as_tibble(full_join(gfs, wdi, by = c("Country_Name", "Time")) %>%
  full_join(gem, by = c("Country_Name", "Time")) %>%
  full_join(ifs, by = c("Country_Name", "Time")))

data <- data %>% mutate(across(everything(), ~ replace(., . == ".." | . == 0 | . == "0", NA)))

data <- data %>% mutate(
  "Gov Deficit (IMF GFS GNLB) (% of GDP)" = -gov_balance_pct_gdp,
  "Gov Deficit (IMF GFS GNLB) (current US$, from 'Net Lending Net Borrowing')" = -gov_balance_pct_gdp * GDP / 100,
  "Gov Expenditure (IMF GFS G2M) (% of GDP)" = gov_expenditure_pct_gdp,
  "Gov Expenditure (IMF GFS G2M) (current US$)" = gov_expenditure_pct_gdp * GDP / 100,
  "Gov Revenue (IMF GFS G1) (% of GDP)" = gov_revenue_pct_gdp,
  "Gov Revenue (IMF GFS G1) (current US$)" = gov_revenue_pct_gdp * GDP / 100,
  "Private NAFA (% of GDP)" = Savings - Investment,
  "Private NAFA (current US$)" = (Savings - Investment) * GDP / 100,
  CPI_Inflation = (CPIPrice - lag(CPIPrice)) / lag(CPIPrice) * 100,
  CoreCPI_Inflation = (CoreCPI - lag(CoreCPI)) / lag(CoreCPI) * 100,
  GDP_Deflator = (GDP / GDP_Real) * 100,
  GDP_Inflation = ((GDP_Deflator / lag(GDP_Deflator)) - 1) * 100
)
data <- data %>% select(
  -gov_balance_pct_gdp, -gov_expenditure_pct_gdp, -gov_revenue_pct_gdp
)

### See how bad our Private NAFA calculation is
data <- data %>% mutate(
  "Private NAFA (-G-CA) (% GDP)" = `Gov Deficit (IMF GFS GNLB) (% of GDP)` + CAB/GDP*100,
  "Private NAFA (-G-CA) (current US$)" = `Gov Deficit (IMF GFS GNLB) (current US$, from 'Net Lending Net Borrowing')` + CAB
)
data <- data %>% mutate(
  "Divergence (% GDP)" = `Private NAFA (% of GDP)` - `Private NAFA (-G-CA) (% GDP)`,
)

print(data %>%
  group_by(`Country_Name`) %>%
  summarise(
    count_non_na = sum(!is.na(`Divergence (% GDP)`)),
    mean_divergence = mean(`Divergence (% GDP)`, na.rm = TRUE),
    min_divergence = min(`Divergence (% GDP)`, na.rm = TRUE),
    max_divergence = max(`Divergence (% GDP)`, na.rm = TRUE)
  ) %>%
  filter(count_non_na > 0), n=70)

data %>%
  filter(Country_Name == "United States") %>%
  mutate("Gov balance" = -`Gov Deficit (IMF GFS GNLB) (current US$, from 'Net Lending Net Borrowing')`) %>%
  mutate("CAB_neg" = -`CAB`) %>%
  mutate("NAFA" = NAFA_Perc_GDP * GDP / 100) %>%
  filter(Time > 1993 & Time < 2019) %>%
  select(Time,
         "Gov balance",
         "Private NAFA (-G-CA) (current US$)",
         "Private NAFA (current US$)",
         "CAB_neg"
  ) %>%
  pivot_longer(-Time, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Time, y = Value, color = Variable)) +
  geom_line() +
  theme(aspect.ratio = 1/2)

data %>%
  filter(Country_Name == "United States") %>%
  mutate("Gov balance" = -`Gov Deficit (IMF GFS GNLB) (current US$, from 'Net Lending Net Borrowing')`) %>%
  select(Time,
  "Private NAFA (% of GDP)",
  "Private NAFA (-G-CA) (% GDP)",
  #"Savings",
  #"Investment"
  ) %>%
  pivot_longer(-Time, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Time, y = Value, color = Variable)) +
  geom_line() +
  theme(aspect.ratio = 1/2)

data %>%
  filter(Country_Name == "United States") %>%
  select(Time,
  "CAB",
  "External_Balance",
  "Net_Primary_Income",
  "Net_Secondary_Income"
  #"Savings",
  #"Investment"
  ) %>%
  pivot_longer(-Time, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Time, y = Value, color = Variable)) +
  geom_line() +
  theme(aspect.ratio = 1/2)
### OK done with NAFA stuff


# Remove rows where all values are NA
check_columns <- setdiff(names(data), c("Country_Name", "Time"))
rows_to_keep <- !apply(data[, check_columns], 1, function(row) all(is.na(row)))
data <- data[rows_to_keep, ]

data <- data %>% arrange(Country_Name, Time)


# Go back to all the long names
reverse_rename <- function(data, name_mapping) {
  current_keys <- names(name_mapping)[names(name_mapping) %in% names(data)]
  reverse_name_mapping <- setNames(current_keys, name_mapping[current_keys])
  data <- data %>% rename(!!!reverse_name_mapping)
  return(data)
}

name_map_wdi <- read_csv("./wb-wdi-name-mapping.csv")
wdi_long_names <- setNames(name_map_wdi$new_name, name_map_wdi$old_name)

name_map_gem <- read_csv("./wb-gem-name-mapping.csv")
gem_long_names <- setNames(name_map_gem$new_name, name_map_gem$old_name)

data_renamed <- reverse_rename(data, names_imf_gfs)
data_renamed <- reverse_rename(data_renamed, names_wb_wdi)
data_renamed <- reverse_rename(data_renamed, wdi_long_names)
data_renamed <- reverse_rename(data_renamed, gem_long_names)
data_renamed <- reverse_rename(data_renamed, names_imf_ifs)

write_csv(data_renamed, "./output/wray-curve.csv")


# Let's look for countries that sustained fiscal deficits with low inflation
library(zoo)
result <- data %>%
  group_by(Country_Name) %>%
  arrange(Time) %>%
  mutate(
    fiscal_deficit = ifelse(gov_balance_pct_gdp < -2, 1, 0),
    low_inflation = ifelse(WDI_Inflation <= 5, 1, 0)  # You can set your own inflation limit
  ) %>%
  mutate(
    rolling_fiscal_deficit = rollapply(fiscal_deficit, width = 3, FUN = sum, align = "right", fill = NA),
    rolling_low_inflation = rollapply(low_inflation, width = 3, FUN = sum, align = "right", fill = NA)
  ) %>%
  filter(rolling_fiscal_deficit == 3, rolling_low_inflation == 3) %>%
  select(Country_Name) %>%
  distinct()
