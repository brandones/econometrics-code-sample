library(tidyverse)

names_imf_gfs = c(
  gov_revenue_pct_gdp = "Revenue (G1|_Z)",
  gov_expenditure_pct_gdp = "Expenditure (G2M|_Z)",
  gov_balance_pct_gdp = "Net lending (+) / Net borrowing (-) (GNLB|_Z)"
)

names_wb_wdi = c(
  CAB = "Current_Account_Balance",
  WDI_Inflation = "Inflation"
)