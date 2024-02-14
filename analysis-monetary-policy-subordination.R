# Import the IMF IFS Parquet file

library(arrow)
library(tidyverse)

data <- read_parquet("data_raw/imf-ifs.parquet", as_tibble = TRUE)

#  [1] "Country Name"
#  [2] "Time Period"
#  [3] "Capital Account, Total, Debit, US Dollars"
#  [4] "Current Account, Goods and Services, Net, US Dollars"
#  [5] "External Balance of Goods and Services, Nominal, Domestic Currency"
#  [6] "Final Consumption Expenditure, Nominal, Domestic Currency"
#  [7] "Financial Account, Other Investment, Net Acquisition of Financial Assets, US Dollars"
#  [8] "Financial Account, Other Investment, Other Equity, Net Acquisition of Financial Assets, Debt Instruments, US Dollars"
#  [9] "Financial Account, Other Investment, Other Equity, Net Acquisition of Financial Assets, US Dollars"
# [10] "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, Debt Securities, US Dollars"
# [11] "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, Equity and Investment Fund Shares, US Dollars"
# [12] "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, US Dollars"
# [13] "Financial, Interest Rates, 3-Month Interbank Interest, Percent per annum"
# [14] "Financial, Interest Rates, Average Cost of Funds, Percent per Annum"
# [15] "Financial, Interest Rates, Central Bank Borrowing Facility Rate"
# [16] "Financial, Interest Rates, Central Bank Certificates"
# [17] "Financial, Interest Rates, Certificates of Deposit, Percent per annum"
# [18] "Financial, Interest Rates, Corporate Paper Rate"
# [19] "Financial, Interest Rates, Deposit, Overnight"
# [20] "Financial, Interest Rates, Government Bond Yields, Medium Term, Percent per annum"
# [21] "Financial, Interest Rates, Government Bond Yields, Short- to Medium-Term, Percent per Annum"
# [22] "Financial, Interest Rates, Government Securities, Government Bonds, Percent per annum"
# [23] "Financial, Interest Rates, Government Securities, Treasury Bills, 3-month, Percent per annum"
# [24] "Financial, Interest Rates, Government Securities, Treasury Bills, Bond Equivalent, Percent per Annum"
# [25] "Financial, Interest Rates, Government Securities, Treasury Bills, Percent per annum"
# [26] "Financial, Interest Rates, Key Repurchase Agreement Rate, Percent per annum"
# [27] "Financial, Interest Rates, Lending Rate, Overnight"
# [28] "Financial, Interest Rates, Monetary Policy-Related Interest Rate, Percent per annum"
# [29] "Financial, Interest Rates, Money Market, Foreign Currency, Percent per Annum"
# [30] "Financial, Interest Rates, Money Market, Percent per annum"
# [31] "Fiscal, General Government, Net operating balance, 2001 Manual, Domestic Currency"
# [32] "Fund Accounts, Outstanding GRA, US Dollars"
# [33] "Fund Accounts, Outstanding Loans, US Dollars"
# [34] "General Government Final Consumption Expenditure, Nominal, Domestic Currency"
# [35] "Gross Capital Formation, Nominal, Domestic Currency"
# [36] "Gross Fixed Capital Formation, Real, Domestic Currency"
# [37] "Households Final Consumption Expenditure, Nominal, Domestic Currency"
# [38] "Households Final Consumption Expenditure, Real, Domestic Currency"
# [39] "Imports of Goods and Services, Nominal, Domestic Currency"
# [40] "International Liquidity, Total Reserves excluding Gold, US Dollars"
# [41] "International Reserves, Official Reserve Assets, US Dollars"
# [42] "Labor Markets, Wage Rates, Index"
# [43] "Non-profit Institutions Serving Households"
# [44] "Prices, Consumer Price Index, All items, Index"
# [45] "Prices, Export Price Index, All Commodities, Index"
# [46] "Prices, Import Price Index, All Commodities, Index"
# [47] "Private Sector Final Consumption Expenditure, Nominal, Domestic Currency"
# [48] "Supplementary Items, Reserves and Related items, US Dollars"

# Plot for the united states:
# - fiscal net operating balance
# only plot annually (so no 'Time Period containing "Q" or "M"')
# and only plot from 2000 onwards

ggplot(data %>% filter(`Country Name` == "United States" & `Time Period` >= 2000 & !grepl("[QM]", `Time Period`)),
       aes(x = `Time Period`, y = `Fiscal, General Government, Net operating balance, 2001 Manual, Domestic Currency`)) +
       geom_line() + coord_fixed(ratio = 1)

View(data %>%
     filter(`Country Name` == "United States" & `Time Period` >= 2000 & !grepl("[QM]", `Time Period`)) %>%
     select(`Time Period`, `Fiscal, General Government, Net operating balance, 2001 Manual, Domestic Currency`))
