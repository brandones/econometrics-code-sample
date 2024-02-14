library(data.table)
library(tidyverse)
library(arrow)
library(stringdist)

# Read in the first few rows to get the column names
first_few_rows <- fread("/home/brandon/Downloads/Levy Datasets Scratch/imf-ifs.csv", nrows = 5)
column_names <- colnames(first_few_rows)
  
pattern <- c(
  "Country Name",
  "Time Period",
  "Capital Account, Total, Debit, US Dollars",
  "Current Account, Goods and Services, Net, US Dollars",
  "External Balance of Goods and Services, Nominal, Domestic Currency",
  "Final Consumption Expenditure, Nominal, Domestic Currency",
  "Financial, Interest Rates, 3-Month Interbank Interest, Percent per annum",
  "Financial, Interest Rates, Average Cost of Funds, Percent per Annum",
  "Financial, Interest Rates, Central Bank Borrowing Facility Rate",
  "Financial, Interest Rates, Central Bank Certificates",
  "Financial, Interest Rates, Certificates of Deposit, Percent per annum",
  "Financial, Interest Rates, Corporate Paper Rate",
  "Financial, Interest Rates, Deposit, Overnight",
  "Financial, Interest Rates, Government Bond Yields, Medium Term, Percent per annum",
  "Financial, Interest Rates, Government Bond Yields, Short- to Medium-Term, Percent per Annum",
  "Financial, Interest Rates, Government Securities, Government Bonds, Percent per annum",
  "Financial, Interest Rates, Government Securities, Treasury Bills, 3-month, Percent per annum",
  "Financial, Interest Rates, Government Securities, Treasury Bills, Bond Equivalent, Percent per Annum",
  "Financial, Interest Rates, Government Securities, Treasury Bills, Percent per annum",
  "Financial, Interest Rates, Key Repurchase Agreement Rate, Percent per annum",
  "Financial, Interest Rates, Lending Rate, Overnight",
  "Financial, Interest Rates, Monetary Policy-Related Interest Rate, Percent per annum",
  "Financial, Interest Rates, Money Market, Foreign Currency, Percent per Annum",
  "Financial, Interest Rates, Money Market, Percent per annum",
  "Financial Account, Financial Derivatives (Other Than Reserves) and Employee Stock Options, Net Acquisition of Financial Assets, US Dollars",
  "Financial Account, Financial Derivatives (Other Than Reserves) and Employee Stock Options, Net Incurrence of Liabilities, US Dollars",
  "Financial Account, Financial Derivatives (Other Than Reserves) and Employee Stock Options, US Dollars",
  "Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Acquisition of Financial Assets, Debt Instruments, US Dollars",
  "Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Acquisition of Financial Assets, Equity and Investment Fund Shares, US Dollars",
  "Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Acquisition of Financial Assets, US Dollars",
  "Financial Account, Other Investment, Net Acquisition of Financial Assets, US Dollars",
  "Financial Account, Other Investment, Other Equity, Net Acquisition of Financial Assets, Debt Instruments, US Dollars",
  "Financial Account, Other Investment, Other Equity, Net Acquisition of Financial Assets, US Dollars",
  "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, Debt Securities, US Dollars",
  "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, Equity and Investment Fund Shares, US Dollars",
  "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, US Dollars",
  "Fiscal, Budgetary Central Government, Cash surplus/deficit [CSD_CA], 2001 Manual, Cash, Domestic Currency",
  "Fiscal, General Government, Net Lending/borrowing [NLB], 2001 Manual, Domestic Currency",
  "Fiscal, General Government, Net operating balance, 2001 Manual, Domestic Currency",
  "Fund Accounts, Outstanding GRA, US Dollars",
  "Fund Accounts, Outstanding Loans, US Dollars",
  "Gross Capital Formation, Nominal, Domestic Currency",
  "Gross Domestic Product, Nominal, Domestic Currency",
  "Gross Domestic Product, Real, Domestic Currency",
  "Gross Fixed Capital Formation, Real, Domestic Currency",
  "Households Final Consumption Expenditure, Nominal, Domestic Currency",
  "Households Final Consumption Expenditure, Real, Domestic Currency",
  "Imports of Goods and Services, Nominal, Domestic Currency",
  "International Liquidity, Total Reserves excluding Gold, US Dollars",
  "International Reserves, Official Reserve Assets, Gold (Including Gold Deposits and, If Appropriate, Gold Swapped) , US Dollars",
  "International Reserves, Official Reserve Assets, US Dollars",
  "Labor Markets, Wage Rates, Index",
  "Prices, Consumer Price Index, All items, Index",
  "Prices, Consumer Price Index, All items, Percentage change, Corresponding period previous year, Percent",
  "Prices, Consumer Price Index, All items, Percentage change, Previous period, Percent",
  "Prices, Export Price Index, All Commodities, Index",
  "Prices, Import Price Index, All Commodities, Index",
  "Private Sector Final Consumption Expenditure, Nominal, Domestic Currency",
  "Supplementary Items, Capital Account (Excludes Reserves and Related Items), US Dollars",
  "Supplementary Items, Capital Account, Credit (Excludes Reserves and Related Items), US Dollars",
  "Supplementary Items, Current Account, Net (Excluding Exceptional Financing), US Dollars",
  "Supplementary Items, Current Acct + Capital Acct + Financial Acct, US Dollars",
  "Supplementary Items, Financial Account, Net (Excluding Exceptional Financing), US Dollars",
  "Supplementary Items, Net Credit and Loans from the IMF (Excluding Reserve Position), US Dollars",
  "Supplementary Items, Reserve Assets (with Fund Record), US Dollars",
  "Supplementary Items, Reserves and Related items, US Dollars",
  "Total International Reserves, US Dollars (gold at 35 SDRs per ounce)",
  "Total Reserves, US Dollars (Gold at Market Price)"
  )

desired_columns <- character(0)
for (p in pattern) {
  matching_columns <- column_names[startsWith(column_names, p)]
  if (length(matching_columns) == 0) {
    # Find the most similar column name using string distance
    closest_column <- column_names[which.min(stringdist(p, column_names))]
    stop(paste("Error: No match found for pattern:", p, "\nMost similar column name:", closest_column))
  } else {
    desired_columns <- c(desired_columns, matching_columns)
  }
}
desired_columns

# Read in the data with selected columns
data <- fread("/home/brandon/Downloads/Levy Datasets Scratch/imf-ifs.csv",
              select = desired_columns, showProgress = TRUE)

# Remove parenthetical name code at the end of column names
names(data) <- gsub("\\s\\([^()]*\\)$", "", names(data))

# Treat 0 as NA for certain columns
data[, c("Fund Accounts, Outstanding GRA, US Dollars",
         "Fund Accounts, Outstanding Loans, US Dollars") := lapply(.SD, function(x) replace(x, x == 0, NA)),
   .SDcols = c("Fund Accounts, Outstanding GRA, US Dollars",
               "Fund Accounts, Outstanding Loans, US Dollars")]

# Drop rows with no data
check_columns <- setdiff(names(data), c("Country Name", "Time Period"))
data <- data[!apply(is.na(data[, ..check_columns]), 1, all), ]

data <- data %>% rename(Country_Name = "Country Name")

data_annual <- data %>%
  filter(!str_detect(`Time Period`, "Q|M")) %>%
  mutate(Time = as.numeric(`Time Period`)) %>%
  select(-`Time Period`)
arrow::write_parquet(data_annual, "./data_clean/imf-ifs-annual.parquet")

# Write the data to a parquet file
arrow::write_parquet(data, "./data_clean/imf-ifs.parquet")
