# WDI country names are already the canonical ones (in country_codes.csv)

library(tidyverse)

data <- read_csv("./data_raw/wb-wdi.csv", na = "..")

name_mapping <- c(
  GDP_Growth = "GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]",
  GDP_Per_Capita = "GDP per capita (current US$) [NY.GDP.PCAP.CD]",
  GDP_Per_Capita_Growth = "GDP per capita growth (annual %) [NY.GDP.PCAP.KD.ZG]",
  GDP = "GDP (current US$) [NY.GDP.MKTP.CD]",
  GDP_Real = "GDP (constant 2015 US$) [NY.GDP.MKTP.KD]",
  Govt_Debt_Pct_GDP = "Central government debt, total (% of GDP) [GC.DOD.TOTL.GD.ZS]",
  Govt_Debt = "Central government debt, total (current LCU) [GC.DOD.TOTL.CN]",
  Current_Account_Balance_Pct_GDP = "Current account balance (% of GDP) [BN.CAB.XOKA.GD.ZS]",
  Current_Account_Balance = "Current account balance (BoP, current US$) [BN.CAB.XOKA.CD]",
  External_Balance_Pct_GDP = "External balance on goods and services (% of GDP) [NE.RSB.GNFS.ZS]",
  External_Balance = "External balance on goods and services (current US$) [NE.RSB.GNFS.CD]",
  Exchange_Rate = "Official exchange rate (LCU per US$, period average) [PA.NUS.FCRF]",
  Inflation = "Inflation, consumer prices (annual %) [FP.CPI.TOTL.ZG]",
  Domestic_Credit_Pct_GDP = "Domestic credit provided by financial sector (% of GDP) [FS.AST.DOMS.GD.ZS]",
  Financial_Assets_Pct_GDP = "Net acquisition of financial assets (% of GDP) [GC.AST.TOTL.GD.ZS]",
  Firms_Getting_Finance_Investment_Pct = "Firms using banks to finance investment (% of firms) [IC.FRM.BNKS.ZS]",
  Firms_Getting_Finance_Working_Capital_Pct = "Firms using banks to finance working capital (% of firms) [IC.FRM.BKWC.ZS]",
  Net_Financial_Account_Cur_USD = "Net financial account (BoP, current US$) [BN.FIN.TOTL.CD]",
  NAFA_Perc_GDP = "Net acquisition of financial assets (% of GDP) [GC.AST.TOTL.GD.ZS]",
  Nonfinancial_Assets_Investment_GDP = "Net investment in nonfinancial assets (% of GDP) [GC.NFN.TOTL.GD.ZS]",
  External_Debt_Pct_GNI = "External debt stocks (% of GNI) [DT.DOD.DECT.GN.ZS]",
  External_Debt_Cur_USD = "External debt stocks, total (DOD, current US$) [DT.DOD.DECT.CD]",
  Private_External_Debt_Cur_USD = "External debt stocks, private nonguaranteed (PNG) (DOD, current US$) [DT.DOD.DPNG.CD]",
  Public_External_Debt_Cur_USD = "External debt stocks, public and publicly guaranteed (PPG) (DOD, current US$) [DT.DOD.DPPG.CD]",
  Present_Value_External_Debt_Cur_USD = "Present value of external debt (current US$) [DT.DOD.PVLX.CD]",
  Total_Reserves = "Total reserves (includes gold, current US$) [FI.RES.TOTL.CD]",
  Gross_Savings_Pct_GDP = "Gross savings (% of GDP) [NY.GNS.ICTR.ZS]",
  Gross_Savings_Cur_USD = "Gross savings (current US$) [NY.GNS.ICTR.CD]",
  Gross_Domestic_Savings_Pct_GDP = "Gross domestic savings (% of GDP) [NY.GDS.TOTL.ZS]",
  Gross_Domestic_Savings_Cur_USD = "Gross domestic savings (current US$) [NY.GDS.TOTL.CD]",
  Portfolio_Investment = "Portfolio investment, net (BoP, current US$) [BN.KLT.PTXL.CD]",
  FDI_Net_Outflows = "Foreign direct investment, net outflows (BoP, current US$) [BM.KLT.DINV.CD.WD]",
  FDI_Net_Inflows = "Foreign direct investment, net inflows (BoP, current US$) [BX.KLT.DINV.CD.WD]",
  FDI_Net = "Foreign direct investment, net (BoP, current US$) [BN.KLT.DINV.CD]",
  Gross_Capital_Formation_Pct_GDP = "Gross capital formation (% of GDP) [NE.GDI.TOTL.ZS]",
  Gross_Capital_Formation_Cur_USD = "Gross capital formation (current US$) [NE.GDI.TOTL.CD]",
  GFCF_Pct_GDP = "Gross fixed capital formation (% of GDP) [NE.GDI.FTOT.ZS]",
  GFCF_Cur_USD = "Gross fixed capital formation (current US$) [NE.GDI.FTOT.CD]",
  GFCF_Private_Pct_GDP = "Gross fixed capital formation, private sector (% of GDP) [NE.GDI.FPRV.ZS]",
  Net_Capital_Account = "Net capital account (BoP, current US$) [BN.TRF.KOGT.CD]",
  Net_Primary_Income = "Net primary income (BoP, current US$) [BN.GSR.FCTY.CD]",
  Net_Secondary_Income = "Net secondary income (BoP, current US$) [BN.TRF.CURR.CD]",
  Broad_Money_LCU = "Broad money (current LCU) [FM.LBL.BMNY.CN]",
  Broad_Money_Reserves_Ratio = "Broad money to total reserves ratio [FM.LBL.BMNY.IR.ZS]",
  Exports = "Exports of goods and services (BoP, current US$) [BX.GSR.GNFS.CD]",
  Imports = "Imports of goods and services (BoP, current US$) [BM.GSR.GNFS.CD]",
  GDP_Constant_LCU = "GDP (constant LCU) [NY.GDP.MKTP.KN]",
  GDP_Current_LCU = "GDP (current LCU) [NY.GDP.MKTP.CN]",
  GDP_Deflator = "GDP deflator: linked series (base year varies by country) [NY.GDP.DEFL.ZS.AD]",
  C_Total = "Final consumption expenditure (current US$) [NE.CON.TOTL.CD]",
  C_Gov = "General government final consumption expenditure (current US$) [NE.CON.GOVT.CD]",
  C_Household = "Households and NPISHs Final consumption expenditure (current US$) [NE.CON.PRVT.CD]"
)
name_mapping_df <- data.frame(old_name = names(name_mapping), new_name = unname(name_mapping), stringsAsFactors = FALSE)
write.table(name_mapping_df, file = "wb-wdi-name-mapping.csv", sep = ",", row.names = FALSE)
# name_mapping_file <- read.table("wb-wdi-name-mapping.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
# name_mapping <- setNames(as.vector(name_mapping_file$new_name), name_mapping_file$old_name)


data <- rename(data,
               Time = "Time",
               Time_Code = "Time Code",
               Country_Name = "Country Name",
               Country_Code = "Country Code"
)
data <- data %>% rename(!!!name_mapping)


data <- data %>%
  arrange(Country_Name, Time) %>%
  group_by(Country_Name)

data <- data[data$Country_Name != "", ]

data <- data %>%
  mutate(Net_Financial_Account_Cur_USD_Negative = -Net_Financial_Account_Cur_USD)

write_csv(data, "./data_clean/wb-wdi.csv")

