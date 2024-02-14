if (!require("haven")) install.packages("haven")

library(haven)

data <- read_dta("data_raw/harvard-complexity.dta")
data <- rename(data,
      economic_complexity_index_sitc = sitc_eci,
      economic_complexity_index_gs = hs_eci,
      Country_Code = code,
      Year = year)

write_csv(data, "data_clean/harvard-complexity.csv")

