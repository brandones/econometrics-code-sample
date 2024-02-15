if (!require("haven")) install.packages("haven")

library(tidyverse)
library(haven)

data <- read_dta("data_0_raw/harvard-complexity.dta")
data <- rename(data,
      economic_complexity_index_sitc = sitc_eci,
      economic_complexity_index_gs = hs_eci,
      Country_Code = code,
      Year = year)

write_csv(data, "data_1_clean/harvard-complexity.csv")

