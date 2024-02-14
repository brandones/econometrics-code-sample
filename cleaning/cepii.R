if (!require("haven")) install.packages("haven")

library(haven)

# The raw data file is 500MB, so I haven't put it in the Google Drive.
# Ask me if you want it, or download it from the CEPII website, dataset "TradeProd."

data <- read_dta("/home/brandon/Downloads/Levy Datasets Scratch/CEPII_TradeProd_TPe_V202201.dta")

