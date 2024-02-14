library(tidyverse)

data <- read_csv("./data_raw/imf-pub-fi-hist.csv")

# revenue 	Government revenue, percent of GDP
# expenditure 	      Government expenditure, percent of GDP
# interest_exp  	    Government interest expense, percent of GDP
# prim_expenditure  	Government primary expenditure, percent of GDP
# prim_balance	      Government primary balance, percent of GDP
# debt  	  Government gross debt, percent of GDP
# rltir	    Real long-term interest rate, percent
# rgc	      Real GDP growth rate, percent
# GG_budg	  sector coverage indicator for rev, exp, ie (0 for central gov't, 1 for general gov't)
# GG_debt	  sector coverage indicator for debt (0 for central gov't, 1 for general gov't)


