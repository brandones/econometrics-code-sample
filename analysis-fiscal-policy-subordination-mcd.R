# 3. Fiscal Policy Subordination
#
# To what extent do governments decrease spending to deal with balance of payment constraints? The flipside to this
# question is how much of fiscal deficits leak out to become the foreign sectors surplus? How strong is the
# counter-cyclicality between the fiscal and domestic private balances relative to the relation between the fiscal
# and foreign sector balance?
#
# Need:
#   Public sector balance   - imf-pub-fi-hist
#   Current account balance - wb-wdi
#   Private sector balance  (= Current - Public)
#   Growth                  - imf-pub-fi-hist
#   Tax revenue             - imf-pub-fi-hist (as general government revenue)
#   Government spending     - imf-pub-fi-hist (as general government expenditure)
#   Unemployment            - wb-gem-annual
#
# - "when GDP goes up, does the fiscal balance move in the direction of surplus?"
# - "when GDP goes up, does the current account move in the direction of surplus?"
# - "when the fiscal balance moves in the direction of surplus, does the current account move in the direction of surplus?"
#
# Use wdi-and-gem-with-imf-pub-fi.csv

library(tidyverse)
if (!require("broom")) install.packages("tidymodels")
library(broom)
if (!require(xlsx)) install.packages("xlsx")
library(xlsx)

data <- read_csv("./wdi-and-gem-with-imf-pub-fi.csv")
data_mcd <- read_csv('./data_raw/imf-reo-mcd.csv')

data_mcd$Country_Name[data_mcd$Country_Name == "Yemen, Republic of"] <- "Yemen, Rep."
data_mcd$Country_Name[data_mcd$Country_Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."

data <- inner_join(data, data_mcd, by = c("Country_Name", "Time"))

mcd_name_mapping <- c(
  Govt = "Overal fiscal balance, General government, percent of GDP (GGXOFB_GDP)",
  CAB = "Current account balance, in percent of GDP (BCA_GDP_GDP_PT)",
  CAB_USD = "Balance on Current Account, US Dollars (BCA_USD)"
)
data <- data %>% rename(!!!mcd_name_mapping)

data <- data %>% mutate(Govt_USD = Govt * GDP / 100)

# Recession: GDP falls
# Slowdown: GDP growth is less than 75% of the previous year's growth, or less than 75% of the growth two years ago
# Expansion: GDP growth is greater than 1%
# Acceleration: GDP growth is greater than the previous year's growth, or greater than the growth two years ago
data <- data %>%
  mutate(Recession = ifelse(GDP_Growth < 0, 1, 0),
         Slowdown = ifelse((GDP_Growth > 0 & (GDP_Growth < (lag(GDP_Growth) + lag(GDP_Growth, 2)) / 2 * 0.75))
                           |  GDP_Growth < 0, 1, 0),
         Expansion = ifelse(GDP_Growth > 2, 1, 0),
         Acceleration = ifelse(GDP_Growth > 0 & GDP_Growth > (lag(GDP_Growth) + lag(GDP_Growth, 2)) / 2, 1, 0))

data <- data %>%
  mutate(Recession_Zone = lag(Recession) | Recession,
          Slowdown_Zone = (lag(Slowdown) | Slowdown) | (Slowdown & lead(Slowdown)),
          Expansion_Zone = (lag(Expansion) & Expansion) | (Expansion & lead(Expansion)),
          Acceleration_Zone = (lag(Acceleration, 2) & lag(Acceleration)) | (lag(Acceleration) & Acceleration)
  )

# This stabilizes the data using a variant of weighted least squares. The 'variance' (in a loose sense)
# is estimated using the mean of the absolute value of the data series. Where this estimated variance
# is below the average absolute value of the data series, the estimated variance is replaced with the
# average absolute value of the data series. This is done to avoid the estimated variance being too
# small. So the weight function winds up being horizontal in time
# up to a point, and then linear from there on (or vice versa, in the unusual case that variance in the
# past was greater).
# The data series is then divided by the estimated variance. This is done separately for each country.
correct_wls <- function(df, y_var) {
  # Extract non-NA rows
  data_non_na <- df[!is.na(df[[y_var]]) & !is.na(df$Time), ]
  
  # If not enough data, return all NA values matching original length
  if (nrow(data_non_na) < 5) {
    return(rep(NA, nrow(df)))
  }

  # Fit a linear model to the absolute value of the data. We use this to estimate how variance
  # changes over time. We can then divide out this estimated variance.
  data_non_na$data_abs <- abs(data_non_na[[y_var]])
  variance_model <- lm(data_abs ~ Time, data = data_non_na)
  data_non_na$pred_var <- predict(variance_model, data_non_na)
  
  # Ensure that all predicted variances are positive
  y_scale_factor <- abs(mean(data_non_na[[y_var]]))
  data_non_na$pred_var <- pmax(data_non_na$pred_var, y_scale_factor)

  # Divide out the predicted variance
  data_non_na$y_var_stabilized <- data_non_na[[y_var]] / data_non_na$pred_var * y_scale_factor
  
  # Initialize a vector of NA values
  res <- rep(NA, nrow(df))
  
  # Fill in only for the non-NA values in the original data
  res[!is.na(df[[y_var]])] <- data_non_na$y_var_stabilized

  return(res)
}

data <- data %>%
  group_by(Country_Name) %>%
  group_modify(~mutate(.x, Govt_Stabilized = correct_wls(.x, "Govt"))) %>%
  group_modify(~mutate(.x, CAB_Stabilized = correct_wls(.x, "CAB"))) %>%
  ungroup()

# Define the correlation calculation function
calculate_correlation <- function(data_subset) {
  if (nrow(data_subset) == 0) {
    return(NA)
  }
  cor(data_subset[, c("GDP_Growth", "Govt_Stabilized", "CAB_Stabilized", "UnemploymentRate")], use = "pairwise.complete.obs")
}

# Apply the function to different subsets of the data
data_nested <- data %>%
  group_by(Country_Name) %>%
  nest() %>%
  mutate(
    correlation_Anytime = map(data, ~ calculate_correlation(.x)),
    correlation_Recession = map(data, ~ calculate_correlation(.x[.x$Recession_Zone, ])),
    correlation_Slowdown = map(data, ~ calculate_correlation(.x[.x$Slowdown_Zone, ])),
    correlation_Expansion = map(data, ~ calculate_correlation(.x[.x$Expansion_Zone, ])),
    correlation_Acceleration = map(data, ~ calculate_correlation(.x[.x$Acceleration_Zone, ]))
  )

# Define a function to tidy up the correlation matrices
tidy_correlation <- function(mat, condition) {
  correlation <- as.data.frame(mat)
  correlation$variable1 <- rownames(correlation)
  correlation_long <- tidyr::pivot_longer(correlation, -variable1, names_to = "variable2", values_to = "correlation")
  correlation_long <- correlation_long[!is.na(correlation_long$correlation), ]
  correlation_long <- correlation_long[correlation_long$variable1 != correlation_long$variable2, ]
  correlation_long$condition <- condition
  return(correlation_long)
}

# Apply this function to each correlation matrix
data_correlations_Anytime <- data_nested %>%
  mutate(tidy_corr_Anytime = purrr::map(correlation_Anytime, tidy_correlation, condition = "Anytime")) %>%
  select(Country_Name, tidy_corr_Anytime) %>%
  unnest(cols = c(tidy_corr_Anytime))

data_correlations_Recession <- data_nested %>%
  mutate(tidy_corr_Recession = purrr::map(correlation_Recession, tidy_correlation, condition = "Recession")) %>%
  select(Country_Name, tidy_corr_Recession) %>%
  unnest(cols = c(tidy_corr_Recession))

data_correlations_Slowdown <- data_nested %>%
  mutate(tidy_corr_Slowdown = purrr::map(correlation_Slowdown, tidy_correlation, condition = "Slowdown")) %>%
  select(Country_Name, tidy_corr_Slowdown) %>%
  unnest(cols = c(tidy_corr_Slowdown))

data_correlations_Expansion <- data_nested %>%
  mutate(tidy_corr_Expansion = purrr::map(correlation_Expansion, tidy_correlation, condition = "Expansion")) %>%
  select(Country_Name, tidy_corr_Expansion) %>%
  unnest(cols = c(tidy_corr_Expansion))

data_correlations_Acceleration <- data_nested %>%
  mutate(tidy_corr_Acceleration = purrr::map(correlation_Acceleration, tidy_correlation, condition = "Acceleration")) %>%
  select(Country_Name, tidy_corr_Acceleration) %>%
  unnest(cols = c(tidy_corr_Acceleration))

# Combine the two data frames
data_correlations <- bind_rows(data_correlations_Anytime, data_correlations_Recession,
                               data_correlations_Slowdown, data_correlations_Expansion,
                               data_correlations_Acceleration)

# Arrange the variable pairs in alphabetical order to avoid duplicate correlations
data_correlations <- data_correlations %>%
  mutate(variable_pair = pmap_chr(list(variable1, variable2), ~ paste(sort(c(..1, ..2)), collapse = "_"))) %>%
  select(-variable1, -variable2)

# Remove duplicates
data_correlations <- data_correlations %>%
  distinct()

# Pivot to wide format
data_correlations <- data_correlations %>%
  pivot_wider(names_from = variable_pair, values_from = correlation)

# Clean up & shorten all the column names
data_correlations <- data_correlations %>%
  rename(GDP_Gov = GDP_Growth_Govt_Stabilized,
         GDP_CAB = CAB_Stabilized_GDP_Growth,
         Gov_CAB = CAB_Stabilized_Govt_Stabilized,
         GDP_Unemp = GDP_Growth_UnemploymentRate,
         CAB_Unemp = CAB_Stabilized_UnemploymentRate,
         Gov_Unemp = Govt_Stabilized_UnemploymentRate,
         Period = condition) %>%
  rowwise() %>%
  filter(!all(is.na(c_across(c(GDP_Gov, GDP_CAB, Gov_CAB, GDP_Unemp, CAB_Unemp, Gov_Unemp))))) %>%
  ungroup()

# Write to CSV
write_csv(data_correlations, "output/fiscal_policy_subordination_mcd.csv")

export_data <- data %>%
  filter(Country_Name %in% data_correlations$Country_Name) %>%
  select(Country_Name, Time, GDP_Growth, Govt_USD, CAB_USD,
         GDP, CAB, revenue, expenditure, Recession_Zone, Slowdown_Zone, Expansion_Zone, Acceleration_Zone,
         UnemploymentRate) %>%
  filter(rowSums(is.na(select(., -Country_Name, -Time))) != ncol(.) - 2) %>%
  rename(Govt_Revenue = revenue,
         Govt_Expenditure = expenditure)

# Restore the original variable names
reverse_name_mapping <- setNames(names(name_mapping), name_mapping)
reverse_name_mapping <- reverse_name_mapping[names(reverse_name_mapping) %in% names(export_data)]
export_data <- export_data %>% rename(!!!reverse_name_mapping)

write_csv(export_data, "output/fiscal_policy_subordination_data_mcd.csv")

# Output some diagnostics

# The mean absolute value of GDP_Gov
print(data_correlations %>%
  ungroup() %>%
  summarise(mean(abs(GDP_Gov), na.rm=TRUE), mean(abs(GDP_CAB), na.rm=TRUE), mean(abs(Gov_CAB), na.rm=TRUE)))

# Print quintiles of abs(GDP_Gov)
print(data_correlations %>%
  ungroup() %>%
  pull(GDP_Gov) %>%
  abs() %>%
  quantile(probs = seq(0, 1, 0.2), na.rm=TRUE))
