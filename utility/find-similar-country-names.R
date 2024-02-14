if (!require("stringdist")) install.packages("stringdist")

# Load the package
library(stringdist)

# Get unique country names from both data frames
wdi_countries <- unique(wdi$Country_Name)
gem_countries <- unique(gem$Country_Name)

# Create an empty data frame to store possible matches
possible_matches <- data.frame()

# For each country in WDI, get the most similar country name in GEM
for (wdi_country in wdi_countries) {
  similarities <- stringdist::stringdistmatrix(wdi_country, gem_countries, method = "jw")
  min_similarity_index <- which.min(similarities)
  possible_match <- gem_countries[min_similarity_index]
  # We add an entry to the dataframe only if the match is close but the strings are not equal
  if (similarities[min_similarity_index] < 0.3 && wdi_country != possible_match) {
    possible_matches <- rbind(possible_matches, data.frame(WDI = wdi_country, GEM = possible_match))
  }
}

# Now, 'possible_matches' should have a mapping of country names that are different but likely refer to the same country
possible_matches
