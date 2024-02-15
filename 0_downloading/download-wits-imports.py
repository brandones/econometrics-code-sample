import pandas as pd
import world_trade_data as wits

pd.set_option('display.max_rows', 6)

countries = wits.get_countries().index.values
countries
imports = {}
for country in countries:
  try:
    imports[country] = wits.get_indicator('MPRT-TRD-VL', reporter=country, year='all', partner="wld")
  except Exception as e:
    print("Error with ", country, ": ", e)
  else:
    print(country, ": ", imports[country].shape)

# Flatten imports dictionary so that it is a single dataframe with country as a column
imports_df = pd.concat(imports, axis=0)
imports_df = imports_df.reset_index()
imports_df = imports_df.rename(columns={'level_0': 'Country', 'level_1': 'Year'})
imports_df = imports_df.set_index(['Country', 'Year'])

# Save to CSV
imports_df.to_csv('data_0_raw/wits-imports.csv')
