library(here)
library(tidyverse)

# load processed gapminder code
df_gapminder <- read_csv(here("data", "processed", "gapminder_processed.csv")) %>%
  select(-name)

# load plotly codes from lecture notes
df_plotly_codes <- read_csv(here("data", "raw", "plotly_country_codes.csv")) %>%
  mutate(code = CODE, country = COUNTRY) %>%
  select(code, country) %>%
  mutate(country = str_replace(country, "Bahamas, The", "Bahamas"),
         country = str_replace(country, "Congo, Democratic Republic of the", "Congo, Dem. Rep."),
         country = str_replace(country, "Congo, Republic of the", "Congo, Rep."),
         country = str_replace(country, "Gambia, The", "Gambia"),
         country = str_replace(country, "Kyrgyzstan", "Kyrgyz Republic"),
         country = str_replace(country, "Laos", "Lao"),
         country = str_replace(country, "Macedonia", "Macedonia, FYR"),
         country = str_replace(country, "Korea, North", "North Korea"),
         country = str_replace(country, "Slovakia", "Slovak Republic"),
         country = str_replace(country, "Korea, South", "South Korea"))
  

df_merged <- left_join(df_gapminder, df_plotly_codes)

write_csv(df_merged, here("data", "processed", "gapminder_processed_codes.csv"))