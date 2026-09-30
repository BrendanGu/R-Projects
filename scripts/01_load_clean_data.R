library(tidyverse)
library(here)

qld_raw <- read_csv(here("data", "raw", "QLD_Reported_Offences_Rates.csv"))

qld_clean <- qld_raw %>%
  rename(month_year = `Month Year`) %>%
  mutate(
    month_num = match(toupper(str_sub(month_year, 1, 3)), toupper(month.abb)),
    yy        = as.integer(str_sub(month_year, 4, 5)),
    year_num  = if_else(yy > 50, 1900L + yy, 2000L + yy),
    date      = make_date(year_num, month_num, 1)
  ) %>%
  select(-month_year, -month_num, -yy, -year_num) %>%
  relocate(date)

qld_long <- qld_clean %>%
  pivot_longer(-date, names_to = "offence_type", values_to = "rate_per_100k")

saveRDS(qld_long, here("data", "processed", "qld_offence_rates_long.rds"))

write_csv(qld_long, here("data", "processed", "qld_offence_rates_long.csv"))