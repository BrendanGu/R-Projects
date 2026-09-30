library(tidyverse)
library(here)

qld_raw <- read_csv(here("data", "raw", "QLD_Reported_Offences_Rates.csv")) %>%
  rename(`Common Assault` = `Common Assault'`)

known_data_caveats <- tibble(
  offence_type = "Common Assault",
  break_date   = as.Date("2021-07-01"),
  note         = "From 1 July 2021, QLD Police were required to record all DFV-related offences. QGSO's own reporting shows this caused a large increase in recorded assault offences from this date — treat pre/post 2021-07 as not directly comparable."
)

category_totals <- c(
  "Offences Against the Person", "Offences Against Property", "Drug Offences",
  "Weapons Act Offences", "Good Order Offences", "Traffic and Related Offences"
)

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
  pivot_longer(-date, names_to = "offence_type", values_to = "rate_per_100k") %>%
  mutate(level = if_else(offence_type %in% category_totals, "category_total", "detail")) %>%
  left_join(known_data_caveats, by = "offence_type") %>%
  mutate(post_break = if_else(!is.na(break_date) & date >= break_date, TRUE, FALSE))

# --- Validation checks ---
n_missing <- sum(is.na(qld_long$rate_per_100k))
if (n_missing > 0) warning(n_missing, " missing values found") else message("Check passed: no missing rate values")

expected_months <- seq(min(qld_long$date), max(qld_long$date), by = "month")
missing_months <- setdiff(expected_months, sort(unique(qld_long$date)))
if (length(missing_months) > 0) {
  warning("Missing months detected")
} else {
  message("Check passed: monthly sequence continuous, ", min(qld_long$date), " to ", max(qld_long$date))
}

n_dupes <- qld_long %>% count(date, offence_type) %>% filter(n > 1) %>% nrow()
if (n_dupes > 0) warning(n_dupes, " duplicate rows found") else message("Check passed: no duplicate rows")

message("Known data caveats:")
print(known_data_caveats)

# --- Save ---
saveRDS(qld_long, here("data", "processed", "qld_offence_rates_long.rds"))
write_csv(qld_long, here("data", "processed", "qld_offence_rates_long.csv"))