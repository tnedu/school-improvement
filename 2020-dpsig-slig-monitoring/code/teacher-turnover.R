library(haven)
library(lubridate)
library(magrittr)
library(openxlsx)
library(tidyverse)

# Functions ----

strip_attributes <- function(x, df = T) {
  if(df) attributes(x)$spec <- NULL
  if(!df) {
    attributes(x)$label <- NULL
    attributes(x)$format.stata <- NULL
  }
  return(x)
}

# Input ----

retention <-
  list(
    y19 = "N:/Data Mgmt and Reporting/DU_Data/Educators/Educator_Retention/Final_Cleaned/retention_1718_1819.dta",
    y18 = "N:/Data Mgmt and Reporting/DU_Data/Educators/Educator_Retention/Final_Cleaned/retention_1617_1718.dta",
    y17 = "N:/Data Mgmt and Reporting/DU_Data/Educators/Educator_Retention/Final_Cleaned/retention_1516_1617.dta"
  ) %>%
  map(read_dta) %>%
  map(~ mutate_all(.x, strip_attributes, df = F))

# Explore input ----

map(retention, ~ n_distinct(.x$final_district, .x$final_school))

map(retention, ~ count(.x, final_role))

# Clean teacher turnover data for CSI schools ----

turnover_school <-
  retention %>%
  imap(
    ~ .x %>%
      filter(final_role == "teacher") %>%
      mutate(year = as.numeric(str_replace(.y, "y", ""))) %>%
      transmute(
        district = final_district,
        school = final_school,
        period = str_c(
          "20", year - 2, "-", year - 1,
          " to 20", year - 1, "-", year
        ),
        total,
        pct_exited = 100 - pct_retained,
        loe4,
        # The existing calculation uses total teachers for the denominator, but
        # total teachers who earned LOE 4+ is more appropriate for our purposes
        # here.
        pct_exited_loe4 = 100 - 100 * retained_loe4 / loe4
      ) %>%
      group_by(district, school, period) %>%
      nest(.key = "n") %>%
      mutate(
        pct = map(
          n,
          ~ .x %>%
            select(starts_with("pct")) %>%
            gather(type, pct) %>%
            rename(pct_exited = pct)
        ),
        n = map(
          n,
          ~ .x %>%
            select(total, loe4) %>%
            gather(type, denom)
        )
      ) %>%
      unnest() %>%
      filter(denom >= 5) %>%
      select(district, school, period, type, denom, pct_exited) %>%
      group_by(period, type) %>%
      mutate(
        quintile = if_else(
          pct_exited == 0,
          1,
          ceiling(percent_rank(pct_exited) * 5)
        )
      ) %>%
      ungroup() %>%
      inner_join(
        schools_csi %>% select(district, school),
        by = c("district", "school")
      )
  ) %>%
  reduce(bind_rows) %>%
  arrange(district, school, period)

# Output ----

write_csv(turnover_school, "data/turnover-school.csv")
