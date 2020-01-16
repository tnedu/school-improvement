# Create student chronic absenteeism, student discipline, and student mobility
# graphs for monthly reports.

# Josh Carson

library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

# Data ----

con_eis <- connect()

demographics_student <-
  get_demos(years = 2019:2020, districts = 1:1000) %>%
  tidy_demos(type = "eis to stu")

chronic_student <-
  get_chra(years = 2019:2020, districts = 1:1000) %>%
  tidy_chra(demos_tidy = demographics_student, type = "eis to stu")

discipline_student <-
  get_disc(years = 2019:2020, districts = 1:1000) %>%
  tidy_disc(demos = demographics_student, type = "eis to stu")

schools_csi_2019 <-
  read.xlsx(
    "C:/Users/CA20397/SharePoint/School Improvement - Documents/School Lists/school-designations-2018.xlsx",
    sheet = "Comprehensive Support"
  ) %>%
  filter(
    active == "Yes",
    !(system == 600 & school == 110), # Northfield Academy (adult high school)
    !(system == 792 & school == 8275) # The Excel Center (adult high school)
  ) %>%
  select(system:school_name) %>%
  bind_rows(
    tibble(
      system = 792, system_name = "Shelby County",
      school = 2245, school_name = "Geeter School"
    )
  ) %>%
  arrange(system_name, school_name)

# Chronic Absenteeism ----

chronic_school <- tidy_chra(chronic_student, demos_tidy = demographics_student, type = "stu to sch")

chronic_school_csi <-
  chronic_school %>%
  inner_join(schools_csi_2019 %>% select(system, school))

chronic_district_csi <-
  chronic_school_csi %>%
  group_by(year, system, system_name) %>%
  summarize_at(vars(absences, starts_with("n_")), sum) %>%
  ungroup() %>%
  mutate(
    pct_chronically_absent_10_to_19 = n_chronically_absent_10_to_19,
    pct_chronically_absent_20_plus = n_chronically_absent_20_plus,
    pct_chronically_absent = n_chronically_absent
  ) %>%
  mutate_at(vars(starts_with("pct_")), funs(round(100 * . / n_students_total, 1)))
