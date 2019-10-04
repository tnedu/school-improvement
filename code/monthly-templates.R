# Create one monthly data collection template per district, pre-populated with
# district and school numbers, school names, and first instructional dates.

library(lubridate)
library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

# Data ----

connection_eis <- connect()

content_areas <- c("ELA", "Math", "Science", "Social Studies")

first_instructional_dates <-
  DBI::dbGetQuery(
    connection_eis, "
    select sch.district_no, sch.school_no, cal.school_year, cal.id_date
    from scal_id_days cal join school sch on cal.school_bu_id = sch.school_bu_id
    where cal.school_year = 2019
    "
  ) %>%
  rename_all(tolower) %>%
  transmute(
    system = district_no,
    school_number = school_no,
    year = school_year + 1,
    first_instructional_date = as_date(id_date)
  ) %>%
  group_by(system, school_number, year) %>%
  filter(first_instructional_date == min(first_instructional_date, na.rm = T)) %>%
  ungroup() %>%
  arrange(system, school_number, year)

schools_csi_2019 <-
  read.xlsx(
    "C:/Users/CA20397/OneDrive - TN Dept of Education/resources/school-improvement-policy-strategy/school-designations/school-designations-2018.xlsx",
    sheet = "Comprehensive Support"
  ) %>%
  filter(
    active == "Yes",
    !(system == 600 & school == 110), # Northfield Academy (adult high school)
    !(system == 792 & school == 8275) # The Excel Center (adult high school)
  ) %>%
  select(system:school_name) %>%
  rename(school_number = school) %>%
  bind_rows(
    tibble(
      system = 792, system_name = "Shelby County",
      school_number = 2245, school_name = "Geeter School"
    )
  ) %>%
  arrange(system_name, school_name)

template_blank <- "C:/Users/CA20397/SharePoint/School Improvement - Documents/Grant Monitoring/monthly-data-template-2019.xlsx"

# Output ----

output <-
  map(
    1:length(content_areas),
    ~ schools_csi_2019 %>%
      left_join(
        first_instructional_dates %>% select(-year),
        by = c("system", "school_number")
      ) %>%
      mutate_at(
        vars(first_instructional_date),
        funs(if_else(is.na(.), ymd("2019-08-05"), .))
      ) %>%
      mutate(district_number = system)
  ) %>%
  map2(
    content_areas,
    ~ .x %>%
      mutate(content_area = .y) %>%
      select(
        system, system_name, district_number, school_number, school_name,
        content_area, first_instructional_date
      )
  ) %>%
  reduce(bind_rows) %>%
  arrange(system, school_name) %>%
  group_by(system, system_name) %>%
  nest()

template_create <- function(df, system) {
  file_name <- str_c("data/templates-monthly/monthly-dpsig-data-", system, ".xlsx")
  template <- loadWorkbook(template_blank)
  writeData(template, "Teachers", df, startRow = 2, colNames = F)
  saveWorkbook(template, file_name, overwrite = T)
  rm(file_name, template)
}

walk2(.x = output$data, .y = output$system, template_create)
