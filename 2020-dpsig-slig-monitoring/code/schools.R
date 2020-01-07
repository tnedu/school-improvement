library(DBI)
library(magrittr)
library(openxlsx)
library(tidyverse)

# Import CSI schools ----

schools_csi <-
  read.xlsx(
    "C:/Users/CA20397/OneDrive - TN Dept of Education/resources/school-improvement-policy-strategy/school-designations/school-designations-2018.xlsx",
    sheet = "Comprehensive Support"
  ) %>%
  filter(
    active == "Yes",
    !(system == 600 & school == 110), # Northfield Academy (adult high school)
    !(system == 792 & school == 8275) # The Excel Center (adult high school)
  ) %>%
  transmute(
    district = system,
    district_name = system_name,
    school,
    school_name
  ) %>%
  bind_rows(
    tibble(
      district = 792,
      district_name = "Shelby County",
      school = 2245,
      school_name = "Geeter School"
    )
  ) %>%
  left_join(
    dbGetQuery(
      connection_eis,
      "SELECT school_bu_id AS bu_id, district_no, school_no
      FROM school
      WHERE operational_status = 'A'"
    ),
    by = c("district" = "DISTRICT_NO", "school" = "SCHOOL_NO")
    ) %>%
  rename_all(tolower) %>%
  arrange(district_name, school_name)

# Extract and glue school IDs for SQL queries ----

school_ids <- str_c("(", glue::glue_collapse(schools_csi$bu_id, sep = ", "), ")")

# Extract district numbers for report rendering ----

districts_csi <-
  schools_csi %>%
  distinct(district, district_name) %>%
  arrange(district)

attributes(districts_csi)$spec <- NULL

# Write CSV ----

write_csv(schools_csi, "data/schools-csi.csv")
