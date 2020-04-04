library(lubridate)
library(magrittr)
library(openxlsx)
library(tidyverse)

connection_sde <-
  DBI::dbConnect(
    RJDBC::JDBC(
      "oracle.jdbc.OracleDriver",
      classPath = Sys.getenv("jar_path")
    ),
    Sys.getenv("sde_connection_string"),
    "SDE_DIR", Sys.getenv("sde_password")
  )

# Input ----

# School information from accountability files

# For more-efficient file loading and clearer documentation, copy input files
# that exist elsewhere. Only copy if they are missing from the data folder.

missing_input <- F

if(missing_input) {
  
  file.copy(
    from = "N:/ORP_accountability/data/2019_final_accountability_files/school_designations_file.xlsx",
    to = "data/2019-school-designations.xlsx",
    overwrite = T
  )
  
  file.copy(
    from = "N:/ORP_accountability/data/2018_final_accountability_files/school_designations_file.xlsx",
    to = "data/2018-school-designations.xlsx",
    overwrite = T
  )
  
  file.copy(
    from = "N:/ORP_accountability/projects/2017_school_accountability/priority_exit_improving.csv",
    to = "data/2017-priority-exit.csv",
    overwrite = T
  )
  
  file.copy(
    from = "N:/ORP_accountability/projects/2015/2015_Priority_Cusp/priority_cusp.xlsx",
    to = "data/2015-priority-cusp.xlsx",
    overwrite = T
  )
  
  # This cohort of Priority schools is commonly referred to as 2015 Priority
  # schools, but they were actually identified in 2014.
  
  file.copy(
    from = "N:/ORP_accountability/projects/2014/2014_Final_school_accountability/2015_priority_list_withSR.xlsx",
    to = "data/2014-priority-schools.xlsx",
    overwrite = T
  )
  
  file.copy(
    from = "N:/ORP_accountability/data/2012/raw/school_designations_simple2012.xlsx",
    to = "data/2012-school-designations.xlsx",
    overwrite = T
  )
  
}

# School information from School Directory

schools_sde <-
  DBI::dbGetQuery(
    connection_sde,
    read_file("code/school-directory.sql")
  ) %>%
  rename_all(tolower) %>%
  arrange(district_name, school_name)

# Explore input ----

# Confirm that the two 2014 Priority lists in the data folder are identical.

read.xlsx("data/2014-priority-public.xlsx", startRow = 2, cols = 1:4) %>%
  janitor::clean_names() %>%
  filter(!is.na(system_number), !is.na(school_number)) %>%
  select(system_number, school_number) %>%
  mutate(in_1 = T) %>%
  full_join(
    read.xlsx("data/2014-priority.xlsx", startRow = 2, cols = 1:4) %>%
      janitor::clean_names() %>%
      filter(!is.na(district_number), !is.na(school_number)) %>%
      select(system_number = district_number, school_number) %>%
      mutate_all(as.numeric) %>%
      mutate(in_2 = T)
  )

# Historical Priority school list ----

# This list includes schools designated Priority and or Comprehensive Support
# and Improvement (CSI).

# The year field refers to the summer(s) in which the school was identified.

priority_2012 <-
  read.xlsx("data/2012-school-designations.xlsx") %>%
  janitor::clean_names() %>%
  filter(status == "Priority") %>%
  mutate(
    district_old = case_when(district_number == 791 ~ 791),
    school_old = case_when(district_number == 791 ~ school_number)
  ) %>%
  transmute(
    district_number = if_else(
      district_number == 791,
      792,
      district_number
    ),
    district_name = district,
    school_number = if_else(
      district_number == 792 & school_number < 8000,
      school_number + 2000,
      school_number
    ),
    school_name = school,
    year = 2012,
    instructional_type = if_else(
      instructional_type_description == "High School",
      "High",
      instructional_type_description
    ),
    title_1 = T,
    priority = T,
    district_old,
    school_old
  ) %>%
  rename(
    district = district_number,
    school = school_number
  )

priority_2013 <-
  priority_2012 %>%
  mutate(year = 2013) %>%
  select(-instructional_type, -title_1)

priority_2014 <-
  read.xlsx("data/2014-priority.xlsx", startRow = 2, cols = 1:4) %>%
  janitor::clean_names() %>%
  filter(!is.na(district_number), !is.na(school_number)) %>%
  mutate_at(vars(district_number, school_number), as.numeric) %>%
  mutate(year = 2014, priority = T) %>%
  rename(
    district_name = district,
    district = district_number,
    school_name = school,
    school = school_number
  )

priority_2015 <-
  priority_2014 %>%
  anti_join(
    read.xlsx("data/2015-priority-exit-public.xlsx", startRow = 2, cols = 1:4) %>%
      janitor::clean_names() %>%
      select(district = system_number, school = school_number),
    by = c("district", "school")
  ) %>%
  mutate(year = 2015)

priority_2016 <-
  priority_2015 %>%
  mutate(year = 2016)

priority_2017 <-
  priority_2016 %>%
  anti_join(
    read_csv("data/2017-priority-exit.csv") %>%
      filter(!is.na(priority_exit), priority_exit == 1) %>%
      select(district = system, school),
    by = c("district", "school")
  ) %>%
  mutate(year = 2017)

priority_2018 <-
  read.xlsx(
    "data/2018-school-designations.xlsx",
    sheet = "Comprehensive Support"
  ) %>%
  # Maury County successfully appealed Northfield Academy's designation because
  # the school was an adult high school.
  filter(system != 600) %>%
  transmute(
    district = system,
    district_name = system_name,
    school,
    school_name,
    year = 2018,
    priority = T
  )

priority_2019 <-
  read.xlsx("data/2019-school-designations.xlsx") %>%
  filter(str_detect(designation, "Comprehensive Support")) %>%
  transmute(
    district = system,
    district_name = system_name,
    school,
    school_name,
    year = 2019,
    priority = T
  )

priority <-
  list(
    priority_2012,
    priority_2013,
    priority_2014,
    priority_2015,
    priority_2016,
    priority_2017,
    priority_2018,
    priority_2019
  ) %>%
  reduce(bind_rows) %>%
  arrange(district, school, year)

# Output ----

write.xlsx(
  list("Priority-CSI" = priority),
  "data/historical-school-designations.xlsx",
  colWidths = "auto",
  overwrite = T
)
