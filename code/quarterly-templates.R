# Create one quarterly data collection template per district, pre-populated with
# district and school numbers, school names, and other fields.

library(lubridate)
library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

# Data ----

connection_sde <- connect(to = "sde")

content_areas_rti <-
  tibble(
    content_area = rep(
      c("Reading Fluency", "Reading Comprehension", "Math"),
      each = 11
    ),
    grade_level = rep(c("All Grades", as.character(3:12)), 3)
  )

content_areas_benchmark <-
  tibble(
    content_area = c(
      rep(c("ELA", "Math"), each = 7),
      "English I", "English II", "English III",
      "Algebra I or Int. Math I",
      "Geometry or Int. Math II",
      "Algebra II or Int. Math III"
    ),
    grade_level = c(
      rep(c("All Grades", as.character(3:8)), 2),
      rep("All Grades", 6)
    )
  )

grade_levels <-
  DBI::dbGetQuery(
    connection_sde, "
    select
      sch.district_number,
      sch.school_number,
      sig.grade_id as grade_level,
      gra.grade_description,
      gra.seq_num as grade_sequence,
      sig.sig_begin_date,
      sig.sig_end_date
    from
      school sch
      join school_instructional_grade sig on sch.bu_id = sig.bu_id
      join grades gra on sig.grade_id = gra.grade_id"
  ) %>%
  rename_all(tolower) %>%
  mutate_at(vars(district_number, school_number), as.numeric) %>%
  mutate_at(vars(sig_begin_date, sig_end_date), as_date) %>%
  filter(
    is.na(sig_end_date) | sig_end_date > today(),
    sig_begin_date <= today()
  ) %>%
  arrange(district_number, school_number, grade_sequence)

# first_instructional_dates <-
#   DBI::dbGetQuery(
#     connection_eis, "
#     select sch.district_no, sch.school_no, cal.school_year, cal.id_date
#     from scal_id_days cal join school sch on cal.school_bu_id = sch.school_bu_id
#     where cal.school_year = 2019
#     "
#   ) %>%
#   rename_all(tolower) %>%
#   transmute(
#     system = district_no,
#     school_number = school_no,
#     year = school_year + 1,
#     first_instructional_date = as_date(id_date)
#   ) %>%
#   group_by(system, school_number, year) %>%
#   filter(first_instructional_date == min(first_instructional_date, na.rm = T)) %>%
#   ungroup() %>%
#   arrange(system, school_number, year)

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
  select(system:school_name) %>%
  bind_rows(
    tibble(
      system = 792, system_name = "Shelby County",
      school = 2245, school_name = "Geeter School"
    )
  ) %>%
  arrange(system_name, school_name) %>%
  rename(
    district_number = system,
    district_name = system_name,
    school_number = school
  )

districts_csi <-
  schools_csi %>%
  arrange(district_number) %>%
  distinct(district_number) %>%
  extract2("district_number")

student_groups <-
  tibble(
    student_group = c(
      "All Students",
      "Economically Disadvantaged",
      "English Learners (including T1-T4)",
      "Students with Disabilities",
      "Black or African-American",
      "Hispanic"
    ),
    grade_level = "All Grades"
  )

template_blank <- "C:/Users/CA20397/SharePoint/School Improvement - Documents/Grant Monitoring/quarterly-data-template-2019.xlsx"

# Output: Benchmarks and RTI ----

output_benchmark_rti <-
  map2(
    .x = 1:2, .y = list(content_areas_benchmark, content_areas_rti),
    ~ schools_csi %>%
      mutate(grade_level = "All Grades", grade_sequence = -1) %>%
      bind_rows(
        left_join(
          schools_csi,
          grade_levels,
          by = c("district_number", "school_number")
        )
      ) %>%
      left_join(.y, by = "grade_level") %>%
      left_join(student_groups, by = "grade_level") %>%
      group_by(district_number, school_number) %>%
      filter(!is.na(content_area)) %>%
      filter(
        !content_area %in% c("ELA", "Math")
        | mean(grade_level == "All Grades") != 1
      ) %>%
      filter(
        !str_detect(content_area, "II")
        | mean(grade_level == "All Grades") == 1
      ) %>%
      ungroup() %>%
      mutate_at("student_group", funs(if_else(is.na(.), "All Students", .))) %>%
      select(
        district_number, school_number, school_name,
        content_area, grade_level, student_group
      )
  ) %>%
  map_at(
    1, ~ .x %>%
      arrange(
        district_number, school_name,
        content_area != "ELA", content_area != "Math",
        !str_detect(content_area, "English"),
        !str_detect(content_area, "Algebra I "),
        !str_detect(content_area, "Geometry"),
        grade_level != "All Grades",
        student_group
      )
  ) %>%
  map_at(
    2, ~ .x %>%
      arrange(
        district_number, school_name, desc(content_area),
        grade_level != "All Grades",
        student_group
      )
  )

# Output: Grades ----

output_grades <-
  schools_csi %>%
  left_join(grade_levels, by = c("district_number", "school_number")) %>%
  filter(grade_level %in% c("9", "10")) %>%
  distinct(district_number, school_number, school_name)

# Write Excel templates ----

template_create <- function(data_list, district) {
  data_filtered <- map(data_list, ~ filter(.x, district_number == district))
  file_name <- str_c("data/templates-quarterly/quarterly-dpsig-data-", district, ".xlsx")
  template <- loadWorkbook(template_blank)
  walk2(
    .x = data_filtered, .y = c("Benchmarks", "RTI", "Grades"),
    ~ writeData(template, sheet = .y, x = .x, startRow = 2, colNames = F)
  )
  saveWorkbook(template, file_name, overwrite = T)
  rm(file_name, template)
}

output <- c(output_benchmark_rti, list(output_grades))

walk(.x = districts_csi, ~ template_create(output, .x))
