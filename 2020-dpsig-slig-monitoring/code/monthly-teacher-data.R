# Store variable names from the template ----

variable_names <-
  read.xlsx(
    "C:/Users/CA20397/SharePoint/School Improvement - Documents/Grant Monitoring/monthly-data-template-2019.xlsx",
    sheet = "Teachers"
  ) %>%
  janitor::clean_names() %>%
  names()

# List file names for the most recent month ----

# The calls to map_at() below will fail if the list is not ordered by district
# number.

(teacher_last_month_paths <-
  list.files(
    path = str_c("data/from-districts-", month_folder),
    pattern = "monthly-dpsig-data",
    full.names = T
  ))

# Import August teacher data ----

if(month_folder == "08") {
  teacher_last_month <-
    teacher_last_month %>%
    map(
      ~ .x %>%
        read.xlsx(sheet = "Teachers") %>%
        janitor::clean_names() %>%
        mutate_at(vars(first_instructional_date, current_date), convertToDate)
    ) %>%
    map_at(
      2, ~ .x %>%
        mutate(district_number = 180) %>%
        select(district_number, everything())
    ) %>%
    map_at(
      3, ~ .x %>%
        mutate_at(
          "number_of_teacher_observations",
          funs(case_when(str_detect(., "50 WT") ~ 50,
                         str_detect(., "7 WT") ~ 7,
                         str_detect(., "10-15") ~ 10,
                         T ~ as.numeric(.)))
        )
    ) %>%
    map_at(
      8, ~ .x %>%
        mutate_at(vars(starts_with("num")), as.numeric) %>%
        mutate_at(
          "school_number",
          funs(if_else(district_number == 792 & . == 2240, 2245, .))
        )
    ) %>%
    map_at(9, ~ fill(.x, district_number, school_number)) %>%
    map(
      ~ .x %>%
        select(-school_name) %>%
        rename(district = district_number, school = school_number) %>%
        left_join(schools_csi %>% select(-bu_id), by = c("district", "school")) %>%
        select(district, district_name, school, school_name, everything())
    ) %>%
    reduce(bind_rows) %>%
    arrange(district_name, school_name)
}

# Import September teacher data ----

if(month_folder == "09") {
  
  # Check sheet names.
  sheet_names <- teacher_last_month %>% map(getSheetNames)
  
  # Select sheets to import.
  sheet_names <-
    sheet_names %>%
    map(~ .x[str_detect(.x, "Teachers|September")]) %>%
    map_at(7, ~ .x[[2]]) %>%
    unlist()
  
  # Read data.
  
  teacher_last_month <-
    teacher_last_month %>%
    map2(
      sheet_names,
      ~ .x %>%
        read.xlsx(sheet = .y) %>%
        janitor::clean_names() %>%
        mutate_at(vars(first_instructional_date, current_date), convertToDate)
    )
  
  # Check variable names.
  
  map(teacher_last_month, ~ all(names(.x) == variable_names))
  map(teacher_last_month[2:3], names)
  
  # Check inconsistent variable types.
  
  map(teacher_last_month, ~ class(.x$number_of_teacher_observations))
  map(teacher_last_month, ~ class(.x$number_of_teachers_chronically_absent))
  map(teacher_last_month, ~ class(.x$number_of_teachers_start_of_year))
  map(as.list(teacher_last_month[[8]]), class)
  
  teacher_last_month %>%
    keep(~ is.character(.x$number_of_teacher_observations)) %>%
    map(~ sort(unique(.x$number_of_teacher_observations)))
  
  teacher_last_month %>%
    keep(~ is.character(.x$number_of_teachers_chronically_absent)) %>%
    map(~ sort(unique(.x$number_of_teachers_chronically_absent)))
  
  sort(unique(teacher_last_month[[8]]$number_of_teachers_start_of_year))

  # Check district and school numbers.
  
  teacher_last_month %>%
    keep(~ any(str_detect(names(.x), "district_number"))) %>%
    map(~ summarize_at(.x, vars(district_number), funs(mean(is.na(.)))))
  
  map(teacher_last_month, ~ summarize_at(.x, vars(school_number), funs(mean(is.na(.)))))
  
  # Fix issues.
  
  teacher_last_month <-
    teacher_last_month %>%
    map_at(
      2, ~ .x %>%
        mutate(district_number = 180) %>%
        select(district_number, everything())
    ) %>%
    map_at(
      3, ~ .x %>%
        mutate_at(
          "number_of_teacher_observations",
          funs(
            str_extract_all(., "\\d+", simplify = T) %>%
              apply(1:2, as.numeric) %>%
              rowSums(na.rm = T)
          )
        ) %>%
        select(-x, -x14)
    ) %>%
    map_at(
      6, ~ .x %>%
        mutate_at(
          "number_of_teacher_observations",
          funs(as.numeric(str_sub(., start = 1, end = 1)))
        ) %>%
        mutate_at(
          "number_of_teachers_chronically_absent",
          funs(if_else(!is.na(.), 3, NA_real_))
        )
    ) %>%
    map_at(
      8, ~ .x %>%
        mutate_at(vars(starts_with("num")), as.numeric) %>%
        mutate_at(
          "school_number",
          funs(if_else(district_number == 792 & . == 2240, 2245, .))
        )
    ) %>%
    map_at(9, ~ fill(.x, district_number, school_number)) %>%
    map(
      ~ .x %>%
        select(-school_name) %>%
        rename(district = district_number, school = school_number) %>%
        left_join(schools_csi %>% select(-bu_id), by = c("district", "school")) %>%
        select(district, district_name, school, school_name, everything())
    ) %>%
    reduce(bind_rows) %>%
    arrange(district_name, school_name)
  
}

# Import October teacher data ----

if(month_folder == "10") {
  
  # Check sheet names.
  sheet_names <- teacher_last_month %>% map(getSheetNames)
  
  # Select sheets to import.
  sheet_names <-
    sheet_names %>%
    map(~ .x[str_detect(.x, "Teachers|October")]) %>%
    # map_at(7, ~ .x[[2]]) %>%
    unlist()
  
  # Read data.
  
  teacher_last_month <-
    teacher_last_month %>%
    map2(
      sheet_names,
      ~ .x %>%
        read.xlsx(sheet = .y) %>%
        janitor::clean_names() %>%
        mutate_at(vars(first_instructional_date, current_date), convertToDate)
    )
  
  # Check variable names.
  
  map(teacher_last_month, ~ all(names(.x) == variable_names))
  # map(teacher_last_month[2:3], names)
  all(c("district_number", names(teacher_last_month[[2]])) == variable_names)
  
  # Check inconsistent variable types.
  
  map(teacher_last_month, ~ class(.x$number_of_teacher_observations))
  
  teacher_last_month %>%
    keep(~ is.character(.x$number_of_teacher_observations)) %>%
    map(~ sort(unique(.x$number_of_teacher_observations)))
  
  map(teacher_last_month, ~ class(.x$number_of_teachers_chronically_absent))

  # teacher_last_month %>%
  #   keep(~ is.character(.x$number_of_teachers_chronically_absent)) %>%
  #   map(~ sort(unique(.x$number_of_teachers_chronically_absent)))
  
  map(teacher_last_month, ~ class(.x$number_of_teachers_start_of_year))
  # map(as.list(teacher_last_month[[8]]), class)
  # sort(unique(teacher_last_month[[8]]$number_of_teachers_start_of_year))
  
  # Check district and school numbers.
  
  teacher_last_month %>%
    keep(~ any(str_detect(names(.x), "district_number"))) %>%
    map(~ summarize_at(.x, vars(district_number), funs(mean(is.na(.)))))
  
  map(
    teacher_last_month,
    ~ summarize_at(.x, vars(school_number), funs(mean(is.na(.))))
  )
  
  teacher_last_month %>%
    keep(~ max(.x[["district_number"]]) == 792) %>%
    pluck(1) %>%
    filter(school_number %in% c(2240, 2245)) %>%
    select(school_number)
  
  # Fix issues.
  
  teacher_last_month <-
    teacher_last_month %>%
    map_at(
      2, ~ .x %>%
        mutate(district_number = 180) %>%
        select(district_number, everything())
    ) %>%
    # map_at(
    #   3, ~ .x %>%
    #     mutate_at(
    #       "number_of_teacher_observations",
    #       funs(
    #         str_extract_all(., "\\d+", simplify = T) %>%
    #           apply(1:2, as.numeric) %>%
    #           rowSums(na.rm = T)
    #       )
    #     ) %>%
    #     select(-x, -x14)
    # ) %>%
    # map_at(
    #   6, ~ .x %>%
    #     mutate_at(
    #       "number_of_teacher_observations",
    #       funs(as.numeric(str_sub(., start = 1, end = 1)))
    #     ) %>%
    #     mutate_at(
    #       "number_of_teachers_chronically_absent",
    #       funs(if_else(!is.na(.), 3, NA_real_))
    #     )
    # ) %>%
    map_at(
      5, ~ .x %>% # 8
        mutate_at(vars(starts_with("num")), as.numeric)
        # mutate_at(
        #   "school_number",
        #   funs(if_else(district_number == 792 & . == 2240, 2245, .))
        # )
    ) %>%
    # map_at(9, ~ fill(.x, district_number, school_number)) %>%
    map(
      ~ .x %>%
        mutate_at(vars(district_number, school_number), as.numeric) %>%
        select(-school_name) %>%
        rename(district = district_number, school = school_number) %>%
        inner_join(schools_csi %>% select(-bu_id), by = c("district", "school")) %>%
        select(district, district_name, school, school_name, everything())
    ) %>%
    reduce(bind_rows) %>%
    arrange(district_name, school_name)
  
}

# Import November teacher data ----

if(str_detect(month_folder, "-11$")) {
  
  # Check sheet names.
  (sheet_names <- teacher_last_month %>% map(getSheetNames))
  
  # Select sheets to import.
  (sheet_names <-
    sheet_names %>%
    map(~ .x[str_detect(.x, "Teachers|November|Sheet1")]) %>%
    unlist())
  
  # Read data.
  
  teacher_last_month_import <-
    teacher_last_month_paths %>%
    map2(
      sheet_names,
      ~ .x %>%
        read.xlsx(sheet = .y) %>%
        janitor::clean_names() %>%
        mutate_at(vars(first_instructional_date, current_date), convertToDate)
    )
  
  # Check variable names.
  
  map(teacher_last_month_import, ~ all(names(.x) == variable_names))
  all(c("district_number", names(teacher_last_month_import[[2]])) == variable_names)
  
  # Check inconsistent variable types.
  
  map(teacher_last_month_import, ~ class(.x$number_of_teachers_start_of_year))
  
  map(teacher_last_month_import, ~ class(.x$number_of_teachers_persisted_this_year))
  unique(teacher_last_month_import[[3]]$number_of_teachers_persisted_this_year)
  
  map(teacher_last_month_import, ~ class(.x$number_of_teachers_gained_this_year))
  map(teacher_last_month_import, ~ class(.x$number_of_unendorsed_teachers))
  map(teacher_last_month_import, ~ class(.x$number_of_long_term_substitutes))
  map(teacher_last_month_import, ~ class(.x$number_of_teachers_chronically_absent))
  
  map(teacher_last_month_import, ~ class(.x$number_of_teacher_observations))
  
  teacher_last_month_import %>%
    keep(~ is.character(.x$number_of_teacher_observations)) %>%
    map(~ sort(unique(.x$number_of_teacher_observations)))
  
  # Check district and school numbers.
  # Geeter School in Shelby County should have school number 2245.
  
  teacher_last_month_import %>%
    keep(~ any(str_detect(names(.x), "district_number"))) %>%
    map(~ summarize_at(.x, vars(district_number), funs(mean(is.na(.)))))
  
  map(
    teacher_last_month_import,
    ~ summarize_at(.x, vars(school_number), funs(mean(is.na(.))))
  )
  
  teacher_last_month_import %>%
    keep(~ max(.x[["district_number"]]) == 792) %>%
    pluck(1) %>%
    filter(school_number %in% c(2240, 2245)) %>%
    select(school_number)
  
  # Fix issues.
  
  teacher_last_month <-
    teacher_last_month_import %>%
    map_at(
      2, ~ .x %>%
        mutate(district_number = 180) %>%
        select(district_number, everything())
    ) %>%
    map_at(
      3, ~ .x %>%
        mutate_at("number_of_teachers_persisted_this_year", as.numeric) %>%
        mutate_at(
          "number_of_teacher_observations",
          funs(
            str_extract_all(., "\\d+", simplify = T) %>%
              apply(1:2, as.numeric) %>%
              rowSums(na.rm = T)
          )
        )
    ) %>%
    map_at(
      6, ~ .x %>%
        mutate(number_of_teacher_observations = NA_real_)
    ) %>%
    map_at(
      8, ~ .x %>%
        mutate_at(vars(starts_with("num")), as.numeric)
    ) %>%
    map(
      ~ .x %>%
        mutate_at(vars(district_number, school_number), as.numeric) %>%
        select(-school_name) %>%
        rename(district = district_number, school = school_number) %>%
        inner_join(schools_csi %>% select(-bu_id), by = c("district", "school")) %>%
        select(district, district_name, school, school_name, everything())
    ) %>%
    reduce(bind_rows) %>%
    arrange(district_name, school_name)
  
}

# Check teacher data ----

map(as.list(teacher_last_month), ~ mean(is.na(.x)))
map(.x = keep(as.list(teacher_last_month), is.numeric), .f = summary)

schools_csi %>%
  group_by(district, district_name) %>%
  summarize(n_schools_csi = n_distinct(school)) %>%
  ungroup() %>%
  left_join(
    teacher_last_month %>%
      group_by(district) %>%
      summarize(n_schools_with_data = n_distinct(school)) %>%
      ungroup(),
    by = "district"
  )

# Summarize (school, YTD) ----

teacher_school <-
  map(
    1:3,
    ~ teacher_last_month %>%
      mutate_at(vars(content_area), funs(str_replace(tolower(.), " ", "_"))) %>%
      filter(content_area %in% c("ela", "math", "science", "social_studies")) %>%
      transmute(
        district, school, content_area,
        n_exited = pmax(number_of_teachers_start_of_year - number_of_teachers_persisted_this_year, 0),
        pct_exited = round(100 * n_exited / number_of_teachers_start_of_year, 1)
      )
  ) %>%
  map_at(
    1, ~ .x %>%
      select(-pct_exited) %>%
      spread(content_area, n_exited)
  ) %>%
  map_at(
    2, ~ .x %>%
      select(-n_exited) %>%
      spread(content_area, pct_exited)
  ) %>%
  map_at(
    3, ~ .x %>%
      group_by(content_area) %>%
      mutate(school_rank_exited = min_rank(pct_exited)) %>%
      ungroup() %>%
      select(district, school, content_area, school_rank_exited) %>%
      spread(content_area, school_rank_exited)
  ) %>%
  map2(
    .y = c("n_exited", "pct_exited", "school_rank_exited"),
    ~ .x %>%
      rename_at(
        vars(ela:social_studies),
        funs(str_c(.y, ., sep = "_"))
      )
  ) %>%
  reduce(left_join, by = c("district", "school"))

# Write clean data for reports ----

walk2(
  .x = list(teacher_school),
  .y = list("teacher-school-"),
  ~ write_csv(.x, path = str_c("data/", .y, today(), ".csv"))
)

# Write raw data for DSI ----

# Last month's data
template <- loadWorkbook("data/templates-monthly/monthly-data-template-2019-with-district-name.xlsx")
filename <- str_c("monthly-dpsig-data-", month_folder, ".xlsx")
writeData(template, "Teachers", teacher_last_month, startRow = 2, colNames = F)
saveWorkbook(template, str_c("data/", filename), overwrite = T)

# All months' data
teacher_all_months <-
  list.files(
    path = "data",
    pattern = "monthly-dpsig-data-2019",
    full.names = T
  ) %>%
  map(
    ~ .x %>%
      read.xlsx(sheet = "Teachers") %>%
      janitor::clean_names() %>%
      mutate_at(vars(ends_with("date")), convertToDate)
  ) %>%
  reduce(bind_rows) %>%
  arrange(district_name, school_name)

template <- loadWorkbook("data/templates-monthly/monthly-data-template-2019-with-district-name.xlsx")
writeData(template, "Teachers", teacher_all_months, startRow = 2, colNames = F)
saveWorkbook(template, "data/monthly-dpsig-data-2019.xlsx", overwrite = T)

file.copy(
  from = str_c("data/", filename),
  to = str_c(
    "C:/Users/CA20397/SharePoint/School Improvement - Documents/Grant Monitoring/monthly-dpsig-data/",
    filename
  ),
  overwrite = T
)