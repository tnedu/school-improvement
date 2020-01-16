designations_2019 <- read.xlsx("N:/ORP_accountability/data/2019_final_accountability_files/school_designations_file.xlsx")

designations_2018 <-
  read.xlsx(
    "N:/ORP_accountability/data/2018_final_accountability_files/school_designations_file.xlsx",
    sheet = "Designations"
  )

designations_compare <-
  designations_2019 %>%
  full_join(designations_2018, by = c("system", "school")) %>%
  filter(str_detect(designation.x, "Comp") | str_detect(designation.y, "Comp")) %>%
  filter(is.na(designation.x) | is.na(designation.y))
