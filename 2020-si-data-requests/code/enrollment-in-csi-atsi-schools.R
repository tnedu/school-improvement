# Enrollment in CSI or ATSI Schools
# Josh Carson
# Last updated 2020-01-16

# Calculate district-level counts of students currently enrolled in CSI and
# ATSI schools, respectively.

library(lubridate)
library(magrittr)
library(openxlsx)
library(tidyverse)

connection_eis <-
  DBI::dbConnect(
    RJDBC::JDBC(
      "oracle.jdbc.OracleDriver",
      classPath = Sys.getenv("jar_path")
    ),
    Sys.getenv("eis_connection_string"),
    "EIS_MGR", Sys.getenv("eis_password")
  )

# Input ----

enrollment <-
  DBI::dbGetQuery(
    connection_eis,
    read_file("code/enrollment-in-csi-atsi-schools.sql")
  ) %>%
  rename_all(tolower) %>%
  mutate_at(vars(begin_date, end_date), as_date)

schools <-
  read.xlsx("N:/ORP_accountability/data/2019_final_accountability_files/school_designations_file.xlsx") %>%
  filter(
    designation %in% c(
      "Priority & Comprehensive Support",
      "Comprehensive Support",
      "Priority Exit & Comprehensive Support",
      "Additional Targeted Support and Improvement"
    )
  )

# Output ----

output <-
  schools %>%
  left_join(
    enrollment %>%
      filter(
        begin_date <= today()
        & (is.na(end_date) | end_date > today())
      ),
    by = c("system", "school")
  ) %>%
  mutate_at(
    vars(designation),
    funs(if_else(str_detect(., "Additional"), "ATSI", "Priority or CSI"))
  ) %>%
  group_by(system, system_name, designation) %>%
  summarize(n_students_enrolled = n_distinct(student_key)) %>%
  ungroup() %>%
  spread(designation, n_students_enrolled) %>%
  arrange(system_name) %>%
  rename(District = system, `District Name` = system_name)

write.xlsx(
  list(enrollment = output),
  "data/enrollment-in-csi-atsi-schools.xlsx",
  colWidths = "auto",
  overwrite = T
)
