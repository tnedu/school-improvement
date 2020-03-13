library(DBI)
library(RJDBC)
library(ggpubr)
library(rmarkdown)
library(haven)
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

# Parameters (manual) ----

# It's unclear if we need the last date of month_for_ytd_filter to filter EIS
# data. month_for_ytd_filter might already suffice.

current_school_year <- 2020

month_for_ytd_filter <- 2

# Parameters ----

directory_current <- getwd()

directory_master <- str_c(
  Sys.getenv("tnshare_data_use"), "/",
  "projects-master/school-improvement/2020-dpsig-slig-monitoring/"
)

if(month_for_ytd_filter > 7) {
  if(month_for_ytd_filter < 10) separator <- "-0" else separator <- "-"
  month_folder <- str_c(current_school_year - 1, separator, month_for_ytd_filter)
  rm(separator)
} else {
  month_folder <- str_c(current_school_year, "-0", month_for_ytd_filter)
}

report_month <- str_c("-", month_folder)

# Tidy and write data ----

file.edit("code/monthly-teacher-data.R")
source("code/monthly-eis-data.R")
source("code/teacher-turnover.R")

# Render reports ----

render_reports <-
  function(
    district_arg,
    district_name_arg,
    data_file_date_arg = today(),
    directory_project = directory_current,
    teacher_file_date_arg = today(),
    teacher_file_month_arg = report_month
  ) {
    render(
      input = "code/monthly-report.Rmd",
      params = list(
        district = district_arg,
        district_name = district_name_arg,
        data_file_date = data_file_date_arg,
        directory_project = directory_project,
        teacher_file_date = teacher_file_date_arg,
        teacher_file_month = teacher_file_month_arg
      )
    )
    if(!dir.exists("output")) dir.create("output")
    file.copy(
      from = "code/monthly-report.pdf",
      to = str_c(
        "output/monthly-dpsig-report-",
        district_arg,
        report_month,
        ".pdf"
      ),
      overwrite = T
    )
  }

walk2(
  .x = districts_csi$district,
  .y = districts_csi$district_name,
  ~ render_reports(
    .x, .y
    # data_file_date_arg = today()
  )
)

render_reports(
  0, "State-Level Users"
  # data_file_date_arg = "2020-02-05", teacher_file_date_arg = "2020-02-04"
)

# Clean up ----

rm(list = grep("schools_csi|districts_csi|school_ids", ls(), invert = T, value = T))
