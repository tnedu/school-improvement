library(DBI)
library(RJDBC)
library(ggpubr)
library(rmarkdown)
library(lubridate)
library(magrittr)
library(openxlsx)
# library(sida)
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

# Parameters ----

current_school_year <- 2020

directory_current <- getwd()

directory_tnshare <- str_c(
  Sys.getenv("tnshare_data_use"), "/",
  "projects-master/school-improvement/2020-dpsig-slig-monitoring/"
)

month_for_ytd_filter <- 12

if(month_for_ytd_filter > 7) {
  if(month_for_ytd_filter < 10) separator <- "-0" else separator <- "-"
  month_folder <- str_c(current_school_year - 1, separator, month_for_ytd_filter)
  rm(separator)
} else {
  month_folder <- str_c(current_school_year, "-0", month_for_ytd_filter)
}

report_month <- str_c("-", month_folder)

# Tidy and write data ----

source("code/monthly-eis-data.R")
file.edit("code/monthly-teacher-data.R")

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
      to = str_c("output/monthly-dpsig-report-", district_arg, report_month, ".pdf"),
      overwrite = T
    )
  }

walk2(
  .x = districts_csi$district, .y = districts_csi$district_name,
  ~ render_reports(.x, .y)
)

# Clean up ----

rm(list = grep("schools_csi|districts_csi|school_ids", ls(), invert = T, value = T))
