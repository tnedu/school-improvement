library(DBI)
library(RJDBC)
library(ggpubr)
library(rmarkdown)
library(lubridate)
library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

connection_eis <- connect()

# Set parameters ----

month_folder <- "09"

month_for_ytd_filter <- 9

report_month <- "-2019-09"

# Tidy and write data ----

# source("code/schools.R")
source("code/monthly-eis-data.R")

# Do not source this file. Step through it, and resolve issues as they arise.
# source("code/monthly-teacher-data.R")

# Render reports ----

render_reports <-
  function(
    district_arg,
    district_name_arg,
    data_file_date_arg = today()
  ) {
    render(
      input = "code/monthly-report.Rmd",
      # output_file = str_c("monthly-dpsig-report-", .x, report_month, ".pdf"),
      # output_dir = "output",
      params = list(
        district = district_arg,
        district_name = district_name_arg,
        data_file_date = data_file_date_arg
      )
    )
    file.copy(
      from = "code/monthly-report.pdf",
      to = str_c("output/monthly-dpsig-report-", district_arg, report_month, ".pdf"),
      overwrite = T
    )
  }

walk2(
  .x = districts_csi$district, .y = districts_csi$district_name,
  # .x = 985, .y = "Achievement School District",
  ~ render_reports(.x, .y)
)

# Clean up ----

rm(list = grep("schools_csi|districts_csi|school_ids", ls(), invert = T, value = T))
