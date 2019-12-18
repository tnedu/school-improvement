# Chronic Absenteeism and Discipline Report
# Josh Carson
# Last Updated: 2019-03-14

# Next Steps
# 1) Plan the markdown for HPN.

# Need to add IPN into a lot of these group by commands

library(DBI)
library(RJDBC)
library(magrittr)
library(openxlsx)
library(sida)
library(lubridate)
library(tidyverse)

con_eis <- connect()

yrs <- 2019:2020
dst <- NULL
asd <- F
hpn <- F

scripts <-
  c(
    "01_functions",
    "02_schools",
    "03_calendar"
    # "04_demographics",
    # "05_absence" # doesn't work for ASD
    # "06_discipline"
    # "07_save"
  )

psource(scripts)

rmarkdown::render(
  "reports/markdown_hpn.Rmd",
  params = list(system = 330, school = 55, school_name = "Dalewood Middle School")
  # output_file = str_c("ca_discipline_dalewood_", format(now(), "%Y%m%d"), ".pdf"),
  # output_dir = str_c(getwd(), "/reports")
)

file.rename(
  from = "reports/markdown_hpn.pdf",
  to = str_c("reports/CA Discipline Dalewood ", format(now(), "%Y-%m-%d"), ".pdf")
)

# Notes and Questions

# These scripts currently only work for one district at a time.

# Add homeless, migrant, runaway, and foster care indicators back into
# demographics.

# Do districts and schools define discipline actions consistently? For
# instance, an ISS or OSS might replace instructional time in some places
# but not others.

# Do staff across the state follow the same business rules to record
# begin and end dates of disciplinary actions? For instance, does the end
# date typically refer to the last day of discipline or the first day
# after the discipline ends?

