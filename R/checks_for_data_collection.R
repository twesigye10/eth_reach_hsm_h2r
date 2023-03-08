# checks for data collection
library(tidyverse)
library(lubridate)
library(glue)

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel("inputs/ETH2002_H2R_data.xlsx") |>  
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = ifelse(is.na(loc_zone), loc_zone_other, loc_zone),
         i.check.point_number = "",
         start = as_datetime(start),
         end = as_datetime(end))

# tool

loc_tool <- "inputs/ETH2002_H2R_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")
