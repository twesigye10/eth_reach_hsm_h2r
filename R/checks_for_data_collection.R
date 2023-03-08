# checks for data collection
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel("inputs/ETH2002_H2R_data.xlsx") |>  
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = ifelse(is.na(loc_zone), loc_zone_other, loc_zone),
         i.check.loc_zone = ifelse(is.na(loc_zone), loc_zone_other, loc_zone),
         i.check.point_number = "",
         point_number = "",
         start = as_datetime(start),
         end = as_datetime(end))

# tool

loc_tool <- "inputs/ETH2002_H2R_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")


# checks ------------------------------------------------------------------

checks_output <- list()

# check duplicate uuids

# outliers


# other_specify
df_others_data <- extract_other_specify_data(input_tool_data = df_tool_data, 
                                             input_point_id_col = "point_number", 
                                             input_location_col = "loc_zone",
                                             input_survey = df_survey,  
                                             input_choices = df_choices)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")


# logical checks