# checks for data collection
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel("inputs/ETH2002_H2R_data.xlsx") |>  
  mutate(#i.check.uuid = `_uuid`,
         # i.check.start_date = as_date(start),
         # i.check.enumerator_id = enumerator_id,
         # i.check.district_name = ifelse(is.na(loc_zone), loc_zone_other, loc_zone),
         # i.check.loc_zone = ifelse(is.na(loc_zone), loc_zone_other, loc_zone),
         # i.check.point_number = "",
         point_number = "",
         start = as_datetime(start),
         end = as_datetime(end))

# tool

loc_tool <- "inputs/ETH2002_H2R_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")


# checks ------------------------------------------------------------------

checks_output <- list()

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_tool_data = df_tool_data, 
                                       input_enumerator_id_col = "enumerator_id",
                                       input_location_col = "loc_zone",
                                       input_point_id_col = "point_number",
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# duplicate uuids ---------------------------------------------------------



# outliers ----------------------------------------------------------------




# other_specify -----------------------------------------------------------

df_others_data <- extract_other_specify_data(input_tool_data = df_tool_data, 
                                             input_point_id_col = "point_number", 
                                             input_location_col = "loc_zone",
                                             input_survey = df_survey,  
                                             input_choices = df_choices)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")

# logical checks ----------------------------------------------------------




# combined  checks --------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_h2r_eth.csv"), na = "")
