# read in log and data

library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# Read data and checking log 

df_cleaning_log <- read_csv("inputs/combined_checks_h2r_eth.csv", col_types = cols(sheet = "c", index = "i"))

# raw data
loc_data <- "inputs/ETH2002_H2R_data.xlsx"

cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time",
                    "date_last_in_settlement", "date_arrived_current_location", "crops_destroyed_by_conflict",
                    "when_schools_last_opened")

data_nms <- names(readxl::read_excel(path = loc_data, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = loc_data, col_types = c_types) |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# tool
loc_tool <- "inputs/ETH2002_H2R_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
  mutate(label = `label::english`)


# create variable summary -------------------------------------------------
# also need to add composite indicators
# need to determine new choices added and how many entries were affected
df_variable_summary <- df_survey |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  mutate(variable = name, action = "checked", description = "", observations_affected = "") |> 
  select(variable, action, description, observations_affected)

# extract data ------------------------------------------------------------
df_data_extract <- df_raw_data |> 
  select(uuid = `_uuid`, enumerator_id)

# log ---------------------------------------------------------------------

df_formatted_log <- df_cleaning_log |> 
  mutate(adjust_log = ifelse(adjust_log %in% c("delete_log"), "no", "yes")) |>  
  select(uuid, enumerator_id, question.name = name, issue, type_of_issue = type, 
         feedback = comment, changed = adjust_log, old.value = current_value, new.value = value)
  
# deletion log ------------------------------------------------------------

df_deletion_log <- df_cleaning_log |> 
  filter(type %in% c("remove_survey")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup()

# enumerator performance --------------------------------------------------


