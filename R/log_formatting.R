# read in log and data

library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# Read data and checking log 

df_cleaning_log <- read_csv("inputs/combined_checks_h2r_eth.csv", col_types = cols(sheet = "c", index = "i")) |> 
  filter(reviewed %in% c("1"))

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
  select(uuid = `_uuid`, `enumerator ID` = enumerator_id)

# log ---------------------------------------------------------------------

df_formatted_log <- df_cleaning_log |> 
  # mutate(int.adjust_log = ifelse(adjust_log %in% c("delete_log"), "no", "yes")) |> 
  mutate(int.adjust_log = ifelse(adjust_log %in% c("delete_log"), "no", "yes"),
         `enumerator ID` = enumerator_id, question.name = name, Issue = str_replace_all(string = issue, pattern = "\\[+.+\\]", replacement = ""), 
         `Type of Issue` = type, feedback = comment, 
         changed = int.adjust_log, old.value = current_value, new.value = value) |> 
  select(uuid, `enumerator ID`, question.name, Issue, `Type of Issue`, 
         feedback, changed, old.value, new.value)
  
# deletion log ------------------------------------------------------------

df_deletion_log <- df_cleaning_log |> 
  filter(!adjust_log %in% c("delete_log"), type %in% c("remove_survey")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  select(uuid, `enumerator ID` = enumerator_id, Issue = issue, `Type of Issue (Select from dropdown list)` = type, 
         feedback = comment)

# enumerator performance --------------------------------------------------
# Number of surveys collected by enumerators
df_surveys_by_enum <- df_raw_data |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())
# Number of changes by enumerators
df_changes_by_enum <- df_cleaning_log |> 
  filter(!adjust_log %in% c("delete_log")) |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())
# Number of changes by enumerators filtered by issues
df_changes_by_enum_issue <- df_cleaning_log |> 
  filter(!adjust_log %in% c("delete_log")) |> 
  group_by(enumerator_id, issue_id) |> 
  summarise(Number = n())
# Number of deletions by enumerators
df_deletion_by_enum <- df_cleaning_log |> 
  filter(!adjust_log %in% c("delete_log"), type %in% c("remove_survey")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())
# Number of deletions due to time by enumerator
df_deletion_by_enum_time <- df_cleaning_log |> 
  filter(!adjust_log %in% c("delete_log"), type %in% c("remove_survey")) |> 
  filter(issue_id %in% c("less_survey_time")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())


# export sheets -----------------------------------------------------------


openxlsx::write.xlsx(x = list(Summary = df_variable_summary,
                             data_extract = df_data_extract,
                             # Log_book = df_formatted_log,
                             deletion_log = df_deletion_log,
                             surveys_by_enum = df_surveys_by_enum,
                             changes_by_enum = df_changes_by_enum,
                             changes_by_issue = df_changes_by_enum_issue,
                             del_by_enum = df_deletion_by_enum,
                             del_by_enum_time = df_deletion_by_enum_time), 
                    file = paste0("outputs/", butteR::date_file_prefix(), "_eth_h2r_data_cleaning_logbook.xlsx"))

# exporting log alone
write_csv(df_formatted_log, file = "outputs/Log_book.csv", na = "")
