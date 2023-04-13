# checks for data collection
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel("inputs/ETH2002_H2R_data.xlsx") |>  
  mutate(point_number = "",
         start = as_datetime(start),
         end = as_datetime(end)) |> 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "loc_zone")

# tool

loc_tool <- "inputs/ETH2002_H2R_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# logical checks
df_logical_check_description <-  readxl::read_excel("support_files/V5_Logical checks template_ETH2002.xlsx") |> 
  janitor::clean_names() |> 
  filter(!is.na(check_number)) |>
  select(check_number, check_description) |> 
  mutate(check_number = as.character(check_number))

# checks ------------------------------------------------------------------

checks_output <- list()


# testing data ------------------------------------------------------------

df_testing_data <- df_tool_data |> 
  filter(i.check.start_date < as_date("2023-03-08")) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 15
max_time_of_survey <- 120

df_c_survey_time <-  supporteR::check_survey_time(input_tool_data = df_tool_data, 
                                       input_enumerator_id_col = "enumerator_id",
                                       input_location_col = "loc_zone",
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  supporteR::checks_duplicate_uuids(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_duplicate_uuid")

# outliers ----------------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data,
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "loc_zone")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_outliers")

# other_specify -----------------------------------------------------------

df_others_data <- supporteR::extract_other_specify_data(input_tool_data = df_tool_data, 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "loc_zone",
                                                        input_survey = df_survey,  
                                                        input_choices = df_choices)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")

# logical checks ----------------------------------------------------------
# logic_c_enough_water_but_schedule_change_1
df_logic_c_enough_water_but_schedule_change_1 <- df_tool_data |> 
  filter(freq_not_enough_water_for_all_household_needs %in% c("never"),
         freq_change_schedules_due_to_problems_with_your_water_situation %in% c("rarely", "sometimes", "often", "always")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "freq_not_enough_water_for_all_household_needs",
         i.check.current_value = freq_not_enough_water_for_all_household_needs,
         i.check.value = "freq_change_schedules_due_to_problems_with_your_water_situation",
         i.check.issue_id = "logic_c_enough_water_but_schedule_change_1",
         i.check.issue = glue("freq_not_enough_water_for_all_household_needs: {freq_not_enough_water_for_all_household_needs}, but freq_change_schedules_due_to_problems_with_your_water_situation: {freq_change_schedules_due_to_problems_with_your_water_situation}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_enough_water_but_schedule_change_1")

# logic_c_displacement_but_no_freq_2
df_logic_c_displacement_but_no_freq_2 <- df_tool_data |> 
  filter(any_displacement %in% c("yes"), freq_displacements == 0) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "any_displacement",
         i.check.current_value = any_displacement,
         i.check.value = "no",
         i.check.issue_id = "logic_c_displacement_but_no_freq_2",
         i.check.issue = glue("any_displacement: {any_displacement}, but freq_displacements: {freq_displacements}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_displacement_but_no_freq_2")

# logic_c_no_population_remained_3
df_logic_c_no_population_remained_3 <- df_tool_data |> 
  filter(proportion_of_original_population_remained %in%  c('none')) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "proportion_of_original_population_remained",
         i.check.current_value = proportion_of_original_population_remained,
         i.check.value = "",
         i.check.issue_id = "logic_c_no_population_remained_3",
         i.check.issue = glue("proportion_of_original_population_remained: {proportion_of_original_population_remained}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "delete_log",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_no_population_remained_3")

# logic_c_displaced_and_cannot_leave_settlement_4
df_logic_c_displaced_and_cannot_leave_settlement_4 <- df_tool_data |> 
  filter(proportion_of_original_population_remained == proportion_of_the_population_unable_to_move) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "proportion_of_original_population_remained",
         i.check.current_value = proportion_of_original_population_remained,
         i.check.value = "",
         i.check.issue_id = "logic_c_displaced_and_cannot_leave_settlement_4",
         i.check.issue = glue("proportion_of_original_population_remained: {proportion_of_original_population_remained} and proportion_of_the_population_unable_to_move: {proportion_of_the_population_unable_to_move}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "delete_log",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_displaced_and_cannot_leave_settlement_4")

# logic_c_all_people_have_left_but_some_cannot_leave_5a
df_logic_c_all_people_have_left_but_some_cannot_leave_5a <- df_tool_data |> 
  filter(str_detect(string = displacement_population_groups, pattern = "all_people_have_left_the_settlement"),
         !str_detect(string = which_people_primarily_cannot_leave, pattern = "all_people_have_left_the_settlement")) |> 
  mutate(i.check.type = "remove_option",
         i.check.name = "displacement_population_groups",
         i.check.current_value = displacement_population_groups,
         i.check.value = "",
         i.check.issue_id = "logic_c_all_people_have_left_but_some_cannot_leave_5a",
         i.check.issue = glue("displacement_population_groups: {displacement_population_groups} and which_people_primarily_cannot_leave: {which_people_primarily_cannot_leave}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_all_people_have_left_but_some_cannot_leave_5a")

# logic_c_all_people_have_left_but_some_will_move_5b
df_logic_c_all_people_have_left_but_some_will_move_5b <- df_tool_data |> 
  filter(str_detect(string = displacement_population_groups, pattern = "all_people_have_left_the_settlement"),
         !str_detect(string = which_people_will_move, pattern = "all_people_have_left_the_settlement")) |> 
  mutate(i.check.type = "remove_option",
         i.check.name = "displacement_population_groups",
         i.check.current_value = displacement_population_groups,
         i.check.value = "",
         i.check.issue_id = "logic_c_all_people_have_left_but_some_will_move_5b",
         i.check.issue = glue("displacement_population_groups: {displacement_population_groups} and which_people_will_move: {which_people_will_move}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_all_people_have_left_but_some_will_move_5b")

# logic_c_dk_loc_but_loc_reason_6
df_logic_c_dk_loc_but_loc_reason_6 <- df_tool_data |> 
  filter(if_all(c(destination_region, destination_zone, destination_woreda, destination_kebele), ~ .x == "dk"), 
         !main_reason_choosing_destination %in% c("dk", "dwa")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "main_reason_choosing_destination",
         i.check.current_value = main_reason_choosing_destination,
         i.check.value = "",
         i.check.issue_id = "logic_c_dk_loc_but_loc_reason_6",
         i.check.issue = glue("main_reason_choosing_destination: {main_reason_choosing_destination} and destination_region: {destination_region},  destination_zone: {destination_zone}, destination_woreda: {destination_woreda}, destination_kebele: {destination_kebele}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_dk_loc_but_loc_reason_6")

# logic_c_dk_people_cannot_leave_but_reasons_7
df_logic_c_dk_people_cannot_leave_but_reasons_7 <- df_tool_data |> 
  filter(str_detect(string = which_people_primarily_cannot_leave, pattern = "dk"),
         !str_detect(string = primary_reasons_people_cannot_leave, pattern = "dk")) |> 
  mutate(i.check.type = "remove_option",
         i.check.name = "which_people_primarily_cannot_leave",
         i.check.current_value = which_people_primarily_cannot_leave,
         i.check.value = "",
         i.check.issue_id = "logic_c_dk_people_cannot_leave_but_reasons_7",
         i.check.issue = glue("which_people_primarily_cannot_leave: {which_people_primarily_cannot_leave} but primary_reasons_people_cannot_leave: {primary_reasons_people_cannot_leave}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_dk_people_cannot_leave_but_reasons_7")

# logic_c_dk_people_will_leave_but_reasons_8
df_logic_c_dk_people_will_leave_but_reasons_8 <- df_tool_data |> 
  filter(str_detect(string = which_people_will_move, pattern = "dk"),
         !str_detect(string = main_reasons_leaving_during_this_period, pattern = "dk")) |> 
  mutate(i.check.type = "remove_option",
         i.check.name = "which_people_will_move",
         i.check.current_value = which_people_will_move,
         i.check.value = "",
         i.check.issue_id = "logic_c_dk_people_will_leave_but_reasons_8",
         i.check.issue = glue("which_people_will_move: {which_people_will_move} but main_reasons_leaving_during_this_period: {main_reasons_leaving_during_this_period}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_dk_people_will_leave_but_reasons_8")

# logic_c_dk_people_will_return_but_reasons_9
df_logic_c_dk_people_will_return_but_reasons_9 <- df_tool_data |> 
  filter(str_detect(string = which_people_will_be_returning, pattern = "dk"),
         !str_detect(string = main_reasons_for_return, pattern = "dk")) |> 
  mutate(i.check.type = "remove_option",
         i.check.name = "which_people_will_be_returning",
         i.check.current_value = which_people_will_be_returning,
         i.check.value = "",
         i.check.issue_id = "logic_c_dk_people_will_return_but_reasons_9",
         i.check.issue = glue("which_people_will_be_returning: {which_people_will_be_returning} but main_reasons_for_return: {main_reasons_for_return}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_dk_people_will_return_but_reasons_9")

# logic_c_insufficient_food_but_no_coping_10
df_logic_c_insufficient_food_but_no_coping_10 <- df_tool_data |> 
  filter(before_you_left_people_access_sufficient_food %in%  c('no'),
         str_detect(string = not_enough_food_or_money_to_buy_food, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "before_you_left_people_access_sufficient_food",
         i.check.current_value = before_you_left_people_access_sufficient_food,
         i.check.value = "yes",
         i.check.issue_id = "logic_c_insufficient_food_but_no_coping_10",
         i.check.issue = glue("before_you_left_people_access_sufficient_food: {before_you_left_people_access_sufficient_food} but not_enough_food_or_money_to_buy_food: {not_enough_food_or_money_to_buy_food}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_insufficient_food_but_no_coping_10")

# logic_c_no_income_sources_but_access_livelihood_11
df_logic_c_no_income_sources_but_access_livelihood_11 <- df_tool_data |> 
  filter(str_detect(string = top_three_sources_of_income, pattern = "none"),
         people_had_access_to_livelihood_sources %in%  c("yes")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_had_access_to_livelihood_sources",
         i.check.current_value = people_had_access_to_livelihood_sources,
         i.check.value = "no",
         i.check.issue_id = "logic_c_no_income_sources_but_access_livelihood_11",
         i.check.issue = glue("people_had_access_to_livelihood_sources: {people_had_access_to_livelihood_sources} but top_three_sources_of_income: {top_three_sources_of_income}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_no_income_sources_but_access_livelihood_11")

# logic_c_harvest_more_than_4_months_in_sequence
# df_logic_c_harvest_more_than_4_months_in_sequence_12 <- df_tool_data |> 
#   filter() |> 
#   mutate()  |> 
#   batch_select_rename()

# add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_harvest_more_than_4_months_in_sequence_12")


# logic_c_more_yield_but_also_crop_loss_13
df_logic_c_more_yield_but_also_crop_loss_13 <- df_tool_data |> 
  filter(yield_recent_harvest_season_meher %in% c("somewhat_more_than_normal", "much_more_than_normal"),
         crop_loss_estimate %in% c("few_crops_were_lost", "some_crops_were_lost", "many_crops_were_lost", "all_or_almost_all_crops_were_lost")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "yield_recent_harvest_season_meher",
         i.check.current_value = yield_recent_harvest_season_meher,
         i.check.value = "",
         i.check.issue_id = "logic_c_more_yield_but_also_crop_loss_13",
         i.check.issue = glue("yield_recent_harvest_season_meher: {yield_recent_harvest_season_meher} but crop_loss_estimate: {crop_loss_estimate}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "delete_log",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_more_yield_but_also_crop_loss_13")

# logic_c_no_inadequate_shelter_but_shelters_destroyed_14
df_logic_c_no_inadequate_shelter_but_shelters_destroyed_14 <- df_tool_data |> 
  filter(proportion_of_households_in_inadequate_shelter %in% c("no_households"),
         shelters_destroyed_or_severely_damaged %in% c("few_shelters", "some_shelters", "many_shelters", "almost_all_or_all_shelters")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "proportion_of_households_in_inadequate_shelter",
         i.check.current_value = proportion_of_households_in_inadequate_shelter,
         i.check.value = "",
         i.check.issue_id = "logic_c_no_inadequate_shelter_but_shelters_destroyed_14",
         i.check.issue = glue("proportion_of_households_in_inadequate_shelter: {proportion_of_households_in_inadequate_shelter} but shelters_destroyed_or_severely_damaged: {shelters_destroyed_or_severely_damaged}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_no_inadequate_shelter_but_shelters_destroyed_14")

# logic_c_time_short_but_health_facility_far_15
df_logic_c_time_short_but_health_facility_far_15 <- df_tool_data |> 
  filter(time_to_nearest_healthcare_facility %in% c("under_30_minutes"),
         str_detect(string = main_barriers_to_access_health_care, pattern = "health_facility_is_too_far_away")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "time_to_nearest_healthcare_facility",
         i.check.current_value = time_to_nearest_healthcare_facility,
         i.check.value = "",
         i.check.issue_id = "logic_c_time_short_but_health_facility_far_15",
         i.check.issue = glue("time_to_nearest_healthcare_facility: {time_to_nearest_healthcare_facility} but main_barriers_to_access_health_care: {main_barriers_to_access_health_care}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_time_short_but_health_facility_far_15")

# logic_c_time_short_but_school_far_16
df_logic_c_time_short_but_school_far_16 <- df_tool_data |> 
  filter(time_to_closest_functional_primary_school %in% c("under_30_minutes"),
         main_barrier_accessing_education_girls %in% c("distance_to_school_too_far")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "time_to_closest_functional_primary_school",
         i.check.current_value = time_to_closest_functional_primary_school,
         i.check.value = "",
         i.check.issue_id = "logic_c_time_short_but_school_far_16",
         i.check.issue = glue("time_to_closest_functional_primary_school: {time_to_closest_functional_primary_school} but main_barrier_accessing_education_girls: {main_barrier_accessing_education_girls}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_time_short_but_school_far_16")

# logic_c_time_short_but_school_far_17
df_logic_c_time_short_but_school_far_17 <- df_tool_data |> 
  filter(time_to_closest_functional_primary_school %in% c("under_30_minutes"),
         main_barrier_accessing_education_boys %in% c("distance_to_school_too_far")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "time_to_closest_functional_primary_school",
         i.check.current_value = time_to_closest_functional_primary_school,
         i.check.value = "",
         i.check.issue_id = "logic_c_time_short_but_school_far_17",
         i.check.issue = glue("time_to_closest_functional_primary_school: {time_to_closest_functional_primary_school} but main_barrier_accessing_education_boys: {main_barrier_accessing_education_boys}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_time_short_but_school_far_17")

# logic_c_time_short_but_school_far_18
df_logic_c_time_short_but_school_far_18 <- df_tool_data |> 
  filter(time_to_closest_functional_secondary_school %in% c("under_30_minutes"),
         main_barrier_accessing_education_girls %in% c("distance_to_school_too_far")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "time_to_closest_functional_secondary_school",
         i.check.current_value = time_to_closest_functional_secondary_school,
         i.check.value = "",
         i.check.issue_id = "logic_c_time_short_but_school_far_18",
         i.check.issue = glue("time_to_closest_functional_secondary_school: {time_to_closest_functional_secondary_school} but main_barrier_accessing_education_girls: {main_barrier_accessing_education_girls}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_time_short_but_school_far_18")

# logic_c_time_short_but_school_far_19
df_logic_c_time_short_but_school_far_19 <- df_tool_data |> 
  filter(time_to_closest_functional_secondary_school %in% c("under_30_minutes"),
         main_barrier_accessing_education_boys %in% c("distance_to_school_too_far")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "time_to_closest_functional_secondary_school",
         i.check.current_value = time_to_closest_functional_secondary_school,
         i.check.value = "",
         i.check.issue_id = "logic_c_time_short_but_school_far_19",
         i.check.issue = glue("time_to_closest_functional_secondary_school: {time_to_closest_functional_secondary_school} but main_barrier_accessing_education_boys: {main_barrier_accessing_education_boys}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_time_short_but_school_far_19")

# logic_c_time_fetch_short_but_waterpoints_far_20
df_logic_c_logic_c_time_fetch_short_but_waterpoints_far_20 <- df_tool_data |> 
  filter(time_to_fetch_water_and_return %in%  c('less_than_5_min'),
         str_detect(string = main_problems_accessing_water, pattern = "waterpoints_are_too_far")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "time_to_fetch_water_and_return",
         i.check.current_value = time_to_fetch_water_and_return,
         i.check.value = "",
         i.check.issue_id = "logic_c_time_fetch_short_but_waterpoints_far_20",
         i.check.issue = glue("time_to_fetch_water_and_return: {time_to_fetch_water_and_return} but main_problems_accessing_water: {main_problems_accessing_water}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_logic_c_time_fetch_short_but_waterpoints_far_20")

# logic_c_enough_water_but_no_water_drinking_21
df_logic_c_enough_water_but_no_water_drinking_21 <- df_tool_data |> 
  filter(proportion_hh_enough_water_cooking_bathing_washing %in%  c('all_or_almost_all_76_100'),
         proportion_hh_enough_water_drinking %in%  c('none')) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "proportion_hh_enough_water_cooking_bathing_washing",
         i.check.current_value = proportion_hh_enough_water_cooking_bathing_washing,
         i.check.value = "",
         i.check.issue_id = "logic_c_enough_water_but_no_water_drinking_21",
         i.check.issue = glue("proportion_hh_enough_water_cooking_bathing_washing: {proportion_hh_enough_water_cooking_bathing_washing} but proportion_hh_enough_water_drinking: {proportion_hh_enough_water_drinking}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_enough_water_but_no_water_drinking_21")

# logic_c_no_water_drinking_but_no_water_problems_22
df_logic_c_no_water_drinking_but_no_water_problems_22 <- df_tool_data |> 
  filter(proportion_hh_enough_water_drinking %in%  c('none'),
         str_detect(string = main_problems_accessing_water, pattern = "no_problem")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "proportion_hh_enough_water_drinking",
         i.check.current_value = proportion_hh_enough_water_drinking,
         i.check.value = "",
         i.check.issue_id = "logic_c_no_water_drinking_but_no_water_problems_22",
         i.check.issue = glue("proportion_hh_enough_water_drinking: {proportion_hh_enough_water_drinking} but main_problems_accessing_water: {main_problems_accessing_water}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_no_water_drinking_but_no_water_problems_22")

# logic_c_access_sanitation_but_open_defecation_23
df_logic_c_access_sanitation_but_open_defecation_23 <- df_tool_data |> 
  filter(proportion_hh_access_functioning_sanitation_facility %in% c("all_or_almost_all_76_100"),
         proportion_hh_engaged_in_open_defecation %in% c("all_or_almost_all_76_100")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "proportion_hh_access_functioning_sanitation_facility",
         i.check.current_value = proportion_hh_access_functioning_sanitation_facility,
         i.check.value = "",
         i.check.issue_id = "logic_c_access_sanitation_but_open_defecation_23",
         i.check.issue = glue("proportion_hh_access_functioning_sanitation_facility: {proportion_hh_access_functioning_sanitation_facility} but proportion_hh_engaged_in_open_defecation: {proportion_hh_engaged_in_open_defecation}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_access_sanitation_but_open_defecation_23")

# logic_c_feel_safe_but_women_safety_24
df_logic_c_feel_safe_but_women_safety_24 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("yes"),
         !str_detect(string = main_safety_concerns_for_women, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_feel_safe_but_women_safety_24",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_women: {main_safety_concerns_for_women}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_feel_safe_but_women_safety_24")

# logic_c_feel_safe_but_men_safety_25
df_logic_c_feel_safe_but_men_safety_25 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("yes"),
         !str_detect(string = main_safety_concerns_for_men, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_feel_safe_but_men_safety_25",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_men: {main_safety_concerns_for_men}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_feel_safe_but_men_safety_25")

# logic_c_feel_safe_but_girls_safety_26
df_logic_c_feel_safe_but_girls_safety_26 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("yes"),
         !str_detect(string = main_safety_concerns_for_girls, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_feel_safe_but_girls_safety_26",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_girls: {main_safety_concerns_for_girls}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_feel_safe_but_girls_safety_26")

# logic_c_feel_safe_but_boys_safety_27
df_logic_c_feel_safe_but_boys_safety_27 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("yes"),
         !str_detect(string = main_safety_concerns_for_boys, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_feel_safe_but_boys_safety_27",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_boys: {main_safety_concerns_for_boys}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_feel_safe_but_boys_safety_27")

# logic_c_not_feel_safe_but_women_safety_concerns_none_28
df_logic_c_not_feel_safe_but_women_safety_concerns_none_28 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("no"),
         str_detect(string = main_safety_concerns_for_women, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_not_feel_safe_but_women_safety_concerns_none_28",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_women: {main_safety_concerns_for_women}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_not_feel_safe_but_women_safety_concerns_none_28")

# logic_c_not_feel_safe_but_men_safety_concerns_none_29
df_logic_c_not_feel_safe_but_men_safety_concerns_none_29 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("no"),
         str_detect(string = main_safety_concerns_for_men, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_not_feel_safe_but_men_safety_concerns_none_29",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_men: {main_safety_concerns_for_men}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_not_feel_safe_but_men_safety_concerns_none_29")

# logic_c_not_feel_safe_but_girls_safety_concerns_none_30
df_logic_c_not_feel_safe_but_girls_safety_concerns_none_30 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("no"),
         str_detect(string = main_safety_concerns_for_girls, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_not_feel_safe_but_girls_safety_concerns_none_30",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_girls: {main_safety_concerns_for_girls}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_not_feel_safe_but_girls_safety_concerns_none_30")

# logic_c_not_feel_safe_but_boys_safety_concerns_none_31
df_logic_c_not_feel_safe_but_boys_safety_concerns_none_31 <- df_tool_data |> 
  filter(people_feel_safe_most_of_the_time %in%  c("no"),
         str_detect(string = main_safety_concerns_for_boys, pattern = "none")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "people_feel_safe_most_of_the_time",
         i.check.current_value = people_feel_safe_most_of_the_time,
         i.check.value = "",
         i.check.issue_id = "logic_c_not_feel_safe_but_boys_safety_concerns_none_31",
         i.check.issue = glue("people_feel_safe_most_of_the_time: {people_feel_safe_most_of_the_time} but main_safety_concerns_for_boys: {main_safety_concerns_for_boys}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_not_feel_safe_but_boys_safety_concerns_none_31")

# logic_c_humanitarian_food_but_no_assistance_32
df_logic_c_humanitarian_food_but_no_assistance_32 <- df_tool_data |> 
  filter(str_detect(string = main_sources_food_before_leaving, pattern = "received_humanitarian_food_assistance"),
         proportion_hh_received_assistance %in%  c("no_households")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "proportion_hh_received_assistance",
         i.check.current_value = proportion_hh_received_assistance,
         i.check.value = "",
         i.check.issue_id = "logic_c_humanitarian_food_but_no_assistance_32",
         i.check.issue = glue("proportion_hh_received_assistance: {proportion_hh_received_assistance} but main_sources_food_before_leaving: {main_sources_food_before_leaving}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_humanitarian_food_but_no_assistance_32")


# combined  checks --------------------------------------------------------

df_combined_checks <- bind_rows(checks_output) |> 
  mutate(int.issue_id = str_extract(string = issue_id, pattern = "[0-9]{1,3}")) |> 
  left_join(df_logical_check_description, by = c("int.issue_id" = "check_number")) |> 
  mutate(issue = ifelse(str_detect(string = issue_id, pattern = "[0-9]{1,3}"), paste("[", issue, "].", check_description), issue)) |> 
  select(-c(int.issue_id, check_description))

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_h2r_eth.csv"), na = "")
