# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df %>% 
    mutate(i.fcs = (fcs_cereals*2 + fcs_pulses*3 + fcs_vegetable +  
                      fcs_fruits + fcs_meat*4 + fcs_dairy*4 + 
                      fcs_sugar*0.5 + fcs_oil*0.5),
           i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                 i.fcs <= 35 ~ "Borderline",
                                 i.fcs <= 112 ~ "Acceptable"),
           int.freq_no_food_lack_resources = case_when(if_yes_food_availability_how_often %in% c("rarely") ~ 0,
                                                       if_yes_food_availability_how_often %in% c("sometimes") ~ 1,
                                                       if_yes_food_availability_how_often %in% c("often") ~ 2),
           int.freq_sleep_hungry = case_when(if_yes_sleep_hungry_how_often %in% c("rarely") ~ 0,
                                             if_yes_sleep_hungry_how_often %in% c("sometimes") ~ 1,
                                             if_yes_sleep_hungry_how_often %in% c("often") ~ 2),
           int.freq_day_and_night_no_food = case_when(if_yes_whole_day_and_night_no_food_how_often %in% c("rarely") ~ 0,
                                                      if_yes_whole_day_and_night_no_food_how_often %in% c("sometimes") ~ 1,
                                                      if_yes_whole_day_and_night_no_food_how_often %in% c("often") ~ 2),
           int.displacement_time = lubridate::time_length(date_arrived_current_location - date_last_in_settlement, unit = "day"),
           i.displacement_time = case_when(int.displacement_time <= 5 ~ "days_0_5",
                                           int.displacement_time <= 10 ~ "days_6_10",
                                           int.displacement_time <= 30 ~ "days_10+"),
           i.crops_destroyed_by_conflict = format(as_date(int.crops_destroyed_by_conflict), "%Y_%b" ),
           i.when_schools_last_opened = format(as_date(int.when_schools_last_opened), "%Y_%b"),
           i.respondent_age = case_when(respondent_age < 18 ~ "age_12_17",
                                        respondent_age <= 24 ~ "age_18_24",
                                        respondent_age <= 39 ~ "age_25_39",
                                        respondent_age <= 59 ~ "age_40_59",
                                        respondent_age > 59 ~ "age_60+"),
           i.freq_displacements = case_when(freq_displacements <= 2 ~ "freq_1_2",
                                            freq_displacements <= 5 ~ "freq_3_5",
                                            freq_displacements > 5 ~ "freq_6+")
    ) |> 
    rowwise() |> 
    mutate( 
      i.hhs = sum(c_across(int.freq_no_food_lack_resources:int.freq_day_and_night_no_food), na.rm = T)
    ) |>
    ungroup() |>
    mutate(i.hhs_cat = case_when(i.hhs <= 1 ~ "Little to no hunger",
                                 i.hhs <= 3 ~ "Moderate hunger",
                                 i.hhs <= 6 ~ "Severe hunger"),
           i.fewsnet_phase = case_when(i.hhs == 0 ~ "Phase 1",
                                     i.hhs == 1 ~ "Phase 2",
                                     i.hhs <= 3 ~ "Phase 3",
                                     i.hhs == 4 ~ "Phase 4",
                                     i.hhs <= 6 ~ "Phase 5")) |> 
    select(-c(starts_with("int.")))
}

