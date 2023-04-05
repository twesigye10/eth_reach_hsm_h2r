# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df %>% 
    mutate(i.fcs = (fcs_cereals*2 + fcs_pulses*3 + fcs_vegetable +  
                      fcs_fruits + fcs_meat*4 + fcs_dairy*4 + 
                      fcs_sugar*0.5 + fcs_oil*0.5),
           i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                 i.fcs <= 35 ~ "Borderline",
                                 i.fcs <= 112 ~ "Acceptable"),
           int.freq_no_food_lack_resources = case_when(if_yes_food_availability_how_often %in% c("sometimes") ~ 1,
                                                       if_yes_food_availability_how_often %in% c("often") ~ 2,
                                                       TRUE ~ 0),
           int.freq_sleep_hungry = case_when(if_yes_sleep_hungry_how_often %in% c("sometimes") ~ 1,
                                             if_yes_sleep_hungry_how_often %in% c("often") ~ 2,
                                             TRUE ~ 0),
           int.freq_day_and_night_no_food = case_when(if_yes_whole_day_and_night_no_food_how_often %in% c("sometimes") ~ 1,
                                                      if_yes_whole_day_and_night_no_food_how_often %in% c("often") ~ 2,
                                                      TRUE ~ 0),
           i.hhs = (int.freq_no_food_lack_resources + int.freq_sleep_hungry + int.freq_day_and_night_no_food),
           i.hhs_cat = case_when(i.hhs <= 1 ~ "Little to no hunger",
                                 i.hhs <= 3 ~ "Moderate hunger",
                                 i.hhs <= 6 ~ "Severe hunger"
           )
    ) |> 
    select(-c(starts_with("int.")))
}