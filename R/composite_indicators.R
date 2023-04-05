# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df %>% 
    mutate(
      i.fcs = (fcs_cereals*2 + fcs_pulses*3 + fcs_vegetable +  
                 fcs_fruits + fcs_meat*4 + fcs_dairy*4 + 
                 fcs_sugar*0.5 + fcs_oil*0.5),
      i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                            i.fcs <= 35 ~ "Borderline",
                            i.fcs <= 112 ~ "Acceptable",
                            # TRUE ~ "NA"
      )
    ) |> 
    select(-c(starts_with("int.")))
}