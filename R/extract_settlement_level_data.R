library(tidyverse)
library(supporteR)  

# devtools::install_github("twesigye10/supporteR")

# mode_support <- function(x, na.rm = FALSE) {
#   
#   if(na.rm){ #if na.rm is TRUE, remove NA values from input x
#     x = x[!is.na(x)]
#   }
#   
#   val <- unique(x)
#   return(val[which.max(tabulate(match(x, val)))])
# }

mode_support <- function(x, na.rm = FALSE) {
  
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  # extract unique values
  val <- unique(x)
  # extract frequencies for values
  data_freqs <- tabulate(match(x, val))
  # max freq
  max_data_val <- max(data_freqs) 
  # if ties, no consensus
  if (length(data_freqs[data_freqs >= max_data_val]) > 1) {
    return("NC")
  } else{
    return(val[which.max(data_freqs)])
  }
  
}

# clean data
data_path <- "inputs/clean_data_h2r_eth.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/ETH2002_H2R_tool.xlsx", sheet = "survey") 


# settlement level data ---------------------------------------------------

# identify settlement level indicators
df_questions_dap <- readxl::read_excel("support_files/V6_REACH_ETH_2002_DAP_HSM_Northern_Ethiopia_August_2022_Translation.xlsx", sheet = "Quant_KI") |> 
  filter(!is.na(`Questionnaire question`))

df_survey_questions_level <- df_survey |> 
  mutate(int.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_),
         i.group = int.group) |> 
  fill(i.group)

# all settlement level questions
df_questions_settlement_level <- df_survey_questions_level |> 
  filter(str_detect(string = i.group, pattern = "grp_shocks|grp_displacement|grp_fs|grp_livelihoods|grp_agriculture|grp_marketsgrp_things_available_in_the_market|grp_shelter|grp_health|grp_education|grp_wash|grp_protection|grp_assistance"),
         !str_detect(string = name, pattern = "_other$"),
         !str_detect(string = type, pattern = "group|text|^note$")
         ) |> 
  select(type, name, `label::english`, i.group)

# select multiple questions
df_questions_settlement_level_sm <- df_questions_settlement_level |> 
  filter(str_detect(string = type, pattern = "select_multiple"))

# select settlement level data
df_settlement_level_data <- df_main_clean_data |> 
  select(info_region, info_zone, info_woreda, info_kebele, info_settlement, matches(paste(df_questions_settlement_level$name, collapse = "|"))) |> 
  select(!df_questions_settlement_level_sm$name, -ends_with("_other"))


# calculate mode for the indicators ---------------------------------------

# taking care of protection indicators and select multiple
# also even number of KIs for finding mode
location_cols <- c("info_region", "info_zone", "info_woreda", "info_kebele", "info_settlement")
df_settlement_level_data_processed <- df_settlement_level_data |> 
  mutate(across(.fns = ~as.character(.x))) |> 
  group_by(info_region, info_zone, info_woreda, info_kebele, info_settlement) |> 
  dplyr::summarise(across(.cols = -any_of(location_cols), .fns = ~mode_support(.x, na.rm = T)))

# export the data
write_csv(df_settlement_level_data_processed, paste0("outputs/settlement_level_data_h2r_eth.csv"), na="")
