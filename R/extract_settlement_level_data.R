library(tidyverse)
library(supporteR)  

# devtools::install_github("twesigye10/supporteR")

source("R/composite_indicators.R")

# clean data
data_path <- "inputs/clean_data_h2r_eth.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/ETH2002_H2R_tool.xlsx", sheet = "survey") 

df_main_clean_data |> 
  group_by(info_region, info_zone, info_woreda, info_kebele, info_settlement) 
  

df_main_clean_data |> 
  select(info_region, info_zone, info_woreda, info_kebele, info_settlement) |> 
  view()
  