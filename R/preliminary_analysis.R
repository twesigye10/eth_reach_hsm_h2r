library(tidyverse)
library(srvyr)
library(supporteR)

# clean data
data_path <- "inputs/clean_data_h2r_eth.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/ETH2002_H2R_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_h2r_eth.csv")

# set up design object
ref_svy <- as_survey(.data = df_main_clean_data)