#############################################################
# Author: Assil BEN-AMOR (assil.benamor@reach-initiative.org)
# Created: 19/05/2022
# Last update: 19/05/2022
# Loading dataset, choices, and questions
##############################################################


rm(list = ls())

options(scipen = 999)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               hypegrammaR,
               rio,
               readxl,
               openxlsx)


source("src/functions/utils.R")
source("src/functions/results_table_functions_weigths_improved_purrr.R")

dir.create(file.path("input"), showWarnings = F)
dir.create(file.path("output"), showWarnings = F)

###### Specify the input file locations and export language (should match label::<language> column in the kobo tool)
params = list(
  assessment_name = "",
  data_location = "",
  kobo_tool_location = "",
  export_language = "",
)


data <- import(params$data_location)

questions <- import(params$kobo_tool_location) %>% filter(!is.na(name)) %>% 
  mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
         list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
         list_name=ifelse(str_starts(type, "select_"), list_name, NA))
  

choices <- import(params$kobo_tool_location,sheet="choices") %>% 
  filter(!is.na(list_name))


###### Source the data preparation script 

source("src/1_data_preparation.R")


###### Run the analysis

analysis_output <- generate_results_table(data = data,
                              questions = questions,
                              choices = choices,
                              weights.column = NULL,
                              use_labels = T,
                              labels_column = params$export_language
                              ## Add here the disaggregation variables separated by ,
)

###### Export the output

dir.create(file.path(sprintf("output/%s",params$assessment_name)), showWarnings = TRUE)
export_table(analysis_output,params$assessment_name,sprintf("output/%s/",params$assessment_name))


