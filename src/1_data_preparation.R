#############################################################
# Author: Assil BEN-AMOR (assil.benamor@reach-initiative.org)
# Created: 19/05/2022
# Last update: 19/05/2022
# Prepare the data for the analysis
##############################################################

##############################################################
# Use this script to apply changes on the dataset before running to analysis, eg:
# Recoding, adding, deleting variables ...
# The following junks of code are common modification that you may want to apply 
# to your dataset, for each block you want to use renov ethe leading "#" signs 
##############################################################




############# Making sure binaries of SM questions are stored as numerical values (0/1) instead of ("0"/"1") ############# 

# binary_q <- questions %>% filter(grepl("^select_multiple",type)) %>% pull(name)
# 
# binary_q_regex <- paste0("(",paste(paste0("^",questions %>% filter(grepl("select_multiple",type)) %>% pull(name),"\\."),collapse = '|'),")")
# data <- mutate_at(data,
#                   names(data)[str_detect(pattern = binary_q_regex,string = names(data))]
#                   ,as.numeric)

############# Remove binary fields added to select one questions if any ############# 

# regex_expr <- paste0("(",paste(paste0("^",questions %>% filter(grepl("select_one",type)) %>% pull(name),"\\."),collapse = '|'),")")
# 
# names(data)[str_detect(pattern = regex_expr,string = names(data))]
# 
# data <- data %>% select(-any_of(c(names(data)[str_detect(pattern = regex_expr,string = names(data))])))

############# Making sure numerical variables are formatted correctly ############# 

# num_q <- questions %>% filter(type %in% c("calculate","integer")) %>% pull(name)
# 
# num_q <- num_q[num_q %in% names(data)]
# 
# data <- mutate_at(data,num_q,as.numeric)

############# Remove non needed questions from data/analysis ############# 

# questions_to_remove <- c("end_", 
#   "today_",
#   "deviceid_")
#                                   
# data <- data %>% select(-any_of(questions_to_remove))



