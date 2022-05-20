library(questionr)
library(tidyverse)
library(data.table)
library(stringr)
library(tcltk)

append_labels <- function(table_maker_output,labels_column) {
  

  cat(blue(paste0("----100%\n\n")))
  cat(blue("Adding labels to the results table .... \n\n"))
  
  df <- table_maker_output %>% select(1,2) %>% setnames(c("answer", "question"))
  
  df[df==""] <- NA
  
  df <- df %>% mutate(
    question = ifelse(is.na(question),answer,as.character(NA))
  )
  
  for (i in 1:nrow(df)) {
    question_name = ifelse(!is.na(df[i,2]),df[i,2],question_name)
    df[i,2] = question_name
  }
  
  df <- df %>% mutate(
    key = case_when(
      answer == "Average" ~ "Average",
      answer == question ~ question,
      TRUE ~ paste0(question,"####",answer)
    )
  )
  
  questions_labels <- questions %>% select(key= name,label = labels_column) %>% mutate(
    key=str_trim(key,side = "both"),
    label=str_trim(label,side = "both")
  ) %>% filter(!is.na(key),!is.na(label),key!="",label!="")
  

  answers_labels <- questions %>% filter(grepl("select_(one|multiple)",type)) %>% select(name,type) %>% mutate(
    type = str_trim(gsub("^\\s*select_(one|multiple)\\s*","",type),side = "both"),
    name = str_trim(name,side = "both")
    
  )
  
  
  answers_labels <- do.call("rbind",map2(answers_labels[[1]],answers_labels[[2]],
                                         ~ {choices %>% mutate(list_name=str_trim(list_name,side = "both")) %>% 
                                             filter(list_name == .y) %>% select(answer = name , label = labels_column) %>% 
                                             mutate(question = .x) %>% mutate(key=paste0(question,"####",answer)) %>% select(key,label)}
  ))
  
  
  
  df <- left_join(df,
            rbind(rbind(questions_labels,answers_labels),
                  c("Average","Average")),by="key"
            ) %>% mutate(
              labeled = ifelse(is.na(label),answer,label)
            ) 
  
  return(df$labeled)
  
  
}

get_label <- function(key,labels_column) {
  
  # r = round(match(key, keys) / length(keys) *100,2)
  # setTkProgressBar(pb1, r, progress.bar.title, paste0("Percentage ", r,"%"))
  
  if (key=="Average") return("Average")
  
  if(!str_detect(key,"####")) {
    label = hash_table_questions[[key]]
    if(is.null(label)) {
      return(key)
      } else {
        return(hash_table_questions[[key]])
      }

  } else{
    
    if(!is.null(hash_table_anwers[[key]])){
      return(hash_table_anwers[[key]])
    } else {
      return(unlist(str_split(key,"####"))[2])
    }
    
  }
  
  return(as.character(NA))
}

analyzer <- function(x, in_questionnaire, data, weights_vector_name,
                     main_col_name = "All", ...) {
  #print(x)
  cat(blue(paste0("----",round(match(x, colnames(data)) / ncol(data) *100,2),"%\n\n")))
  r = round(match(x, colnames(data)) / ncol(data) *100,2)
  
  setTkProgressBar(pb, r, progress.bar.title, paste0("Percentage ", r,"%"))

  if (is.null(weights_vector_name)) {
    weights <- rep(1, nrow(data))
  } else {
    weights <- data[[paste0(weights_vector_name)]]
  }
  
  strata <- list(...)
  strata <- unlist(strata)
  strata <- strata[!is.na(strata)]

  x_data <- data[[x]][which(!is.na(data[[x]]))]
  weights <- weights[which(!is.na(data[[x]]))]
  
  if (class(x_data) %in% c("logical", "numeric", "integer")) {
    if (in_questionnaire) {
      avg <- round(wtd.mean(x_data, weights = weights),2)
    } else if (min(x_data) >= 0 & max(x_data) <= 1) {
      avg <- 100 * round(wtd.mean(x_data, weights = weights), 2)
    } else {
      avg <- round(wtd.mean(x_data, weights = weights),2)
    }
    table <- tibble("data" = c(x, "Average"), !!main_col_name := c(main_col_name, avg))
  } else {
    table <- wtd.table(x_data, rep(1, length(x_data)), weights = weights)
    if(is_empty(table)) {
      table <- tibble("data" = "", !!main_col_name := "")
    } else {
      table <- prop(table)
      table <- as.data.frame.matrix(table)
      table[,1] <- row.names(table)
      table <- table[-nrow(table),]
      table <- table[order(as.numeric(table[,ncol(table)]),
                           decreasing = T), c(1, ncol(table))] %>%
        mutate(Total = round(Total, 2))
      table <- rbind(c(x, main_col_name), table)
      table <- as.data.frame(table)
      names(table) <- c("data", main_col_name)
    }
  } 
  if (length(strata) > 0) {
    for (i in 1:length(strata)) {
      groups <- unique(unlist(data[strata[[i]]]))
      groups <- groups[!is.na(groups)]
      for (j in 1:length(groups)) {
        new_data <- filter(data, !!sym(strata[[i]]) == groups[j])
        new_table <- analyzer(x, in_questionnaire, new_data, 
                               weights_vector_name,
                               groups[j], NA)
        table <- left_join_NA(table, new_table, by = "data")
      }
    }
  }
  return(table)
}

left_join_NA <- function(x, y, ...) {
  
  left_join(x = x, y = y, by = ...) %>% 
    replace(is.na(.), 0)
    #mutate_each(list(~replace(., which(is.na(.)), 0)))
}


table_maker <- function(data, questionnaire_object, questionnaire, choices, weights_vector_name,
                        labels = T, language = NULL, 
                        main_col_name = "All", ...) {
  # collecting strata information
  strata <- list(...)
  strata <- strata[!is.na(strata)]
  strata <- unlist(strata)
  
  var.names <- names(data)
  # remove the text column for select multiple
  sel_mul <- filter(questionnaire, str_detect(type, "select_multiple"))$name
  data <- select(data, -one_of(sel_mul))
  
  # getting analysis names, don't analyze strata and weights
  if (!is.null(strata)) {
    analysis_names <- names(select(data, -one_of(strata,"weights")))
  } else {
    analysis_names <- names(select(data, -one_of("weights")))
  }
  
  # detect which are in the questionnaire
  in_questionnaire <- analysis_names %in% questionnaire$name
  
  # get initial table output
  progress.bar.title <<- as.character("Analysis in progress ... ")
  pb <<- tkProgressBar(progress.bar.title, "Number of entries executed", 0, 100, 0)
  
  if (!is.null(strata)) {
    table_output <- bind_rows(map2(analysis_names, 
                                   in_questionnaire, 
                                   analyzer, 
                                   data,
                                   weights_vector_name,
                                   main_col_name,
                                   strata))
  } else {
    table_output <- bind_rows(map2(analysis_names, 
                                   in_questionnaire, 
                                   analyzer,
                                   data, 
                                   weights_vector_name,
                                   main_col_name))
  }
  
  # Editing select multiple binary options
  # Removes extra rows so that we end up with # of rows for # of options
  
  if(sum(unlist(purrr::map(var.names, questionnaire_object$question_is_select_multiple)))>0){
    
    sel_mul_rgx <- paste0(sel_mul, "(\\/|\\.)")
    sel_mul_extract_rgx <- paste0("(", str_c(sel_mul, collapse = "|"), ")")
    sel_mul_remove_rgx <- paste0("(", str_c(sel_mul_rgx, collapse = "|"), ")")
    
    table_output <- table_output %>%
      mutate(sel_mul = str_extract(data, sel_mul_extract_rgx))
    num_var_indices <- which(unlist(purrr::map(table_output %>% pull(data), questionnaire_object$question_is_numeric))) +1
    avg_indices <- which(table_output[, 1] == "Average")
    avg_indices <- avg_indices[! avg_indices %in% num_var_indices]
    
    table_output[avg_indices, 1] <- table_output[avg_indices - 1, 1]
    # avg_indices <- avg_indices[avg_indices>=3]
    match_previous <- (table_output[avg_indices - 3, "sel_mul"] == table_output[avg_indices - 1, "sel_mul"])
    match_previous[is.na(match_previous)] <- FALSE
    rem_avg_indices <- avg_indices[match_previous]
    table_output <- table_output[-(rem_avg_indices - 1), ] %>%
      select(-sel_mul)
    
    # Removing select_multiple question names from binary vars
    # Also removing select multiple binary option from the main question
    
    table_output[,1] <- ifelse(table_output[,2] == main_col_name & str_detect(table_output[,1], sel_mul_remove_rgx),
                               str_extract(table_output[,1], sel_mul_extract_rgx),
                               table_output[,1])
    
    table_output[,1] <- ifelse(table_output[,2] != main_col_name,
                               str_remove_all(table_output[,1], sel_mul_remove_rgx),
                               table_output[,1])
  }
  # Getting question labels if requested
  
  if (labels) {
    if (is.null(language)) {
      label_col <- "label"
    } else {
      language <- gsub("^label(::|\\.\\.)","",language)
      cols <- names(questionnaire)
      label_col <- str_detect(tolower(cols), paste0("label[\\W]{2}(?i)", tolower(language)))
      label_col <- cols[label_col]
    }
    
   
    
    choice_indices <- match(table_output$data, choices$name)
    choice_labels <- choices[[label_col]]
    question_indices <- match(table_output$data, questionnaire$name)
    question_labels <- questionnaire[[label_col]]
    
    table_output <- table_output %>%
      mutate(data = ifelse(is.na(question_indices), 
                           data, 
                           ifelse(is.na(question_labels[question_indices]) | question_labels[question_indices] == "",
                                  data,
                                  question_labels[question_indices])),
             data = ifelse(is.na(choice_indices),
                           data,
                           choice_labels[choice_indices]))
    
    # Fixing select multiple labels
    
    sel_mul_indices <- match(sel_mul, questionnaire$name)
    
    for (i in 1:length(sel_mul_indices)) {
      table_output[,1] <- str_replace(table_output[,1], paste0(questionnaire$name[sel_mul_indices[i]],"\\b"), question_labels[sel_mul_indices[i]])
    }
  }
  
  # Cleaning rows with question names
  
  split_rows <- table_output[,2] == main_col_name
  table_output[split_rows, 2:ncol(table_output)] <- ""
  
  return(table_output[5:nrow(table_output),])
}



generate_results_table <- function(data,
                                   questions,
                                   choices,
                                   weights.column = "weights",
                                   use_labels = T,
                                   labels_column = "label::English",
                                   ...
) {
  
  

  
  
  ### extract strata variables
  strata <- list(...)
  strata <- unlist(strata)
  strata <- strata[!is.na(strata)]
  
  if(!(labels_column %in% names(questions) & labels_column %in% names(choices))){
    stop("labels column doesn't exist in the questions or choices datasets")
  }
    
  
  if(!is.null(strata)){
    if(sum(!strata %in% names(data)) > 0){
      stop(sprintf("Strata variable is not in the dataset:\n ==> %s",
                   paste(strata[which(!strata %in% names(data))],collapse = "\n ==> ")))
    }
  }
  
  
  if(is.null(weights.column)) {
    weights.column = "weights"
    data[weights.column] = 1
  }
  
  if(!weights.column %in% names(data)){
    stop(sprintf("The weights column (%s) doesn't exist in the dataset",weights.column))
  }
  
  if(!is.numeric(data[[weights.column]])){
    stop(sprintf("Weights column should be numeric\n  Hint: Convert using: [df_name]$%s = as.numeric([df_name]$%s)",
                 weights.column,weights.column))
  }
  
  
  if (length(which(str_detect(names(data),"/")))>0) {
    stop("Group names should be removed from the columns names and '.' should be used a seperator between the question and answer in select multiple questions" )
  }
  
  
  
  ### Build the questionnaire object
  questionnaire <- load_questionnaire(data = data,
                                      questions = questions,
                                      choices = choices,
                                      choices.label.column.to.use = labels_column)
  
  
  ### Remove empty columns, text questions, other questions ,,,
  
  text_columns_to_exclude = filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)|(\\backnowledge\\b)"))$name
  
  generic_columns_to_exclude =   c("X_id", "X_uuid", "X_submission_time", "X_validation_status", "X_notes", "X_status",
                                   "X_submitted_by", "X_tags", "X_index", "id", "uuid", "submission_time", "validation_status",
                                   "notes", "status", "submitted_by", "tags", "index", "_id", "_uuid", "_submission_time",
                                   "_validation_status", "_notes", "_status", "_submitted_by", "_tags", "_index", 
                                   "end_note", "start", "end", "today", "deviceid", "device_id","enumerator_id"
                                   )
  

  
  unique_or_equal_columns_to_exclude = map2_chr(data,
                                                names(data),{
                                                  ~  ifelse(length(unique(.x)) %in% c(length(.x),1),.y,as.character(NA))
                                                }) %>% .[!is.na(.)]
  
  dates_columns_to_exclude = questions %>% filter(type=="date") %>% pull(name)
    
  columns_to_exclude = c(text_columns_to_exclude,
                         generic_columns_to_exclude,
                         unique_or_equal_columns_to_exclude,
                         dates_columns_to_exclude
  ) %>% unique()
  
  
  columns_to_exclude = columns_to_exclude[columns_to_exclude!=weights.column]
  
  data_to_analyze <- data %>% 
    select(-one_of(columns_to_exclude)) %>%
    select_if(~ !(all(is.na(.x)) | all(. == "")))
  
  ### Rename the weights column to "weights"
  
  data_to_analyze <- data_to_analyze %>% rename(
    c("weights"=weights.column)
  )
  
  data_to_analyze$tmp_q = rep(c("A","B","C"),nrow(data_to_analyze))[1:nrow(data_to_analyze)]
  data_to_analyze <- data_to_analyze %>% select(ncol(data_to_analyze),
                                                1:(ncol(data_to_analyze)-1))
  
  if (use_labels == F) {
    results_table_output <- table_maker(data_to_analyze, 
                                        questionnaire,
                                        questions, 
                                        choices,
                                        "weights",
                                        labels = FALSE, 
                                        language = NULL, 
                                        "All",
                                        strata
    )
    
    close(pb)
    
    results_table_output[results_table_output==""] <- NA
    

    for (i in 1:nrow(results_table_output)) {
      for (j in 2:ncol(results_table_output)) {
        if (results_table_output[i,1]!="Average" & !is.na(results_table_output[i,j])) {
          results_table_output[i,j] = as.character(as.numeric(results_table_output[i,j])/100)
        }
      }
      
    }
    
    
  }
  
  else {
    
    results_table_output <- table_maker(data_to_analyze, 
                                        questionnaire,
                                        questions, 
                                        choices,
                                        "weights",
                                        labels = FALSE, 
                                        language = NULL, 
                                        "All",
                                        strata)
    close(pb)
    
   
    
    results_table_output <- cbind(
      append_labels(results_table_output,labels_column),
      results_table_output
    )
    

    colnames(results_table_output)[1]="Question label"
    colnames(results_table_output)[2]="Question xml"
    
    results_table_output[results_table_output==""] <- NA
    
    for (i in 1:nrow(results_table_output)) {
      for (j in 3:ncol(results_table_output)) {
        if (results_table_output[i,1]!="Average" & !is.na(results_table_output[i,j])) {
          results_table_output[i,j] = as.character(as.numeric(results_table_output[i,j])/100)
        }
      }
      
    }
    
  }
  

  results_table_output
  
  
}



