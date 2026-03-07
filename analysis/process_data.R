library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "moral_character"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

#code for dealing with atypical participant id storage
exp_data <- exp_data %>%
  #clean up participant ids
  rename(participant_id = participant) %>%
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      participant_id == "A18534325" ~ "moose",
      TRUE ~ trimws(tolower(participant_id))
    )
  )

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

exp_data <- exp_data %>%
  mutate(
    block_type = case_when(
      str_detect(response,"manipulation_check") ~ "manipulation_check",
      TRUE ~ block_type
    )
  )

#extract likert responses
survey_likert_responses <- exp_data %>%
  filter(trial_type == "survey-likert") %>%
  select(random_id,participant_id,block_type,trial_index,response) %>%
  rowwise() %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(json) %>%
  #pivot longer all new columns post response, ignoring columns that are NA
  select(-response) %>%
  group_by(random_id,participant_id,block_type,trial_index) %>%
  pivot_longer(cols = -c(random_id,participant_id,block_type,trial_index),names_to = "likert_question",values_to = "likert_response") %>%
  filter(!is.na(likert_response))

#extract survey responses
survey_responses <- exp_data %>%
  filter(trial_type == "survey-text"& trial_index>5) %>%
  select(random_id,participant_id,response) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(json) %>%
  #unify into a single row per participants, removing NAs
  select(-response) %>%
  group_by(random_id,participant_id) %>%
  summarise_all(~ first(na.omit(.))) %>%
  rename(what_experiment_about = Q0)

# moral_attitudes <- exp_data %>%
#   filter(block_type=="moral_attitudes") %>%
#   select(random_id,participant_id,response) %>%
#   mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
#   unnest(json) %>%
#   select(-response)

#fill and filter exp_data
exp_data <- exp_data %>%
  fill(item_type) %>%
  #filter exp_data
  filter(trial_type == "survey-likert")
  

#join into exp_data
exp_data <- exp_data %>%
  #creates many-to many join, but I think that's ok
  left_join(survey_likert_responses) %>%
  left_join(survey_responses)

#filter participant ids
filter_ids <- c()

#identify participants from the experiment group
group_members <- c("moose","crow","whale","lion","mouse","calf")

processed_data <- exp_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #remove unneeded columns
  select(-c(success,plugin_version)) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(likert_eval_number = row_number()) %>%
  relocate(likert_eval_number,.after=trial_index)

#add condition info
processed_data <- processed_data %>%
  mutate(
    question_agent = case_when(
      #if the likert_question starts with n_ then nate
      str_detect(likert_question,"^n_") ~ "n",
      str_detect(likert_question,"^j_") ~ "j",
      str_detect(likert_question,"^b_") ~ "b",
      str_detect(likert_question,"^B_") ~ "b",
      TRUE ~ NA_character_
    ),
    actor_condition = case_when(
      #if the likert_question starts with n_ then nate
      str_detect(likert_question,"^n_") ~ "slow",
      str_detect(likert_question,"^j_") ~ "fast",
      str_detect(likert_question,"^b_") ~ "bystander",
      str_detect(likert_question,"^B_") ~ "bystander",
      TRUE ~ NA_character_
    ),
  )
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
