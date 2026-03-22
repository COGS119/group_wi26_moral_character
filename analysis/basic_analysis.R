library(here)
library(tidyverse)

processed_data_directory <- here("..","data","processed_data")
file_name <- "moral_character"

#read experiment data
processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv")))


#quick look

#look through manipulation check
man_check_long <- processed_data %>%
  select(random_id,participant_id,actor_condition,likert_question,likert_response) %>%
  filter(likert_question %in% c("j_manipulation_check","n_manipulation_check"))

man_check_wide <- man_check_long %>%
  group_by(random_id,participant_id) %>%
  pivot_wider(names_from = "likert_question",values_from = "likert_response")

ggplot(man_check_long,aes(likert_response))+
  geom_histogram()+
  facet_wrap(~likert_question)

average_subj_moral <- processed_data %>%
  filter(likert_question %in% c("n_principles_eval","j_principles_eval","n_standards_eval",
                                "j_standards_eval","n_deepness_eval","j_deepness_eval",
                                "b_principles_eval","b_standards_eval","b_deepness_eval")) %>%
  separate(likert_question,into=c("character","question"),sep="[_]",remove=F) %>%
  group_by(random_id,participant_id,condition,bystander_condition,character) %>%
  summarize(
    N=n(),
    s_avg=mean(likert_response)
  )%>%
  mutate(
    speed = case_when(
      character=="n" ~ "slow",
      character == "j" ~ "quick",
      TRUE ~ NA)
  )

overall_moral <- average_subj_moral %>%
  filter(character %in% c("n","j")) %>%
  group_by(condition,character) %>%
  summarize(
    N=n(),
    avg = mean(s_avg),
    sd = sd(s_avg),
    sem = sd / sqrt(N)
  ) %>%
  mutate(
    speed = ifelse(character=="n","slow","quick")
  )

ggplot(filter(overall_moral,character %in% c("n","j")),aes(condition,avg, fill=speed,color=speed))+
  geom_bar(stat="identity",position=position_dodge(.95),fill=NA)+
  geom_errorbar(aes(ymin=avg-sem,ymax=avg+sem),width=0,color="black",position=position_dodge(.95))+
  geom_jitter(aes(condition,s_avg),data=filter(average_subj_moral,!is.na(speed)),position=position_jitterdodge(dodge.width=.95,jitter.width=.05),alpha=.5)+
  geom_violin(aes(condition,s_avg),data=filter(average_subj_moral,!is.na(speed)),position=position_dodge(.95),fill=NA)+
  ylab("Positive Moral Character Evaluation")+
  theme_bw(base_size=16)


overall_moral_b <- average_subj_moral %>%
  filter(character %in% c("b")) %>%
  group_by(bystander_condition,condition,character) %>%
  summarize(
    N=n(),
    avg = mean(s_avg),
    sd = sd(s_avg),
    sem = sd / sqrt(N)
  )

ggplot(overall_moral_b,aes(condition,avg, fill=bystander_condition,color=bystander_condition))+
  geom_bar(stat="identity",position=position_dodge(.95))+
  geom_errorbar(aes(ymin=avg-sem,ymax=avg+sem),width=0,color="black",position=position_dodge(.95))+
  ylab("Positive Moral Character Evaluation")+
  theme_bw(base_size=16)
