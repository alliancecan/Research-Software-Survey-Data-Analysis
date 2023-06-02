# Prepare the directory and load the libraries ----------------------------

###Set working directory
setwd("C:/Users/fsdha/OneDrive - 11593765 Canada Association/Fares Drive/Works in progress/RS Survey Analysis/RS Git Repo/Data")

###load libraries
library(tidyverse)
library(stringr)
library(maps)
library(mapdata)
library(ggmap)
library(mapcan)
library(moonBook)
library(webr)
library(ggthemes)
library(RColorBrewer)
library(rmarkdown)
library(gtsummary)
library(gt)
library(RColorBrewer)
library(likert)
library(ggthemes)

###Load the data

#survey data, english and french versions merged together
# survey <- read.csv("Alliance_RS_Survey_EN_FR_20230602.csv",
#                    header = T,
#                    encoding = "UTF-8",
#                    na.strings=c("","NA"))

survey <- read.csv("ID_Alliance_RS_Survey_EN_FR_20230602.csv",
                   header = T,
                   encoding = "UTF-8",
                   na.strings=c("","NA")) %>% 
  rename(Internal.ID = ID)


# Organizing data ----------------------------------------------------------------
# 
# #Create unique ID per column, as raw data doesn't have unique ID
# set.seed(5)#to generate the same numbers each time the code is run
# randnum <- sample.int(1500,952)
# 
# 
# #bind data to IDs
# survey_ID <- survey %>% mutate(AB_ID = randnum)
# write.csv(survey_ID, "ID_Alliance_RS_Survey_EN_FR_20230602.csv")


###Re-organize the data
survey_gather <- 
  survey %>% 
  gather("Question", "Answer", 2:393)

split_question <- 
  data.frame(do.call('rbind', strsplit(as.character(survey_gather$Question),'..',fixed=TRUE)))

survey_organized <- 
  cbind(survey_gather, split_question) %>% 
  select(-Question) %>% 
  rename(Question = X2, Ques_num = X1) %>% 
  relocate(Answer, .after = Question)

survey_organized_clean <- 
  survey_organized %>% 
  select(-Question) %>% 
  drop_na()

  
#Spread the table: the output will have one column per question (in a list)
survey_organized_spread <- pivot_wider(survey_organized_clean, 
                                       names_from = Ques_num,
                                       values_from = Answer,
                                       values_fn = list)


# General Survey Questions ############################################################################################
### Q1 - Please describe your role(s). ######