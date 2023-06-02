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

#Select columns that will be used for the data analysis
survey_selected_data <- 
  survey_organized_spread %>% 
  select(Internal.ID, B1, B2, B3, B7, B8, C2, C3, C4, C5, C6, C7, C10, C11,
         C12, C13, C14, C15, D2, D3, D4, D5, D6, D8, D9)

# General colour Schemes ############################################################################################

cbp1 <- rep(c("#B7B6B3", "#D6AB00","#00DBA7", "#56B4E9",
              "#32322F", "#FBFAFA", "#D55E00", "#CC79A7"), 100)

#This to be used to plot the geographical location                      
cbp_Cad <- rep(c("#B7B6B3", "#D6AB00","#00DBA7", "#56B4E9",
                 "#32322F", "darkturquoise", "#D55E00", "#CC79A7",
                 "green4", "lightslategrey"), 100)

# This to be used for yes, no, not sure                 
cb_pie <- rep(c("#32322F","#FBFAFA", "#D6AB00","#00DBA7", "#B7B6B3",
                "#0072B2", "#D55E00", "#CC79A7"), 100)

# This to be used to plot the domains
cb_pie_3 <- rep(c("#32322F","#B7B6B3", "#D6AB00"), 100)



# General Survey Questions ############################################################################################
### B1 - What is your primary institutional affiliation? ######
survey_B1_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B1) %>% 
  unnest(B1) %>% 
  rename(Affiliation = B1)

sort(unique(survey_B1_v1$Affiliation))# to clean the data

survey_B1_v2 <- 
  survey_B1_v1 %>% 
  mutate(Affiliation_n = ifelse(
    Affiliation == "Lawson Research Institiute ", "Lawson Research Institute", ifelse(
      Affiliation == "Autre", "Other", ifelse(
        Affiliation == "Birds Canada (NGO)", "Birds Canada" , Affiliation
))))
