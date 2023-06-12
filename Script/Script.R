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

survey <- read.csv("ID_Alliance_RS_Survey_EN_FR_20230612.csv",
                   header = T,
                   encoding = "UTF-8",
                   na.strings=c("","NA")) %>%
  rename(Internal.ID = X.U.FEFF.ID) # n = 548


# Organizing data ----------------------------------------------------------------
# # 
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

# This is for mirror plots
cb_pie1 <- rep(c("#32322F", "#D6AB00"), 100)
cb_pie2 <- rep(c("#D6AB00","#32322F"), 100)

# This to be used to plot the domains
cb_pie_3 <- rep(c("#32322F","#B7B6B3", "#D6AB00"), 100)



# General Survey Questions ############################################################################################
### B1 - What is your primary institutional affiliation? ######
survey_B1_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B1) %>% 
  unnest(B1) %>% 
  rename(Affiliation = B1) # n = 344

sort(unique(survey_B1_v1$Affiliation))# to clean the data

survey_B1_v2 <- 
  survey_B1_v1 %>% 
  mutate(Affiliation_n = ifelse(
    Affiliation == "Lawson Research Institiute ", "Lawson Research Institute", ifelse(
      Affiliation == "Autre", "Other", ifelse(
        Affiliation == "Birds Canada (NGO)", "Birds Canada" , ifelse(
          Affiliation == "  ", NA, Affiliation
          )))))


### B2 - Please choose your primary research domain based on the Canadian Research and Development Classification (CRDC) 2020. ######
Domain_Breakdown<- 
  survey_organized_spread %>% 
  select(Internal.ID, B2) %>% 
  unnest(B2) %>% 
  rename(Domain = B2) # n = 312

#summarise the data = count domain's n
domain_summary <- 
  Domain_Breakdown %>% 
  group_by(Domain) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()

domain_summary_v1 <- domain_summary
domain_summary_v1$Domain[domain_summary_v1$Domain== "Social sciences"] <- "Social sciences"
domain_summary_v1$Domain[domain_summary_v1$Domain == "Agricultural and veterinary sciences"] <- "Agricultural and\nveterinary sciences"
domain_summary_v1$Domain[domain_summary_v1$Domain == "Engineering and technology"] <- "Engineering and\ntechnology"
domain_summary_v1$Domain[domain_summary_v1$Domain == "Medical, health and life sciences"] <- "Medical, health\nand life sciences"
domain_summary_v1$Domain[domain_summary_v1$Domain == "Humanities and the artss"] <- "Humanities and\nthe arts"

#### Group domains into TC3 ####
domain_summary1 <-
  domain_summary %>%
  mutate(TC3 = ifelse(
    Domain == "Natural sciences", "Sciences and Engineering", ifelse(
        Domain == "Medical, health and life sciences", "Health Research", ifelse(
            Domain == "Engineering and technology", "Sciences and Engineering", ifelse(
              Domain ==  "Humanities and the arts", "Social Sciences and Humanities", ifelse(
                  Domain == "Social sciences", "Social Sciences and Humanities", ifelse(
                        Domain == "Agricultural and veterinary sciences", "Sciences and Engineering", ifelse(
                          ))))))))

#Link TC3 to Internal.ID = this will be used for the rest of the analysis as we will analyse data by TC3
domain <- 
  Domain_Breakdown %>% 
  left_join(domain_summary1, by = "Domain") %>% 
  select(-n) # n = 312

domain1 <- 
  domain %>% 
  select(-Domain)

b2.domain <- domain

#group by domain
b2.domain.summary <- 
  b2.domain %>% 
  group_by(TC3) %>% count() %>% drop_na()

# #for esthethis purposes, we add "\n" to long TC3 names and to domains so they the names will fully appear in the pie charts
b2.domain.summary$TC3[b2.domain.summary$TC3 == "Social Sciences and Humanities"] <- "Social Sciences\nand Humanities"
b2.domain.summary$TC3[b2.domain.summary$TC3 == "Sciences and Engineering"] <- "Sciences and\nEngineering"
#### Pie charts ####

PieDonut(domain_summary_v1, 
         aes(Domain, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Respondents' roles", 
         titlesize = 5,
         pieLabelSize = 7)+
  scale_fill_manual(values =  cbp_Cad)


#TC3
PieDonut(b2.domain.summary, 
         aes(TC3, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Respondents' roles", 
         titlesize = 5,
         pieLabelSize = 7)+
  scale_fill_manual(values =  cbp1)



### B3 - Please choose the option that best describes your role at your institution. ######
survey_B3_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B3) %>% 
  unnest(B3) %>% 
  rename(Role = B3) # n = 328


##From "Other", avoid duplication and select answers

#Add "2" to rows that I want to select
duplications_v1 <- survey_B3_v1 %>% 
  group_by(Internal.ID) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>% 
  mutate(is_duplicated = dup_id > 1)

#select rows with dup_id = 2
other_answe <- 
  duplications_v1 %>% 
  filter(dup_id == 2)

#select rows with num_dups = 1
duplications_v2 <- 
  duplications_v1 %>% 
  filter(num_dups == 1)

#bind the two tables to have the final table
survey_B3_v3 <- 
  rbind(other_answe, duplications_v2) %>% 
  select(Internal.ID, Role)

#Clean the data
survey_B3_v4 <- 
  survey_B3_v3 %>% 
  filter(!Role == "Other") %>% 
  mutate(Role_n = ifelse(
    Role == "staff in research support unit", "Other", ifelse(
      Role == "Personnel de soutien", "Other", ifelse(
        Role == "Bioinformatics", "Other", ifelse(
          Role == "Program Manager", "Other", ifelse(
            Role == "Director", "Other", ifelse(
              Role == "Community Manager", "Other", ifelse(
                Role == "User Support Analyst", "Other", ifelse(
                  Role == "Sysadmin", "Other", ifelse(
                    Role == "Data Steward", "Other", ifelse(
                      Role == "Conseillere a la recherche", "Other", ifelse(
                        Role == "Software Developer", "Other", ifelse(
                          Role == "Emeritus", "Faculty - Adjunct, emeritus, visiting, or limited-term", ifelse(
                            Role == "DG CCTT affilie cegep Ste-Foy", "Other", ifelse(
                              Role == "IT", "Other", ifelse(
                                Role == "project manager", "Other", Role
                                ))))))))))))))))

#### summary table - Roles ####
roles_summary <-
  survey_B3_v4 %>%
  group_by(Role_n) %>%
  count() %>%
  arrange(-n) %>%
  drop_na() %>%
  print()

roles_summary$Role_n[roles_summary$Role_n == "Faculty - Professor (including assistant/associate/full professor, clinical professor, teaching professor)"] <-  "Faculty - Professor"

#### Bar plot ####
# PieDonut(roles_summary, 
#          aes(Role_n, count= n), 
#          ratioByGroup = FALSE, 
#          showPieName=FALSE, 
#          r0=0.25,r1=1,r2=1.4,start=pi/2,
#          labelpositionThreshold=1, 
#          showRatioThreshold = F, 
#          title= "Respondents' roles", 
#          titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cbp_Cad) #+ 

ggplot(roles_summary, aes(y=n, x=reorder(Role_n, n))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip()+
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")

### B7 - Are you eligible to apply for and receive Tri-Council, CFI, or other research funding? ######
survey_B7_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B7) %>% 
  unnest(B7)

B7_summay <- 
  survey_B7_v1 %>% 
  group_by(B7) %>% 
  count()

#### Pie chart #### 
PieDonut(B7_summay, 
         aes(B7, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5, 
         pieAlpha = 1, 
         donutAlpha = 1, 
         color = "black",
         pieLabelSize = 7)+ 
  scale_fill_manual(values =  cb_pie)


### B8 - How is your group’s research software budget funded? ######
survey_B8_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B8) %>% 
  unnest(B8) %>% 
  rename(answer = B8)


#summarize the data
B8_summary <- 
  survey_B8_v2 %>% 
  group_by(answer_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#link TC3 to B8 IDs
B8.domain <- 
  survey_B8_v2 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  print() ## n = 358 = unique(Internal.ID)


Workflow.B8 <- 
  B8.domain %>% 
  unique()

nHR <- filter(Workflow.B8, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #50
nSE <- filter(Workflow.B8, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#117
nSSH <- filter(Workflow.B8, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #57

Workflow_Health <- filter(Workflow.B8, TC3=="Health Research") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.B8, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.B8, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

# #### Plot funds source #### 

 ggplot(Workflow_Tri2, aes(x=reorder(answer_n,`%`))) + 
   geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
   scale_fill_manual(values =  cbp1) + 
   coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
   theme_linedraw(base_size = 20) +
   theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
   # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
   xlab("") + 
   ylab("")

### C2 - Do you consider the use of research software critical to your research? ######
survey_C2_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C2) %>% 
  unnest(C2) %>% 
  rename(answer = C2)

sort(unique(survey_C2_v1$answer))# to clean the data

survey_C2_v2 <- 
  survey_C2_v1 %>% 
  mutate(answer_n = ifelse(
    answer == "No", "No", ifelse(
      answer == "Yes", "Yes", ifelse(
        answer == "Not sure", "Not sure", "Other"))))

survey_C2_v3 <- 
  survey_C2_v2 %>% 
  filter(!answer_n == "Other")

#Select other
other <- 
  survey_C2_v2 %>% 
  filter(answer_n == "Other")

# write.csv(other, "C2.csv")

C2_summay <- 
  survey_C2_v3 %>% 
  group_by(answer_n) %>% 
  count()

#Link to TC3
survey_C2_v3_tc3 <- 
  survey_C2_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_C2_tc3 <- 
  survey_C2_v3_tc3 %>% 
  group_by(TC3, answer_n) %>% 
  count() %>% 
  print()


#### Pie chart #### 
PieDonut(C2_summay, 
         aes(answer_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5, 
         pieAlpha = 1, 
         donutAlpha = 1, 
         color = "black",
         pieLabelSize = 7)+ 
  scale_fill_manual(values =  cb_pie)

#### Bar plot - TC3 #### 

ggplot(summary_C2_tc3, aes(fill=TC3, y=n, x=answer_n)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")



### C3 - Do you consider the development of research software a primary output of your research? ######
survey_C3_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C3) %>% 
  unnest(C3) %>% 
  rename(answer = C3)


C3_summay <- 
  survey_C3_v1 %>% 
  group_by(answer) %>% 
  count()

#Link to TC3
survey_C3_v3_tc3 <- 
  survey_C3_v2 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_C3_tc3 <- 
  survey_C3_v3_tc3 %>% 
  group_by(TC3, answer_n) %>% 
  count() %>% 
  print()


#### Pie chart #### 
PieDonut(summary_C3_tc3, 
         aes(answer_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5, 
         pieAlpha = 1, 
         donutAlpha = 1, 
         color = "black",
         pieLabelSize = 7)+ 
  scale_fill_manual(values =  cbp1)

#### Bar plot - TC3 #### 

ggplot(summary_C3_tc3, aes(fill=TC3, y=n, x=answer_n)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")



### C4 - Do you have access to software development support? ######
survey_C4_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C4) %>% 
  unnest(C4) %>% 
  rename(answer = C4)

C4_summay <- 
  survey_C4_v1 %>% 
  group_by(answer) %>% 
  count()

#Link to TC3
survey_C4_v3_tc3 <- 
  survey_C4_v2 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_C4_tc3 <- 
  survey_C4_v3_tc3 %>% 
  group_by(TC3, answer_n) %>% 
  count() %>% 
  print()


#### Pie chart #### 
PieDonut(C4_summay, 
         aes(answer, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5, 
         pieAlpha = 1, 
         donutAlpha = 1, 
         color = "black",
         pieLabelSize = 7)+ 
  scale_fill_manual(values =  cb_pie)

#### Bar plot - TC3 #### 

ggplot(summary_C4_tc3, aes(fill=TC3, y=n, x=answer_n)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")

### C5 - Who provides this support? ######

#C5 is part of of C4: select the C5 questions

survey_C5_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "C5")

#Separate the answers from the question
survey_C5_v2 <- 
  within(
    survey_C5_v1, 
    Question <- 
      data.frame(
        do.call(
          'rbind',
          strsplit(
            as.character(Question), 
            '__', 
            fixed=TRUE))))



#Clean the data
survey_C5_v3 <- 
  survey_C5_v2 %>% 
  drop_na() %>% 
  unnest(Question) %>% 
  select(-X1) %>% 
  mutate(answer_n = ifelse(
    X2 == "_My_institution_", "My institution", ifelse(
      X2 == "_The_Alliance_", "The Alliance", ifelse(
        X2 == "_A_disciplinary_community_", "A disciplinary community", "Paid consultants or professional services"
          )))) %>% 
  select(-X2) %>% 
  filter(Answer == "Yes") %>% 
  select(-Answer) %>% 
  unique()

#Link to TC3
C5.domain <- 
  survey_C5_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() # 

# #summarize the data
# C5_summary <- 
#   C5.domain %>% 
#   group_by(answer_n, TC3) %>% 
#   count() %>% 
#   arrange(-n) %>% 
#   print()


##add percentage
Workflow.C5 <- 
  C5.domain %>% 
  unique()

nHR <- filter(Workflow.C5, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #16
nSE <- filter(Workflow.C5, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#45
nSSH <- filter(Workflow.C5, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #16

Workflow_Health <- filter(Workflow.C5, TC3=="Health Research") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.C5, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.C5, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 

# #TC3
# ggplot(C5_summary, aes(fill=TC3, y=n, x= reorder(answer_n, n))) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_fill_manual(values =  cbp1) + 
#   coord_flip()+
#   # ggtitle("") +
#   guides(fill=guide_legend(title="Tri-agency"))+
#   theme_linedraw(base_size = 20) +
#   theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
#   xlab("")+
#   ylab("n")

#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer_n,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

### C6 - How important is (or would be) such a service to your research? ######
survey_C6_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C6) %>% 
  unnest(C6) %>% 
  rename(answer = C6)

C6_summay <- 
  survey_C6_v2 %>% 
  group_by(answer_n) %>% 
  count()

#Link to TC3
survey_C6_v3_tc3 <- 
  survey_C6_v2 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  mutate(order = ifelse(
    answer_n == "Neutral", 3, ifelse(
      answer_n == "Not important", 4, ifelse(
        answer_n == "Somewhat important", 2, ifelse(
          answer_n == "Very important", 1, 5
        ))))) %>%
  drop_na()

# #summarize the data
# summary_C6_tc3 <- 
#   survey_C6_v3_tc3 %>% 
#   group_by(TC3, answer_n) %>% 
#   count() %>% 
#   print()


##add percentage
Workflow.C6 <- 
  survey_C6_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.C6, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #24
nSE <- filter(Workflow.C6, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#49
nSSH <- filter(Workflow.C6, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #29

Workflow_Health <- filter(Workflow.C6, TC3=="Health Research") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.C6, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.C6, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 

# #TC3
# ggplot(summary_C6_tc3, aes(fill=TC3, y=n, x=reorder(answer_n, -order))) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_fill_manual(values =  cbp1) + 
#   coord_flip() +
#   # ggtitle("") +
#   guides(fill=guide_legend(title="Tri-agency"))+
#   theme_linedraw(base_size = 20) +
#   theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
#   xlab("")+
#   ylab("n")

#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer_n,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### C11 - Are there particular software platforms or software services you currently use which you feel would be valuable to be offered as a national service for all researchers to access? ######
survey_C11_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C11) %>% 
  unnest(C11)

C11_summay <- 
  survey_C11_v2 %>% 
  group_by(C11_n) %>% 
  count()

#### Pie chart #### 
PieDonut(C11_summay, 
         aes(C11_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5, 
         pieAlpha = 1, 
         donutAlpha = 1, 
         color = "black",
         pieLabelSize = 7)+ 
  scale_fill_manual(values =  cbp1)


### C12 - Would you be interested in sharing success stories with the Alliance? If yes, please provide your contact information and the URL of the software platform. ######
survey_C12_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C12) %>% 
  unnest(C12)

sort(unique(survey_C12_v1$C12))# to clean the data

survey_C12_v2 <- 
  survey_C12_v1 %>% 
  mutate(C12_n = ifelse(
    C12 == "Yes", "Yes", ifelse(
      C12 == "No", "No", "Other")))

survey_C12_v3 <- 
  survey_C12_v2 %>% 
  filter(!C12_n == "Other") %>% 
  unique() 

#select the shared success stories
other <- 
  survey_C12_v2 %>% 
  filter(C12_n == "Other")

# write.csv(SS, "C12.csv")

C12_summay <- 
  survey_C12_v3 %>% 
  group_by(C12_n) %>% 
  count()

#### Pie chart #### 
PieDonut(C12_summay, 
         aes(C12_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5, 
         pieAlpha = 1, 
         donutAlpha = 1, 
         color = "black",
         pieLabelSize = 7)+ 
  scale_fill_manual(values =  cbp1)


### D2 - Have you or your team received funding from a funding call that is specific to the development of research software (e.g. CANARIE Research Software, CFI Cyberinfrastructure Challenge I, Chan Zuckerberg Open Source Software)? ######
survey_D2_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D2) %>% 
  unnest(D2)

sort(unique(survey_D2_v1$D2))# to clean the data

survey_D2_v2 <- 
  survey_D2_v1 %>% 
  mutate(D2_n = ifelse(
    D2 == "Je ne sais pas", "Not sure", ifelse(
      D2 == "Non", "No", ifelse(
        D2 == "Oui", "Yes", D2
      ))))

D2_summay <- 
  survey_D2_v2 %>% 
  group_by(D2_n) %>% 
  count()

#### Pie chart #### 
PieDonut(D2_summay, 
         aes(D2_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5, 
         pieAlpha = 1, 
         donutAlpha = 1, 
         color = "black",
         pieLabelSize = 7)+ 
  scale_fill_manual(values =  cb_pie)

### D3 - From which funding agencies? ######
survey_D3_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D3) %>% 
  unnest(D3) %>% 
  unique()

sort(unique(survey_D3_v1$D3))# to clean the data

# write.csv(survey_D3_v1, "D3.csv")

D3_other <- read.csv("D3.csv") %>% 
  group_by(D3_n) %>% count() %>% 
  drop_na()

#### Bar plot #### 
ggplot(D3_other, aes(x= reorder(D3_n, n), y=n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal(base_size = 20)+
  xlab("Funding agency") + 
  ylab("n")

### D4 - How many years of research software development experience do you have? ######
survey_D4_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D4) %>% 
  unnest(D4) %>% 
  unique()

#Link Domain - D4 and B3 (role)

D4_B3 <- 
  survey_D4_v1 %>% 
  left_join(survey_B3_v3, by = "Internal.ID")

D4_B3_domain <- 
  D4_B3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()
  
#summary table - only experience (D4)
D4_summary <- 
  survey_D4_v1 %>% 
  group_by(D4) %>% 
  count() %>% 
  print()

#summary table - D4 and Role
D4_B3_summary <- 
  D4_B3_domain %>% 
  group_by(D4, Role_n) %>% 
  count() %>% 
  mutate(Role_n = ifelse(
    Role_n == "Faculty - Professor (including assistant/associate/full professor, clinical professor, teaching professor)", "Faculty - Professor", Role_n
  )) %>% 
  print()
  
#summary table - D4 and TC3
D4_TC3_summary <- 
  D4_B3_domain %>% 
  group_by(D4, TC3) %>% 
  count() %>% 
  print()

#### Bar plot #### 

#Only D4
ggplot(D4_summary, aes(x= reorder(D4, n), y=n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal(base_size = 20)+
  xlab("Years of research software development experience") + 
  ylab("n")

#D4 + B3 (roles) 
ggplot(D4_B3_summary, aes(fill=Role_n, y=n, x= reorder(D4, as.numeric(D4)))) + 
  geom_bar(position="stack", stat="identity") +
  # scale_fill_manual(values =  cbp_Cad) + 
  # ggtitle("") +
  guides(fill=guide_legend(title="Roles"))+
  theme_linedraw(base_size = 20) +
  coord_flip() +
  theme(legend.position = "right", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("Years of research software development experience")+
  ylab("n")

#D4 + TC3
ggplot(D4_TC3_summary, aes(fill=TC3, y=n, x= reorder(D4, as.numeric(D4)))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("Years of research software development experience")+
  ylab("n")

### D5 - What roles do you as an individual play on your software development team? ######
survey_D5_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D5")

#When there is "Yes" it means the responded selected that "answer"
survey_D5_v2 <- 
  within(
    survey_D5_v1, 
    Question <- 
      data.frame(
        do.call(
          'rbind',
          strsplit(
            as.character(Question), 
            '__', 
            fixed=TRUE))))



#Clean the data
survey_D5_v3 <- 
  survey_D5_v2 %>% 
  drop_na() %>% 
  unnest(Question) %>% 
  select(-X1) %>% 
  mutate(answer_n = ifelse(
    X2 == "_Design_", "Design", ifelse(
      X2 == "_Development_", "Development", ifelse(
        X2 == "_Documentation_", "Documentation", ifelse(
          X2 == "_Outreach_", "Outreach", ifelse(
            X2 == "_Funding_", "Funding", ifelse(
              X2 == "_Quality_Assurance_", "Quality Assurance", ifelse(
                X2 == "_Researcher_End_User_", "Researcher/End-User", "Other"
                )))))))) %>% 
  select(-X2) %>% 
  filter(!Answer == "No")

#Link to TC3
D5.domain <- 
  survey_D5_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() # 

#summary table - D5 and TC3
D5_summary <- 
  D5.domain %>% 
  group_by(answer_n, TC3) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#Link Domain - D5 and B3 (role)
D5_B3 <- 
  survey_D5_v3 %>% 
  left_join(survey_B3_v3, by = "Internal.ID") %>% 
  drop_na() %>% 
  select(-Ques_num) %>% 
  unique()

# #to use to reorder the bars for roles
# order <- 
#   D5_B3 %>% 
#   group_by(answer_n) %>% 
#   count() %>% 
#   rename(order = n) %>% 
#   print()
  
#summary table - D4 and Role
D5_B3_summary <- 
  D5_B3 %>% 
  group_by(answer_n, Role_n) %>% 
  count()  %>% 
  mutate(Role_n = ifelse(
    Role_n == "Faculty - Professor (including assistant/associate/full professor, clinical professor, teaching professor)", "Faculty - Professor", Role_n
  )) %>% 
  left_join(order, by = "answer_n") %>% 
  print()

#select "Others"
other <- 
  survey_D5_v3 %>% 
  filter(answer_n == "Other") %>% 
  drop_na() %>% 
  unique() %>% 
  print()

# write.csv(other, "D5.csv")

##add percentage
Workflow.D5 <- 
  D5.domain %>% 
  unique()

nHR <- filter(Workflow.D5, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #26
nSE <- filter(Workflow.D5, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#80
nSSH <- filter(Workflow.D5, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #28

Workflow_Health <- filter(Workflow.D5, TC3=="Health Research") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D5, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D5, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plot #### 

# #D5 + TC3
# ggplot(D5_summary, aes(fill=TC3, y=n, x= reorder(answer_n, n))) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_fill_manual(values =  cbp1) + 
#   coord_flip()+
#   # ggtitle("") +
#   guides(fill=guide_legend(title="Tri-agency"))+
#   theme_linedraw(base_size = 20) +
#   theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
#   xlab("")+
#   ylab("n")


#D5 + B3 (roles) 
ggplot(D5_B3_summary, aes(fill=Role_n, y=n, x= reorder(answer_n, order))) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()+
  # scale_fill_manual(values =  cbp_Cad) + 
  # ggtitle("") +
  guides(fill=guide_legend(title="Role"))+
  theme_linedraw(base_size = 20) +
  coord_flip() +
  theme(legend.position = "right", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("Years of research software development experience")+
  ylab("n")

#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer_n,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

### D6 - Who develops the software in your group? ######
survey_D6_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D6") %>% 
  mutate(answer_n = ifelse(
    Question == "Who_develops_the_software_in_your_group___Faculty___Professor__including_assistant_associate_full_professor__clinical_professor__teaching_professor__", "Faculty - Professor", ifelse(
      Question == "Who_develops_the_software_in_your_group___Faculty___Adjunct__emeritus__visiting__or_limited_term_" , "Faculty - Adjunct, emeritus, visiting, or limited-term", ifelse(
        Question == "Who_develops_the_software_in_your_group___Administrator__", "Administrator", ifelse(
          Question == "Who_develops_the_software_in_your_group___Post_Doctoral_Fellow___", "Post Doctoral Fellow", ifelse(
            Question == "Who_develops_the_software_in_your_group___Research_Software_Engineer___Expert_", "Research Software Engineer Expert", ifelse(
              Question == "Who_develops_the_software_in_your_group___Research_Associate__"  , "Research Associate", ifelse(
                Question == "Who_develops_the_software_in_your_group___Research_Staff__", "Research Staff", ifelse(
                  Question == "Who_develops_the_software_in_your_group___Student__Doctoral__", "Student Doctoral", ifelse(
                    Question == "Who_develops_the_software_in_your_group___Student__Masters___", "Student Masters", ifelse(
                      Question == "Who_develops_the_software_in_your_group___Student__Undergrad___", "Student Undergrad", ifelse(
                        Question == "Who_develops_the_software_in_your_group___Researcher_", "Researcher", ifelse(
                          Question == "Who_develops_the_software_in_your_group___Librarian_" , "Librarian", ifelse(
                            Question == "Who_develops_the_software_in_your_group___Other_", "Other", ifelse(
                              Question == "Who_develops_the_software_in_your_group___Research_Software_Developer_", "Research Software Developer", "?"
                              ))))))))))))))) %>% 
  select(-Question)

#Clean the data
survey_D6_v2 <- 
  survey_D6_v1 %>% 
  drop_na() %>% 
  unnest(answer_n) %>% 
  select(-Ques_num)

survey_D6_v3 <- 
  survey_D6_v2 %>% 
  filter(!Answer == "No") %>% #To not select "Other"
  unique() %>% 
  print()

#Link to TC3
D6.domain <- 
  survey_D6_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() # 

# #summary table - D6 and TC3
# D6_summary <- 
#   D6.domain %>% 
#   group_by(answer_n, TC3) %>% 
#   count() %>% 
#   arrange(-n) %>% 
#   print()

#select "Others"
other <- 
  survey_D6_v2 %>% 
  filter(answer_n == "Other") %>% 
  drop_na() %>% 
  unique() %>% 
  print()

# write.csv(other, "D6.csv")

##add percentage
Workflow.D6 <- 
  D6.domain %>% 
  unique()

nHR <- filter(Workflow.D6, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #26
nSE <- filter(Workflow.D6, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#79
nSSH <- filter(Workflow.D6, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #28

Workflow_Health <- filter(Workflow.D6, TC3=="Health Research") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D6, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D6, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 
# ggplot(D6_summary, aes(fill=TC3, y=n, x= reorder(answer_n, n))) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_fill_manual(values =  cbp1) + 
#   coord_flip()+
#   # ggtitle("") +
#   guides(fill=guide_legend(title="Tri-agency"))+
#   theme_linedraw(base_size = 20) +
#   theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
#   xlab("")+
#   ylab("n")

#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer_n,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

### D7 - Can you provide an estimate of the time you and your research team spend developing research software? ######
survey_D7_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D7") %>% 
  mutate(question_n = ifelse(
    Question == "Can_you_provide_an_estimate_of_the_time_you_and_your_research_team_spend_developing_research_software___The_time_you_spend_developing_research_software___", "You", "Team"
  )) %>% 
  select(-Question)
    
#Clean the data
survey_D7_v2 <- 
  survey_D7_v1 %>% 
  drop_na() %>% 
  unnest(Answer) %>% 
  select(-Ques_num)

#"Team" data
D7_team <- 
  survey_D7_v2 %>% 
  filter(question_n == "Team")

#"You" data
D7_you <- 
  survey_D7_v2 %>% 
  filter(question_n == "You")

#Link "Team" to TC3
D7_team.domain <- 
  D7_team %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() 

#Link "You" to TC3
D7_you.domain <- 
  D7_you %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() 

#Link "You" to B3 (role)
D7_you_role <- 
  D7_you %>% 
  left_join(survey_B3_v3, by = "Internal.ID") %>% 
  drop_na() %>% 
  select(-question_n) %>%
  unique()

##Summarize tables
#summary table - "Team" and TC3
D7_team_summary <- 
  D7_team.domain %>% 
  group_by(Answer, TC3) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of an FTE", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
          ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", ">2", ifelse(
      Answer == "Between 1 and 2 FTEs", "1 - 2", ifelse(
        Answer == "Less than 1/4 of an FTE", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  print()


#summary table - "You" and TC3
D7_you_summary <- 
  D7_you.domain %>% 
  group_by(Answer, TC3) %>% 
  count() %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of an FTE", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", ">2", ifelse(
      Answer == "Between 1 and 2 FTEs", "1 - 2", ifelse(
        Answer == "Less than 1/4 of an FTE", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  arrange(-n) %>% 
  print()

#Mirror ="Team and "you"
survey_D7_v3 <- survey_D7_v2
survey_D7_v3$question_n[survey_D7_v3$question_n == "You"] <- "Respondant"
survey_D7_v3$question_n[survey_D7_v3$question_n == "X35"] <- "Research team"

#link to TC3
survey_D7_v3_TC3 <- 
  survey_D7_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() 

#summarize
survey_D7_v3_TC3.summary <- 
  survey_D7_v3_TC3 %>% 
  group_by(question_n, Answer) %>% 
  count() %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of an FTE", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", ">2", ifelse(
      Answer == "Between 1 and 2 FTEs", "1 - 2", ifelse(
        Answer == "Less than 1/4 of an FTE", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>%  
  drop_na()

#sum
survey_D7_v3_TC3.sum <- 
  survey_D7_v3_TC3.summary %>% 
  group_by(question_n) %>% 
  summarise(sum = sum(n))

#merge sum table to summary table

survey_D7_v3_TC3.merged <- 
  survey_D7_v3_TC3.summary %>% 
  left_join(survey_D7_v3_TC3.sum, by = "question_n") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
survey_D7_v3_TC3.sum.flip <- survey_D7_v3_TC3.merged %>% 
  mutate(new_n = ifelse(question_n == "Respondant",
                        -1*proportion, proportion))


#### Bar plots#### 

#"Team" and TC3
ggplot(D7_team_summary, aes(fill=TC3, y=n, x= reorder(answer_n, order))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")

#"You" and TC3
ggplot(D7_you_summary, aes(fill=TC3, y=n, x= reorder(answer_n, order))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")

#Mirror bar plots
ggplot(survey_D7_v3_TC3.sum.flip, aes(fill=question_n, y=new_n, x=reorder(Answer, -order))) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("Commercial cloud vs Alliance Community cloud")+
  scale_fill_manual(values =  cb_pie2)+
  guides(fill=guide_legend(title="Group"))+
  ylim(-100,100)



### D8 - Approximately how many dollars (CDN) of research funds did your research group spend over the last calendar year on developing research software resources? ######
survey_D8_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D8) %>% 
  unnest(D8) %>% 
  rename(answer = D8) %>% 
  filter(!answer == "Other")

#Link to TC3
survey_D8_v3_tc3 <- 
  survey_D8_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() %>% 
  mutate(order = ifelse(
    answer == "$0 (None)", 1, ifelse(
      answer == "$1 to less than $1000", 2, ifelse(
        answer == "$1000 to less than $5000", 3, ifelse(
          answer == "$5000 to less than $50K", 4, ifelse(
            answer == "$50K to less than $100K", 5, ifelse(
              answer == "$100K to less that $250K", 6, ifelse(
                answer == "More than $250K", 7, 8
              )))))))) %>% 
  print()

#summarize the data
summary_D8_tc3 <- 
  survey_D8_v3_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()


##add percentage
Workflow.D8 <- 
  survey_D8_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.D8, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #26
nSE <- filter(Workflow.D8, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#76
nSSH <- filter(Workflow.D8, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #31

Workflow_Health <- filter(Workflow.D8, TC3=="Health Research") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D8, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D8, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plot - TC3 #### 

# ggplot(summary_D8_tc3, aes(fill=TC3, y=n, x=reorder(answer, -order))) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_fill_manual(values =  cbp1) + 
#   # coord_flip() +
#   # ggtitle("") +
#   guides(fill=guide_legend(title="Tri-agency"))+
#   theme_linedraw(base_size = 20) +
#   theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
#   xlab("")+
#   ylab("n")

#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  # coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")
