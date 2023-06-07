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


### B2 - Please choose your primary research domain based on the Canadian Research and Development Classification (CRDC) 2020. ######
Domain_Breakdown<- 
  survey_organized_spread %>% 
  select(Internal.ID, B2) %>% 
  unnest(B2) %>% 
  rename(Domain = B2)

#summarise the data = count domain's n
domain_summary <- 
  Domain_Breakdown %>% 
  group_by(Domain) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#### Group domains into TC3 ####
domain_summary1 <- 
  domain_summary %>% 
  mutate(TC3 = ifelse(
    Domain == "Natural sciences", "Sciences and Engineering", ifelse(
      Domain == "Sciences naturelles", "Sciences and Engineering", ifelse(
        Domain == "Medical, health and life sciences", "Health Research", ifelse(
          Domain == "Sciences medicales, de la sante et de la vie", "Health Research", ifelse(
            Domain == "Engineering and technology", "Sciences and Engineering", ifelse(
              Domain == "Genie et technologies", "Sciences and Engineering", ifelse(
                Domain ==  "Humanities and the arts", "Social Sciences and Humanities", ifelse(
                  Domain == "Sciences humaines et arts", "Social Sciences and Humanities", ifelse(
                    Domain == "Sciences sociales", "Social Sciences and Humanities", ifelse(
                      Domain == "Social sciences", "Social Sciences and Humanities", ifelse(
                        Domain == "Agricultural and veterinary sciences", "Sciences and Engineering", ifelse(
                          Domain == "Sciences agricoles et veterinaires", "Sciences and Engineering", "X"
                          )))))))))))))

#Link TC3 to Internal.ID = this will be used for the rest of the analysis as we will analyse data by TC3
domain <- 
  Domain_Breakdown %>% 
  left_join(domain_summary1, by = "Domain") %>% 
  select(-n) # n = 518

domain1 <- 
  domain %>% 
  select(-Domain)

b2.domain <- domain

#group by domain
b2.domain.summary <- 
  b2.domain %>% 
  group_by(TC3) %>% count() %>% drop_na()

# #for esthethis purposes, we add "\n" to long TC3 names and to domains so they the names will fully appear in the pie charts
q3.domain.summary$TC3[q3.domain.summary$TC3 == "Social Sciences and Humanities"] <- "Social Sciences\nand Humanities"
q3.domain.summary$TC3[q3.domain.summary$TC3 == "Sciences and Engineering"] <- "Sciences and\nEngineering"
domain_summary1$Domain[domain_summary1$Domain == "Medical, Health and Life Sciences "] <- "Medical, Health\nand Life Sciences"

#### Pie charts ####


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
  rename(Role = B3)


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
  select(Internal.ID, Role_n)

#### summary table - Roles ####
roles_summary <-
  survey_B3_v3 %>%
  group_by(Role_n) %>%
  count() %>%
  arrange(-n) %>%
  drop_na() %>%
  print()


#### Pie charts ####
PieDonut(roles_summary, 
         aes(Role_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.25,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Respondents' roles", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cbp_Cad) #+ 

### B7 - Are you eligible to apply for and receive Tri-Council, CFI, or other research funding? ######
survey_B7_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B7) %>% 
  unnest(B7)

B7_summay <- 
  survey_B7_v1 %>% 
  group_by(B7_n) %>% 
  count()

#### Pie chart #### 
PieDonut(B7_summay, 
         aes(B7_n, count= n), 
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
  mutate(Answer_n = ifelse(
    X2 == "_My_institution_", "My institution", ifelse(
      X2 == "_The_Alliance_", "The Alliance", ifelse(
        X2 == "_A_disciplinary_community_", "A disciplinary community", "Paid consultants or professional services"
          )))) %>% 
  select(-X2)

#Link to TC3
C5.domain <- 
  survey_C5_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() # n = 1280

#Select only rows with Yes (as this is linked to "Yes")
C5.domain.yes <- 
  C5.domain %>% 
  filter(Answer == "Yes")

#summarize the data
C5_summary <- 
  survey_C5_v3 %>% 
  group_by(Answer_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()

### C6 - How important is (or would be) such a service to your research? ######
survey_C6_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C6) %>% 
  unnest(C6) %>% 
  rename(answer = C6)

sort(unique(survey_C6_v1$answer))# to clean the data

survey_C6_v2 <- 
  survey_C6_v1 %>% 
  mutate(answer_n = ifelse(
    answer == "Tres important", "Very important", ifelse(
      answer == "Un peu important", "Somewhat important", ifelse(
        answer == "Neutre", "Neutral", ifelse(
          answer == "Pas important", "Not important", ifelse(
            answer == "Ne s'applique pas", "It doesn't matter",
            )))))) %>% 
  select(-answer)


C6_summay <- 
  survey_C6_v2 %>% 
  group_by(answer_n) %>% 
  count()

#Link to TC3
survey_C6_v3_tc3 <- 
  survey_C6_v2 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_C6_tc3 <- 
  survey_C6_v3_tc3 %>% 
  group_by(TC3, answer_n) %>% 
  count() %>% 
  mutate(order = ifelse(
    answer_n == "Neutral", 3, ifelse(
      answer_n == "Not important", 4, ifelse(
        answer_n == "Somewhat important", 2, ifelse(
          answer_n == "Very important", 1, 5
          ))))) %>% 
  print()


#### Bar plot - TC3 #### 

ggplot(summary_C6_tc3, aes(fill=TC3, y=n, x=reorder(answer_n, -order))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")


### C11 - Are there particular software platforms or software services you currently use which you feel would be valuable to be offered as a national service for all researchers to access? ######
survey_C11_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C11) %>% 
  unnest(C11)

sort(unique(survey_C11_v1$C11))# to clean the data

survey_C11_v2 <- 
  survey_C11_v1 %>% 
  mutate(C11_n = ifelse(
    C11 == "Oui", "Yes", ifelse(
      C11 == "Non", "No", C11
      )))

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
    C12 == "Oui", "Yes", ifelse(
      C12 == "Non", "No", ifelse(
        C12 == "Yes", "Yes", ifelse(
          C12 == "No", "No", "Other"
          )))))

survey_C12_v3 <- 
  survey_C12_v2 %>% 
  filter(!C12_n == "Other") %>% 
  unique() # because of duplications

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

write.csv(survey_D3_v1, "D3.csv")
