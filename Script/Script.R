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

#This to be used to plot general Histograms                    
cbp_Cad <- rep(c("#B7B6B3", "#D6AB00","#00DBA7", "#56B4E9",
                 "#32322F", "darkturquoise", "#D55E00", "#CC79A7",
                 "green4", "lightslategrey"), 100)

cbp_Cad1 <- rep(c("#B7B6B3", "#D6AB00","#00DBA7", "#56B4E9",
                 "#32322F", "darkturquoise", "#D55E00", "#CC79A7",
                 "green4", "lightslategrey", "#B7A6B3", "#32325F",
                 "#D6DB00", "#A55E00", "red"), 100)

# This to be used for yes, no, not sure                 
cb_pie <- rep(c("#32322F","#FBFAFA", "#D6AB00"), 100)
cb_pie2 <- rep(c("#FBFAFA","#32322F", "#D6AB00"), 100)
# This is for mirror plots
cb_pie1 <- rep(c("#32322F", "#D6AB00"), 100)

#For likert graphs
likert_color <- c("#B2182B", "#F4A582", "#d3d3d3", "#92C5DE", "#2166AC")
likert_color1 <- c("#2166AC", "#92C5DE", "#d3d3d3","#F4A582", "#B2182B")

# A ############################################################################################
### A1 - Select the option that best describes your gender identity. ######
survey_A1_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "A1")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_A1_v1$Question),'____',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_A1_v2 <- cbind(separete_v3, survey_A1_v1)
survey_A1_v2 <- 
  survey_A1_v2 %>% 
  drop_na() %>% 
  filter(Answer == "Yes")

#Clean the data
survey_A1_v3 <- 
  survey_A1_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Gender fluid_", "Gender fluid", ifelse(
      Answer_q == " Man_", "Man", ifelse(
        Answer_q == "Non binary_", "Non binary", ifelse(
          Answer_q == "Two Spirit_", "Two Spirit", ifelse(
            Answer_q == "Woman ", "Woman", "Prefer not to answer"
            )))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#summarize
A1_summary <- 
  survey_A1_v3 %>% 
  group_by(answer) %>% 
  count() %>% 
  arrange(n) %>% 
  print()

#### Pie charts ####

PieDonut(A1_summary, 
         aes(answer, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Respondents' roles", 
         titlesize = 5,
         pieLabelSize = 7)+
  scale_fill_manual(values =  cbp_Cad)





### A2 - Do you identify as transgender? ######
survey_A2_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, A2) %>% 
  unnest(A2)

A2_summay <- 
  survey_A2_v1 %>% 
  group_by(A2) %>% 
  count()

#### Pie chart #### 
PieDonut(A2_summay, 
         aes(A2, count= n), 
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

### A3 - What type of research software do you develop? [refer to research software types from research software current state report] ######
survey_A3_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "A3") %>% 
  rename(A3 = Answer) %>% 
  drop_na()

A3_summay <- 
  survey_A3_v1 %>% 
  group_by(A3) %>% 
  count() %>% 
  filter(!A3 == "This does not seem to be relevant to the use of research software.")

#### Pie chart #### 
PieDonut(A3_summay, 
         aes(A3, count= n), 
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

ggplot(A3_summay, aes(y=n, x=reorder(A3, n))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip()+
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")

### A4 - Do you identify as Indigenous? ######
survey_A4_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, A4) %>% 
  unnest(A4)

A4_summay <- 
  survey_A4_v1 %>% 
  group_by(A4) %>% 
  count()

#### Pie chart #### 
PieDonut(A4_summay, 
         aes(A4, count= n), 
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


### A6 - Do you identify as a member of a racialized group* in Canada? ######
survey_A6_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, A6) %>% 
  unnest(A6)

A6_summay <- 
  survey_A6_v1 %>% 
  group_by(A6) %>% 
  count()

#### Pie chart #### 
PieDonut(A6_summay, 
         aes(A6, count= n), 
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


### A7 - How do you identify your race/ethnicity? ######
survey_A7_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "A7")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_A7_v1$Question),'y_',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X3)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X3, "_", " ")

#Bin tables
survey_A7_v2 <- cbind(separete_v3, survey_A7_v1)

#Clean the data
survey_A7_v3 <- 
  survey_A7_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == " _Black_African__including_African_Canadian_American__Caribbean__", "Black/African", ifelse(
      Answer_q == " _East_Asian__e_g___Chinese__Taiwanese__Japanese__Korean__", "East Asian" , ifelse(
        Answer_q ==  " _Indo_Caribbean__Indo_African__Indo_Fijian__or_West_Indian_",  "Indo-Caribbean, Indo- African, Indo-Fijian, or West-Indian", ifelse(
          Answer_q == " _Latin__South__or_Central_American_", "Latin, South, or Central American", ifelse(
            Answer_q == " _Pacific_Islanders_or_Polynesian_Melanesian_Micronesian__e_g___Cook_Island_Ma_ori__Hawaiian_Ma__oli__Fijians__Marquesan__Marshallese__Niuean__Samoans__Tahitian_Ma__ohi__Tongan__New_Zealand_Ma_ori__", "Pacific Islanders or Polynesian/Melanesian/Micronesiani)", ifelse(
              Answer_q == " _South_Asian__e_g___Bangladeshi__Pakistani__Indian__Sri_Lankan__Punjabi__", "South Asian .(e.g. Bangladeshi, Pakistani, Indian, Sri Lankan, Punjabi)", ifelse(
                Answer_q == " _South_East_Asian__e_g___Cambodian__Filipino_a__Malaysian__Thai__Vietnamese__", "South East Asian (e.g. Cambodian, Filipino/a, Malaysian, Thai, Vietnamese)", ifelse(
                  Answer_q == " _West_Asian_or_North_African__e_g___Afghani__Armenian__Egyptian__Iranian__Iraqi__Israeli__Jordanian__Lebanese__Palestinian__Syrian__Yemeni___", "West Asian or North African", ifelse(
                    Answer_q == " _Prefer_not_to_answer__", "Prefer not to answer", "Other" )))))))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#summarize the data
summary_A7<- 
  survey_A7_v3 %>% 
  group_by(answer) %>% 
  count() %>% 
  print()

#### Plot #### 
ggplot(summary_A7, aes(y=n, x=reorder(answer, n))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip()+
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")

### A8 - Did you immigrate to Canada? ######
survey_A8_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, A8) %>% 
  unnest(A8)

A8_summay <- 
  survey_A8_v1 %>% 
  group_by(A8) %>% 
  count()

#### Pie chart #### 
PieDonut(A8_summay, 
         aes(A8, count= n), 
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

### A9 - For how many years have you lived in Canada? ######
survey_A9_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, A9) %>% 
  unnest(A9)

A9_summay <- 
  survey_A9_v1 %>% 
  group_by(A9) %>% 
  count()


### A10 - Do you identify as a person with a disability or as requiring accommodations in the workplace due to a functional limitation?* ######
survey_A10_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, A10) %>% 
  unnest(A10)

#Summarize
A10_summay <- 
  survey_A10_v1 %>% 
  group_by(A10) %>% 
  count()

#### Pie chart #### 
PieDonut(A10_summay, 
         aes(A10, count= n), 
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



### A11 - Select all the type(s) of disability that applies to you. ######
survey_A11_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "A11")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_A11_v1$Question),'u___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bind tables
survey_A11_v2 <- cbind(separete_v3, survey_A11_v1)

survey_A11_v2 <- 
  survey_A11_v2 %>% 
  filter(Answer == "Yes")

#Clean data
survey_A11_v3 <- 
  survey_A11_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Select_all_the_type_s__of_disability_that_applies_to_you___Communications_", "Communications", ifelse(
      Answer_q == "Select_all_the_type_s__of_disability_that_applies_to_you___Developmental_", "Developmental" , ifelse(
        Answer_q ==  "Select_all_the_type_s__of_disability_that_applies_to_you___Dexterity_",  "Dexterity", ifelse(
          Answer_q == "Select_all_the_type_s__of_disability_that_applies_to_you___Flexibility_", "Flexibility", ifelse(
            Answer_q == "Select_all_the_type_s__of_disability_that_applies_to_you___Hearing_", "Hearing", ifelse(
              Answer_q == "", "", ifelse(
                Answer_q == "", "", ifelse(
                  Answer_q == "", "", ifelse(
                    Answer_q == "", "", ifelse(
                      Answer_q == "", "", ifelse(
                        Answer_q == "", "",""
                        )))))))))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#summarize the data
summary_A11<- 
  survey_A11_v3 %>% 
  group_by(answer) %>% 
  count() %>% 
  print()

#### Plot #### 
ggplot(summary_A11, aes(y=n, x=reorder(answer, n))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip()+
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("")+
  ylab("n")

### B ############################################################################################
### B1 - What is your primary institutional affiliation? ######
survey_B1_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B1) %>% 
  unnest(B1) %>% 
  rename(Affiliation = B1) # n = 344

sort(unique(survey_B1_v1$Affiliation))# to clean the data

#Clean data
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

#Edit names
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

#"Domain1" is used to group some questions by Tri-agency (TC3)
domain1 <- 
  domain %>% 
  select(-Domain)

#group by domain
b2.domain.summary <- 
  domain %>% 
  group_by(TC3) %>% count() %>% drop_na()

# #for esthethis purposes, we add "\n" to long TC3 names and to domains so they the names will fully appear in the pie charts
b2.domain.summary$TC3[b2.domain.summary$TC3 == "Social Sciences and Humanities"] <- "Social Sciences\nand Humanities"
b2.domain.summary$TC3[b2.domain.summary$TC3 == "Sciences and Engineering"] <- "Sciences and\nEngineering"
#### Pie charts ####

#All domains
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
#Group the roles
survey_B3_v4.1 <- 
  survey_B3_v3 %>% 
  filter(!Role == "Other") %>% 
  mutate(Role_n = ifelse(
    Role == "staff in research support unit", "Other", ifelse(
      Role == "Personnel de soutien", "Other", ifelse(
        Role == "Bioinformatics", "Other", ifelse(
          Role == "Program Manager", "Administrator", ifelse(
            Role == "Director", "Other", ifelse(
              Role == "Community Manager", "Other", ifelse(
                Role == "User Support Analyst", "Other", ifelse(
                  Role == "Sysadmin", "Other", ifelse(
                    Role == "Data Steward", "Other", ifelse(
                      Role == "Conseillere a la recherche", "Other", ifelse(
                        Role == "Software Developer", "Research Software Developer", ifelse(
                          Role == "Emeritus", "Faculty/Librarian", ifelse(
                            Role == "DG CCTT affilie cegep Ste-Foy", "Other", ifelse(
                              Role == "IT", "Administrator", ifelse(
                                Role == "project manager", "Administrator", ifelse(
                                  Role == "Faculty - Adjunct, emeritus, visiting, or limited-term", "Faculty/Librarian", ifelse(
                                    Role == "Faculty - Professor (including assistant/associate/full professor, clinical professor, teaching professor)", "Faculty/Librarian", ifelse(
                                      Role == "Librarian", "Faculty/Librarian", ifelse(
                                        Role == "Research Software Engineer / Expert", "Research Software Developer", ifelse(
                                          Role == "Research Associate", "Researcher", ifelse(
                                            Role == "Research Staff", "Researcher", ifelse(
                                              Role == "Post-Doctoral Fellow", "Researcher", ifelse(
                                                Role == "Student (Graduate)", "Student", ifelse(
                                                  Role == "Student (Undergrad)", "Student", ifelse(
                                                    Role == "Student (Doctoral)", "Student", Role
                                                    ))))))))))))))))))))))))))

#Roles not grouped
survey_B3_v4.2 <- 
  survey_B3_v3 %>% 
  filter(!Role == "Other") %>% 
  mutate(Role_n = ifelse(
    Role == "staff in research support unit", "Other", ifelse(
      Role == "Personnel de soutien", "Other", ifelse(
        Role == "Bioinformatics", "Other", ifelse(
          Role == "Program Manager", "Administrator", ifelse(
            Role == "Director", "Other", ifelse(
              Role == "Community Manager", "Other", ifelse(
                Role == "User Support Analyst", "Other", ifelse(
                  Role == "Sysadmin", "Other", ifelse(
                    Role == "Data Steward", "Other", ifelse(
                      Role == "Conseillere a la recherche", "Other", ifelse(
                        Role == "Software Developer", "Research Software Developer", ifelse(
                          Role == "Emeritus", "Faculty", ifelse(
                              Role == "IT", "Administrator", ifelse(
                                Role == "project manager", "Administrator", ifelse(
                                  Role == "Faculty - Adjunct, emeritus, visiting, or limited-term", "Faculty", ifelse(
                                    Role == "Faculty - Professor (including assistant/associate/full professor, clinical professor, teaching professor)", "Faculty - Professor", ifelse(
                                      Role == "DG CCTT affilie cegep Ste-Foy", "Other", Role
                                      ))))))))))))))))))

### summary table - Roles grouped 
roles_summary.1 <-
  survey_B3_v4.1 %>%
  group_by(Role_n) %>%
  count() %>%
  arrange(-n) %>%
  drop_na() %>%
  print()

### summary table - Roles not grouped
roles_summary.2 <-
  survey_B3_v4.2 %>%
  group_by(Role_n) %>%
  count() %>%
  arrange(-n) %>%
  drop_na() %>%
  print()

roles_summary.1$Role_n[roles_summary.1$Role_n == "Research Software Developer"] <-  "Research Software\nDeveloper"


#### Pie chart ####

#Grouped
PieDonut(roles_summary.1,
         aes(Role_n, count= n),
         ratioByGroup = FALSE,
         showPieName=FALSE,
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1,
         showRatioThreshold = F,
         title= "Respondents' roles",
         titlesize = 7,pieLabelSize =7,  pieAlpha = 1, donutAlpha = 1, color = "black")+
  scale_fill_manual(values =  cbp_Cad) 

#Note grouped
PieDonut(roles_summary.2,
         aes(Role_n, count= n),
         ratioByGroup = FALSE,
         showPieName=FALSE,
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1,
         showRatioThreshold = F,
         title= "Respondents' roles",
         titlesize = 7, pieAlpha = 1, donutAlpha = 1, color = "black")+
  scale_fill_manual(values =  cbp_Cad1)

### B4 - Please provide the number of years since your first academic appointment. ######
survey_B4_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B4) %>% 
  unnest(B4)

#Clean data as we have answers in number of years (e.g., 27), and the year (e.g., 1997)
survey_B4_v2 <- 
  survey_B4_v1 %>% 
  mutate(B4 = as.integer(B4),
         Answer_n = ifelse(
           B4 < 1000, B4, 2023 - B4
         ))

#Summarize
B4_summay <- 
  survey_B4_v2 %>% 
  group_by(Answer_n) %>% 
  count()

#### Histogram distribution ####
ggplot(B4_summay, aes(x = Answer_n))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of number of years since first academic appointment") +
  xlab("Years") + 
  ylab("Density")+
  # geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20)


### B5 - Please choose the option that best describes your current employment status. ######
survey_B5_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B5) %>% 
  unnest(B5)

survey_B5_v2 <- 
  survey_B5_v1 %>% 
  filter(!B5 == "Other") %>% 
  mutate(Answer_n = ifelse(
    B5 == "Poste 3 jours semaine et contrat 2 jours semaine pour combler ", "Other", ifelse(
      B5 == "Regular full-time", "Other", ifelse(
        B5 == "Regular full-time", "Other", ifelse(
          B5 == "Retired", "Other", ifelse(
            B5 == "full-time contract ", "Other", ifelse(
              B5 == "on contract", "Other", ifelse(
                B5 == "retired", "Other", B5
                ))))))))

B5_summay <- 
  survey_B5_v2 %>% 
  group_by(Answer_n) %>% 
  count()

B5_summay$Answer_n[B5_summay$Answer_n == "Full-time permanent position"] <- "Full-time permanent\nposition"

#### Pie chart ####

PieDonut(B5_summay, 
         aes(Answer_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5,
         pieLabelSize = 7)+
  scale_fill_manual(values =  cbp1)


### B6 - Please provide the number of years since your first appointment into this position. ######
survey_B6_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B6) %>% 
  unnest(B6) %>% 
  mutate(B6 = as.integer(B6))

#summary table
B6_summay <- 
  survey_B6_v1 %>% 
  group_by(B6) %>% 
  count()

#### Histogram distribution ####
ggplot(B6_summay, aes(x = B6))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of the number of years since your first appointment into this position") +
  xlab("Years") + 
  ylab("Density")+
  # geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20)


### B7 - Are you eligible to apply for and receive Tri-Council, CFI, or other research funding? ######
survey_B7_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B7) %>% 
  unnest(B7) # n = 322

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
  rename(answer = B8) # n = 288


#summarize the data
B8_summary <- 
  survey_B8_v1 %>% 
  group_by(answer) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#link TC3 to B8 IDs
B8.domain <- 
  survey_B8_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  mutate(order = ifelse(
    answer == "CIHR", 1, ifelse(
      answer == "NSERC", 2, ifelse(
        answer == "SSHRC", 3, ifelse(
          answer == "CFI", 4, ifelse(
            answer == "CANARIE", 5, ifelse(
              answer == "Genome Canada", 6, ifelse(
                answer == "Alliance", 7, ifelse(
                  answer == "Provincial funding", 8, ifelse(
                    answer == "Institutional", 9, ifelse(
                      answer == "Industry grants", 10, ifelse(
                        answer == "International funding", 11, 12
                        ))))))))))))


Workflow.B8 <- 
  B8.domain %>% 
  unique()

nHR <- filter(Workflow.B8, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #65
nSE <- filter(Workflow.B8, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#136
nSSH <- filter(Workflow.B8, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #73

Workflow_Health <- filter(Workflow.B8, TC3=="Health Research") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.B8, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.B8, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

# #### Plot funds source #### 

 ggplot(Workflow_Tri2, aes(x=reorder(answer,-order))) + 
   geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
   scale_fill_manual(values =  cbp1) + 
   coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
   theme_linedraw(base_size = 20) +
   theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
   # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
   xlab("") + 
   ylab("")

### B9 - Please choose the option that best describes your current employment status. ######
survey_B9_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "B9")  

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_B9_v1$Question),'a___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", "")

#Bin tables
survey_B9_v2 <- cbind(separete_v3, survey_B9_v1)

#Clean the data
survey_B9_v3 <- 
  survey_B9_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Privatesector_industry_", "Private sector/industry", ifelse(
      Answer_q == "Publicsector_government_", "Public sector/government_", ifelse(
        Answer_q == "Nonfor_profit_sector_", "Non-for-profit sector", "Consultant"
        )))) %>% 
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#summarize the data
summary_B9<-
  survey_B9_v3 %>%
  group_by(answer) %>%
  count() %>%
  print()

#Link to TC3
survey_B9_v3_tc3 <- 
  survey_B9_v3 %>% 
  left_join(domain1, by = "Internal.ID")

#summarize the data
summary_B9_TC3<- 
  survey_B9_v3_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  drop_na() %>% 
  print()

##add percentage
Workflow.B9 <- 
  survey_B9_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.B9, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.B9, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#91
nSSH <- filter(Workflow.B9, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #31

Workflow_Health <- filter(Workflow.B9, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.B9, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.B9, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plot and Pie chart#### 

#Bar plot Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

#Pie chart
PieDonut(summary_B9, 
         aes(answer, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         titlesize = 5,
         pieLabelSize = 7)+
  scale_fill_manual(values =  cbp1)

### C ############################################################################################
### C1 - Does the definition of research software meet your understanding of research software, and if not, how would you describe research software? ######
survey_C1_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C1) %>% 
  unnest(C1) %>% 
  rename(answer = C1) # n = 294

#Clean the data
survey_C1_v2 <- 
  survey_C1_v1 %>% 
  mutate(answer_n = ifelse(
    answer == "No", "No", ifelse(
      answer == "Yes", "Yes", ifelse(
        answer == "Not sure", "Not sure", "Other"))))

survey_C1_v3 <- 
  survey_C1_v2 %>% 
  filter(!answer_n == "Other") # n = 254

#Select other
other <- 
  survey_C1_v2 %>% 
  filter(answer_n == "Other")

# #Extract open text
# write.csv(other, "C1.csv")

C1_summay <- 
  survey_C1_v3 %>% 
  group_by(answer_n) %>% 
  count()

#### Pie chart #### 
PieDonut(C1_summay, 
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

### C2 - Do you consider the use of research software critical to your research? ######
survey_C2_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C2) %>% 
  unnest(C2) %>% 
  rename(answer = C2)

#Clean data
survey_C2_v2 <- 
  survey_C2_v1 %>% 
  mutate(answer_n = ifelse(
    answer == "No", "No", ifelse(
      answer == "Yes", "Yes", ifelse(
        answer == "Not sure", "Not sure", "Other"))))

survey_C2_v3 <- 
  survey_C2_v2 %>% 
  filter(!answer_n == "Other") # n = 254

#Select other
other <- 
  survey_C2_v2 %>% 
  filter(answer_n == "Other")

# #Extract open text
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

### C3 - Do you consider the development of research software a primary output of your research? ######
survey_C3_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C3) %>% 
  unnest(C3) %>% 
  rename(answer = C3) # n = 225


C3_summay <- 
  survey_C3_v1 %>% 
  group_by(answer) %>% 
  count()

#Link to TC3
survey_C3_v3_tc3 <- 
  survey_C3_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_C3_tc3 <- 
  survey_C3_v3_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()

#HR
summary_C3_HR <- 
  summary_C3_tc3 %>% 
  filter(TC3 == "Health Research")

#SE
summary_C3_SE <- 
  summary_C3_tc3 %>% 
  filter(TC3 == "Sciences and Engineering")

#SCH
summary_C3_SCH <- 
  summary_C3_tc3 %>% 
  filter(TC3 == "Social Sciences and Humanities")

#### Pie chart #### 
#all
PieDonut(summary_C3_tc3, 
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
  scale_fill_manual(values =  cbp1)

#HR
PieDonut(summary_C3_HR, 
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
  scale_fill_manual(values =  cbp1)

#SE
PieDonut(summary_C3_SE, 
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
  scale_fill_manual(values =  cbp1)

#SCH
PieDonut(summary_C3_SCH, 
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
  scale_fill_manual(values =  cbp1)

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
  survey_C4_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_C4_tc3 <- 
  survey_C4_v3_tc3 %>% 
  group_by(TC3) %>% 
  count() %>% 
  print()

#summarize the data
summary_C4_tc4 <- 
  survey_C4_v3_tc3 %>% 
  group_by(answer, TC3) %>% 
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
  unique() # n = 153

#Link to TC3
C5.domain <- 
  survey_C5_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() # 

# #summarize the data
C5_summary <-
  C5.domain %>%
  group_by(answer_n, TC3) %>%
  count() %>%
  arrange(-n) %>%
  print()


##add percentage
Workflow.C5 <- 
  C5.domain %>% 
  unique()

nHR <- filter(Workflow.C5, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #25
nSE <- filter(Workflow.C5, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#49
nSSH <- filter(Workflow.C5, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #23

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
  survey_C6_v1 %>% 
  group_by(answer) %>% 
  count()

#Link to TC3 and add soring
survey_C6_v3_tc3 <- 
  survey_C6_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  mutate(order = ifelse(
    answer == "Neutral", 3, ifelse(
      answer == "Not important", 4, ifelse(
        answer == "Somewhat important", 2, ifelse(
          answer == "Very important", 1, 5
          ))))) %>%
  mutate(Sorting = ifelse(
    order == 1, "A", ifelse(
      order == 2, "B", ifelse(
        order == 3, "C", ifelse(
          order == 4, "D", "E"
          ))))) %>% 
  drop_na()


###likert graph
#Select imporant columns
C6_clean <- 
  survey_C6_v3_tc3 %>% 
  select(Internal.ID, TC3, Sorting, answer)

#summarize the table
Workfl <- 
  C6_clean %>% 
  group_by(TC3, answer, Sorting)%>%
  summarize(n= n()) %>% 
  drop_na() %>% 
  as.tibble() %>% 
  print()

#Summarize n groups
group_n <- 
  Workfl %>% 
  group_by(TC3) %>% 
  summarise(sum(n)) %>% 
  rename(group_n = `sum(n)`)

#Merge n groups to summarized table
Workfl1 <- 
  Workfl %>% 
  left_join(group_n, by = "TC3")

#Add % for each observation
TC3_Needs_sub <- Workfl1 %>% 
  mutate("%" = (n/group_n)*100)

#Round the %
TC3_Needs_sub1 <- 
  TC3_Needs_sub %>%
  select(-n, -group_n)

##add percentage
Workflow.C6 <- 
  survey_C6_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.C6, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #56
nSE <- filter(Workflow.C6, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#116
nSSH <- filter(Workflow.C6, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #66

Workflow_Health <- filter(Workflow.C6, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.C6, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.C6, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot TC3 + Likert + piechart #### 

#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

#Plot likert
ggplot(TC3_Needs_sub1, aes(x=TC3, y= `%`, fill= answer))+
  geom_col()+
  # facet_grid(rows=vars(TC3)) + 
  scale_fill_manual(values =  likert_color) + 
  geom_hline(yintercept = 50, linetype="dotted", color = "black", size=.75) +
  coord_flip() +
  geom_text(aes(label = round(`%`, digits = 1)), position = position_stack(vjust = .5)) +
  theme_linedraw(base_size = 18) +
  # ggtitle("Importance of cloud service to support research") +
  xlab("Services") + 
  ylab("") 

#Pie chart
PieDonut(C6_summay, 
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

### C11 - Are there particular software platforms or software services you currently use which you feel would be valuable to be offered as a national service for all researchers to access? ######
survey_C11_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C11) %>% 
  unnest(C11)

C11_summay <- 
  survey_C11_v1 %>% 
  group_by(C11) %>% 
  count()

#### Pie chart #### 
PieDonut(C11_summay, 
         aes(C11, count= n), 
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

#Clean the data
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

# #Export open text
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

### C13 - Are there particular software tools, platforms or software services you currently develop or co-develop which you feel would be valuable to be offered as a national service for all researchers to access? ######
survey_C13_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, C13) %>% 
  unnest(C13)

C13_summay <- 
  survey_C13_v1 %>% 
  group_by(C13) %>% 
  count()

#### Pie chart #### 
PieDonut(C13_summay, 
         aes(C13, count= n), 
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


# D ############################################################################################
### D1 - Can you provide an estimate of the time you and your research team spend developing research software? ######
survey_D1_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D1") %>% 
  mutate(question_n = ifelse(
    Question == "Do_you_yourself_lead__or_have_you_previously_led__a_research_software_development_project____Currently_leading_", "Currently", "Previously"
  )) %>% 
  drop_na() %>% 
  select(-Question)

#Clean the data
survey_D1_v2 <- 
  survey_D1_v1 %>% 
  drop_na() %>% 
  unnest(Answer) %>% 
  select(-Ques_num)

#"Currently" data
D1_Currently <- 
  survey_D1_v2 %>% 
  filter(question_n == "Currently")

#"Previously" data
D1_Previously <- 
  survey_D1_v2 %>% 
  filter(question_n == "Previously")

#Summarize tables
D1_Currently_sum <- 
  D1_Currently %>% 
  group_by(Answer) %>% 
  count()

D1_Previously_sum <- 
  D1_Previously %>% 
  group_by(Answer) %>% 
  count()


#### Bar plots#### 

#"Currently"
PieDonut(D1_Currently_sum, 
         aes(Answer, count= n), 
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


#"Previously"
PieDonut(D1_Previously_sum, 
         aes(Answer, count= n), 
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

# #Export open text
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

##Link Domain - D4 and B3 (role)
#Role
D4_B3 <- 
  survey_D4_v1 %>% 
  left_join(survey_B3_v3, by = "Internal.ID")

#Domain - role and D4
D4_B3_domain <- 
  D4_B3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#Categorize the years of experience 0, 0-5, 6-10...
D4_B3_domain_clean <- 
  D4_B3_domain %>% 
  mutate(D4 = as.integer(D4),
         Years = ifelse(
           D4 == 0, "0", ifelse(
             D4 > 0 & D4 <= 5, "1-5", ifelse(
               D4 > 5 & D4 <= 10, "6-10", ifelse(
                 D4 > 10 & D4 <= 15, "11-15", ifelse(
                   D4 > 15 & D4 <= 20, "16-20", ifelse(
                     D4 > 20 & D4 <= 25, "21-25", ifelse(
                       D4 > 25 & D4 <= 30, "26-30", ifelse(
                         D4 > 30 & D4 <= 35, "31-35", ifelse(
                           D4 > 35 & D4 <= 40, "36-40", ifelse(
                             D4 > 40 & D4 <= 45, "41-45", "46-50"
                             ))))))))))) %>% 
  mutate(Order = ifelse(
    Years == "0", 1, ifelse(
      Years == "1-5", 2, ifelse(
        Years == "6-10", 3, ifelse(
          Years == "11-15", 4, ifelse(
            Years == "16-20", 5, ifelse(
              Years == "21-25", 6, ifelse(
                Years == "26-30", 7, ifelse(
                  Years == "31-35", 8, ifelse(
                    Years == "36-40", 9, ifelse(
                      Years == "41-45", 10, 11
                      )))))))))))

##add percentage
Workflow.D4 <- 
  D4_B3_domain_clean %>% 
  select(-D4) %>% 
  unique()

nHR <- filter(Workflow.D4, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #38
nSE <- filter(Workflow.D4, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#93
nSSH <- filter(Workflow.D4, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #39

Workflow_Health <- filter(Workflow.D4, TC3=="Health Research") %>%
  group_by(TC3, Years, Order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D4, TC3=="Sciences and Engineering") %>%
  group_by(TC3, Years, Order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D4, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, Years, Order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot #### 

#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(Years,Order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

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
  drop_na() %>% 
  mutate(Order = ifelse(
    answer_n == "Design", 1, ifelse(
      answer_n == "Development", 2, ifelse(
        answer_n == "Documentation", 3, ifelse(
          answer_n == "Outreach", 4, ifelse(
            answer_n == "Funding", 5, ifelse(
              answer_n == "Quality Assurance", 6, ifelse(
                answer_n == "Researcher/End-User", 7, 8
              ))))))))

#link to roles
D5_TC3_roles <- 
  D5.domain %>% 
  left_join(survey_B3_v3, by = )

#summary table - D5 and TC3
D5_summary <- 
  D5.domain %>% 
  group_by(answer_n, TC3) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate()
  print()


#Link Domain - D5 and B3 (role)
D5_B3 <- 
  survey_D5_v3 %>% 
  left_join(survey_B3_v4.1, by = "Internal.ID") %>% 
  drop_na() %>% 
  select(-Ques_num) %>% 
  unique()
  
#summary table - D4 and Role
D5_B3_summary <- 
  D5_B3 %>% 
  group_by(answer_n, Role_n) %>% 
  count()  %>% 
  mutate(Role_n = ifelse(
    Role_n == "Faculty - Professor (including assistant/associate/full professor, clinical professor, teaching professor)", "Faculty - Professor", Role_n),
    Order = ifelse(
      answer_n == "Design", 1, ifelse(
        answer_n == "Development", 2, ifelse(
          answer_n == "Documentation", 3, ifelse(
            answer_n == "Outreach", 4, ifelse(
              answer_n == "Funding", 5, ifelse(
                answer_n == "Quality Assurance", 6, ifelse(
                  answer_n == "Researcher/End-User", 7, 8
                ))))))))

#select "Others"
other <- 
  survey_D5_v3 %>% 
  filter(answer_n == "Other") %>% 
  drop_na() %>% 
  unique() %>% 
  print()

# #Export open text
# write.csv(other, "D5.csv")

##add percentage
Workflow.D5 <- 
  D5.domain %>% 
  unique()

nHR <- filter(Workflow.D5, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #36
nSE <- filter(Workflow.D5, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#93
nSSH <- filter(Workflow.D5, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #36

Workflow_Health <- filter(Workflow.D5, TC3=="Health Research") %>%
  group_by(TC3, answer_n, Order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D5, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n, Order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D5, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n, Order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plot #### 

#D5 + B3 (roles) 
ggplot(D5_B3_summary, aes(fill=Role_n, y=n, x= reorder(answer_n, -Order))) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()+
  scale_fill_manual(values =  cbp_Cad) +
  # ggtitle("") +
  guides(fill=guide_legend(title="Role"))+
  theme_linedraw(base_size = 20) +
  coord_flip() +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("Years of research software development experience")+
  ylab("n")



#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer_n, -Order))) + 
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
#CLean data
survey_D6_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D6") %>% 
  mutate(answer_n = ifelse(
    Question == "Who_develops_the_software_in_your_group___Faculty___Professor__including_assistant_associate_full_professor__clinical_professor__teaching_professor__", "Faculty/Librarian", ifelse(
      Question == "Who_develops_the_software_in_your_group___Faculty___Adjunct__emeritus__visiting__or_limited_term_" , "Faculty/Librarian", ifelse(
        Question == "Who_develops_the_software_in_your_group___Administrator__", "Administrator", ifelse(
          Question == "Who_develops_the_software_in_your_group___Post_Doctoral_Fellow___", "Researcher", ifelse(
            Question == "Who_develops_the_software_in_your_group___Research_Software_Engineer___Expert_", "Research Software Developer", ifelse(
              Question == "Who_develops_the_software_in_your_group___Research_Associate__"  , "Researcher", ifelse(
                Question == "Who_develops_the_software_in_your_group___Research_Staff__", "Researcher", ifelse(
                  Question == "Who_develops_the_software_in_your_group___Student__Doctoral__", "Student", ifelse(
                    Question == "Who_develops_the_software_in_your_group___Student__Masters___", "Student", ifelse(
                      Question == "Who_develops_the_software_in_your_group___Student__Undergrad___", "Student", ifelse(
                        Question == "Who_develops_the_software_in_your_group___Researcher_", "Researcher", ifelse(
                          Question == "Who_develops_the_software_in_your_group___Librarian_" , "Faculty/Librarian", ifelse(
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

#select "Others"
other <- 
  survey_D6_v2 %>% 
  filter(answer_n == "Other") %>% 
  drop_na() %>% 
  unique() %>% 
  print()

# #Export open text
# write.csv(other, "D6.csv")

##add percentage
Workflow.D6 <- 
  D6.domain %>% 
  unique()

nHR <- filter(Workflow.D6, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #38
nSE <- filter(Workflow.D6, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#91
nSSH <- filter(Workflow.D6, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #36

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

D7_you <- 
  D7_you %>% 
  filter(!Answer == "Between 1 and 2 FTEs" & !Answer == "More than 2 FTEs")

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

#Link "Team" to role
D7_team_role <- 
  D7_team %>% 
  left_join(survey_B3_v4, by = "Internal.ID") %>% 
  drop_na() 

#Link "You" to B3 (role)
D7_you_role <- 
  D7_you %>% 
  left_join(survey_B3_v4, by = "Internal.ID") %>% 
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
    Answer == "More than 2 FTEs", "delete", ifelse(
      Answer == "Between 1 and 2 FTEs", "delete", ifelse(
        Answer == "Less than 1/4 of an FTE", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  arrange(-n) %>% 
  filter(!answer_n == "delete") %>% 
  print()

#summary table - "Team" and Role
D7_team_Role_summary <- 
  D7_team_role %>% 
  group_by(Answer, Role_n) %>% 
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


#summary table - "You" and Role
D7_you_Role_summary <- 
  D7_you_role %>% 
  group_by(Answer, Role_n) %>% 
  count() %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of an FTE", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", "delete", ifelse(
      Answer == "Between 1 and 2 FTEs", "delete", ifelse(
        Answer == "Less than 1/4 of an FTE", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  arrange(-n) %>% 
  filter(!answer_n == "delete") %>% 
  print()

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


#"Team" and Role
ggplot(D7_team_Role_summary, aes(fill=Role_n, y=n, x= reorder(answer_n, order))) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp_Cad) +
  guides(fill=guide_legend(title="Role"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")

#"You" and Role
ggplot(D7_you_Role_summary, aes(fill=Role_n, y=n, x= reorder(answer_n, order))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp_Cad) + 
  guides(fill=guide_legend(title="Role"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")

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
  filter(!answer == "Hard to quantify due but have staff of 16 FTE that work in some capacity on software development") %>% 
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

nHR <- filter(Workflow.D8, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #38
nSE <- filter(Workflow.D8, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#89
nSSH <- filter(Workflow.D8, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #39

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
#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

### D9 - What are the primary programming languages you use to write your research software? ######
survey_D9_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D9")  

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_D9_v1$Question),'____',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", "")

#Bin tables
survey_D9_v2 <- cbind(separete_v3, survey_D9_v1)

#Clean the data
survey_D9_v3 <- 
  survey_D9_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "C_", "C#", ifelse(
      Answer_q == "C__", "C++", ifelse(
        Answer_q == "XQuery__XSLT_", "XQuery / XSLT", ifelse(
          Answer_q == "NOSQL_e_g__Cypher__SPARQL__","NOSQL (e.g. Cypher, SPARQL)", Answer_q
          ))))) %>% 
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#summarize the data
summary_D9<- 
  survey_D9_v3 %>% 
  group_by(answer) %>% 
  count() %>% 
  print()

#Link to TC3
survey_D9_v3_tc3 <- 
  survey_D9_v3 %>% 
  left_join(domain1, by = "Internal.ID")

#summarize the data
summary_D9_TC3<- 
  survey_D9_v3_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()

##add percentage
Workflow.D9 <- 
  survey_D9_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.D9, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.D9, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#91
nSSH <- filter(Workflow.D9, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #31

Workflow_Health <- filter(Workflow.D9, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D9, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D9, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots#### 

#all
ggplot(summary_D9, aes(y=n, x= reorder(answer, n))) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()+
  scale_fill_manual(values =  cbp1) + 
  # guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("Programming language")+
  ylab("n")


#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

### D10 - What type of research software do you develop? [refer to research software types from research software current state report] ######
survey_D10_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D10")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_D10_v1$Question),'____',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_D10_v2 <- cbind(separete_v3, survey_D10_v1)

#Clean the data
survey_D10_v3 <- 
  survey_D10_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Libraries that_are_used_by_other_research_software_tools__e_g__Numpy__", "Libraries that are used by other research software tools (e.g. Numpy)", ifelse(
      Answer_q == "Tools that_are_run_by_researchers__e_g__OpenFoam__", "Tools that are run by researchers (e.g. OpenFoam)", ifelse(
        Answer_q == "Services that_are_used_queried_by_researchers__e_g__MongoDB__", "Services that are used queried by researchers (e.g. MongoDB)", ifelse(
          Answer_q == "Platforms that_are_used_by_end_users__e_g__FRDR__","Platforms that are use by end users (e.g. FRDR)", ifelse(
            Answer_q == "Open source__e_g__GNU_Linux__", "Open source (e.g. GNU/Linux)", ifelse(
              Answer_q == "Closed source__e_g__MATLAB__", "Closed source (e.g. MATLAB)", ifelse(
                Answer_q == "Hybrid dual__or_multi_licensing__e_g__MySQL_AB_database__", "Hybrid dual or multi-licensing (e.g. MySQL AB database)", ifelse(
                  Answer_q == "Source code__e_g__Python_code__", "Source code (e.g. Python_code)", ifelse(
                    Answer_q == "Binary executable__package__e_g___exe_file__", "Binary executable, package (e.g. .exe file)", ifelse(
                      Answer_q == "Container _e_g__Docker_container__", "Container (e.g. Docker container)", ifelse(
                        Answer_q == "Virtual machine_image__e_g__VMware_Workstation__", "Virtual machine image (e.g. VMware Workstation)", ifelse(
                          Answer_q == "Service _e_g__Slack__", "Service (e.g. Slack)", ifelse(
                            Answer_q == "General _e_g__Numpy__", "General (e.g. Numpy)", ifelse(
                              Answer_q == "Domain specific__e_g__Astropy__", "Domain-specific (e.g. Astropy)", ifelse(
                                Answer_q == "Quantitative _e_g__R__", "Quantitative (e.g. R)", ifelse(
                                  Answer_q == "Qualitative _e_g__NVivo__", "Qualitative (e.g. NVivo)", ifelse(
                                    Answer_q == "Planning _e_g__Microsoft_Planner__", "Planning (e.g. Microsoft Planner)", ifelse(
                                      Answer_q == "Analysis _e_g__SAS__", "Analysis (e.g. SAS)", ifelse(
                                        Answer_q == "Computation _e_g__Mathematica__", "Computation e.g. Mathematica)", ifelse(
                                          Answer_q == "Visualisation _e_g__MindManager__ParaView__", "Visualisation (e.g. MindManager, ParaView)", ifelse(
                                            Answer_q == "Transfer _e_g__FTP__", "Transfer (e.g. FTP)", ifelse(
                                              Answer_q == "Storage _e_g__Dropbox__", "Storage (e.g. Dropbox)", ifelse(
                                                Answer_q == "Publishing _e_g__Zenodo__", "Publishing (e.g. Zenodo)", ifelse(
                                                  Answer_q == "Curation preservation_of_data__e_g__Zenodo__", "Curation/preservationof data (e.g. Zenodo)", ifelse(
                                                    Answer_q == "Curation preservation_of_software__e_g__Software_Heritage__Zenodo__", "Curation/preservation of software (e.g. Software Heritage, Zenodo)", ifelse(
                                                      Answer_q == "Discovery _e_g__DataONE__", "Discovery (e.g. (DataONE)", "Other"
                                                      ))))))))))))))))))))))))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#summarize the data
summary_D10<- 
  survey_D10_v3 %>% 
  group_by(answer) %>% 
  count() %>% 
  print()

#Link to TC3
survey_D10_v3_tc3 <- 
  survey_D10_v3 %>% 
  left_join(domain1, by = "Internal.ID")

#summarize the data
summary_D10_TC3<- 
  survey_D10_v3_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()

##add percentage
Workflow.D10 <- 
  survey_D10_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.D10, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #35
nSE <- filter(Workflow.D10, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#92
nSSH <- filter(Workflow.D10, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33

Workflow_Health <- filter(Workflow.D10, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D10, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D10, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots#### 

#all
ggplot(summary_D10, aes(y=n, x= reorder(answer, n))) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()+
  scale_fill_manual(values =  cbp1) + 
  # guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("Programming language")+
  ylab("n")


#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

### D12 - Do you have research software platforms and/or tools your research team has created but are no longer maintained? ######
survey_D12_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D12) %>% 
  unnest(D12) %>% 
  filter(D12 == "No" | D12 == "Yes" | D12 == "Not sure") # n = 166

D12_summay <- 
  survey_D12_v1 %>% 
  group_by(D12) %>% 
  count()

#### Pie chart #### 
PieDonut(D12_summay, 
         aes(D12, count= n), 
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



### D13 - Do you create/provide documentation for all of your software outputs? ######
survey_D13_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D13) %>% 
  unnest(D13) %>% 
  rename(answer = D13) %>% 
  unique()

survey_D13_TC3 <- 
  survey_D13_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() # n = 159


##add percentage
Workflow.D13 <- 
  survey_D13_TC3 %>% 
  unique() %>% 
  mutate(order = ifelse(
    answer == "Always", 1, ifelse(
      answer == "Often", 2, ifelse(
        answer == "Sometimes", 3, ifelse(
          answer == "Rarely", 4, 5
          )))))

#Link to TC3 and add soring
survey_D13_v3_tc3 <- 
  survey_D13_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  mutate(order = ifelse(
    answer == "Always", 1, ifelse(
      answer == "Often", 2, ifelse(
        answer == "Sometimes", 3, ifelse(
          answer == "Rarely", 4, 5
        ))))) %>%
  mutate(Sorting = ifelse(
    order == 1, "A", ifelse(
      order == 2, "B", ifelse(
        order == 3, "C", ifelse(
          order == 4, "D", "E"
        ))))) %>% 
  drop_na()


###likert graph
#Select imporant columns
D13_clean <- 
  survey_D13_v3_tc3 %>% 
  select(Internal.ID, TC3, Sorting, answer)

#summarize the table
Workfl <- 
  D13_clean %>% 
  group_by(TC3, answer, Sorting)%>%
  summarize(n= n()) %>% 
  drop_na() %>% 
  as.tibble() %>% 
  print()

#Summarize n groups
group_n <- 
  Workfl %>% 
  group_by(TC3) %>% 
  summarise(sum(n)) %>% 
  rename(group_n = `sum(n)`)

#Merge n groups to summarized table
Workfl1 <- 
  Workfl %>% 
  left_join(group_n, by = "TC3")

#Add % for each observation
TC3_Needs_sub <- Workfl1 %>% 
  mutate("%" = (n/group_n)*100)

#Round the %
TC3_Needs_sub1 <- 
  TC3_Needs_sub %>%
  select(-n, -group_n)
# mutate(`%` = round(`%`))


nHR <- filter(Workflow.D13, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #34
nSE <- filter(Workflow.D13, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#90
nSSH <- filter(Workflow.D13, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #35

Workflow_Health <- filter(Workflow.D13, TC3=="Health Research") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D13, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D13, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plot and likert- TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


#Plot likert
ggplot(TC3_Needs_sub1, aes(x=TC3, y= `%`, fill= Sorting))+
  geom_col()+
  scale_fill_manual(values =  likert_color1) + 
  geom_hline(yintercept = 50, linetype="dotted", color = "black", size=.75) +
  coord_flip() +
  geom_text(aes(label = round(`%`, digits = 1)), position = position_stack(vjust = .5)) +
  theme_linedraw(base_size = 18) +
  # ggtitle("Importance of cloud service to support research") +
  xlab("Services") + 
  ylab("") 

### D14 - Are you / your lab / collaborators the only users of your research software, or do you have external users? ######
survey_D14_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D14")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_D14_v1$Question),'s__',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_D14_v2 <- cbind(separete_v3, survey_D14_v1)

#Clean the data
survey_D14_v3 <- 
  survey_D14_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == " Mainly_internal_", "Mainly internal", ifelse(
      Answer_q == " Mainly_external__", "Mainly external", "Delete"
          ))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n) %>% 
  filter(!answer == "Delete")

D14_summay <- 
  survey_D14_v3 %>% 
  group_by(answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D14_summay, 
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


### D15 - Are you / your team involved in the co-development of, or contribution to a research software platform? ######
survey_D15_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D15") %>% 
  drop_na() %>% 
  filter(Answer == "Yes" | Answer == "No")

D15_summay <- 
  survey_D15_v1 %>% 
  group_by(Answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D15_summay, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie1)

### D16 - What type of research software do you develop? [refer to research software types from research software current state report] ######
survey_D16_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D16")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_D16_v1$Question),'___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")


#Bin tables
survey_D16_v2 <- cbind(separete_v3, survey_D16_v1)

#Clean the data
survey_D16_v3 <- 
  survey_D16_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Institutional policies__", "Institutional policies", ifelse(
      Answer_q == "Funder policies_", "Funder policies", ifelse(
        Answer_q == "Publisher policies__", "Publisher policies", ifelse(
          Answer_q == "Research association_society_policies_", "Research association/society policies", ifelse(
            Answer_q == "Research community_collaborators__expectation__", "Research community/collaborators' expectation", "Other"
              )))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_D16_v3_tc3 <- 
  survey_D16_v3 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_D16_TC3<- 
  survey_D16_v3_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()

##add percentage
Workflow.D16 <- 
  survey_D16_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.D16, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #32
nSE <- filter(Workflow.D16, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#79
nSSH <- filter(Workflow.D16, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #30

Workflow_Health <- filter(Workflow.D16, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D16, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D16, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3#### 

ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### D17 - Choose the 3 methods you most often use to share your research software. ######
survey_D17_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D17) %>% 
  unnest(D17) %>% 
  rename(answer = D17)

#Link to TC3
survey_D17_v1_tc3 <- 
  survey_D17_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na()

#summarize the data
summary_D17_TC3<- 
  survey_D17_v1_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()

##add percentage
Workflow.D17 <- 
  survey_D17_v1_tc3 %>% 
  unique()

nHR <- filter(Workflow.D17, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #28
nSE <- filter(Workflow.D17, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#84
nSSH <- filter(Workflow.D17, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #30

Workflow_Health <- filter(Workflow.D17, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D17, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D17, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3#### 

ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")



### D18 - Is there a research publication that describes your research software? ######
survey_D18_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D18") %>% 
  drop_na() %>% 
  filter(Answer == "Yes" | Answer == "No")


survey_D18_v1_RS <- 
  survey_organized %>% 
  filter(Ques_num == "D18") %>% 
  drop_na() %>% 
  filter(!Answer == "Yes") %>% 
  filter(!Answer == "No") %>% 
  select(-Question)

# write.csv(survey_D18_v1_RS, "D18_RS.csv")

D18_summay <- 
  survey_D18_v1 %>% 
  group_by(Answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D18_summay, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie1)

### D19 - Are you aware of the FAIR4RS Principles, and / or other related principles and best practices for research software development? ######
survey_D19_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D19") %>% 
  drop_na() %>% 
  filter(Answer == "Yes" | Answer == "No")


survey_D19_v1_RS <- 
  survey_organized %>% 
  filter(Ques_num == "D19") %>% 
  drop_na() %>% 
  filter(!Answer == "Yes") %>% 
  filter(!Answer == "No") %>% 
  select(-Question)

# #Export open text
# write.csv(survey_D19_v1_RS, "D19_comment.csv")

D19_summay <- 
  survey_D19_v1 %>% 
  group_by(Answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D19_summay, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie1)

### D20 - How important is (or would be) such a service to your research? ######
survey_D20_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D20) %>% 
  unnest(D20) %>% 
  rename(answer = D20)

D20_summay <- 
  survey_D20_v1 %>% 
  group_by(answer) %>% 
  count()

#Link to TC3
survey_D20_v3_tc3 <- 
  survey_D20_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  mutate(order = ifelse(
    answer == "Always", 1, ifelse(
      answer == "Very often", 2, ifelse(
        answer == "Sometimes", 3, ifelse(
          answer == "Rarely", 4, 5
        ))))) %>%
  drop_na()

##add percentage
Workflow.D20 <- 
  survey_D20_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.D20, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #30
nSE <- filter(Workflow.D20, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#81
nSSH <- filter(Workflow.D20, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #27

Workflow_Health <- filter(Workflow.D20, TC3=="Health Research") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D20, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D20, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 

ggplot(Workflow_Tri2, aes(x=reorder(answer, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")



### D21 - Are you able to measure the impact/use of your research software using other methods (e.g., number of users, number of downloads)? ######
survey_D21_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D21") %>% 
  drop_na() %>% 
  filter(Answer == "Yes" | Answer == "No")


survey_D21_v1_RS <- 
  survey_organized %>% 
  filter(Ques_num == "D21") %>% 
  drop_na() %>% 
  filter(!Answer == "Yes") %>% 
  filter(!Answer == "No") %>% 
  select(-Question)

# write.csv(survey_D21_v1_RS, "D21_comment.csv")

D21_summay <- 
  survey_D21_v1 %>% 
  group_by(Answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D21_summay, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie1)

### D23 - Do you use version control tools (e.g., Git/GitHub) for software development? ######
survey_D23_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D23) %>% 
  unnest(D23) %>% 
  rename(answer = D23) %>% 
  filter(answer == "Yes" | answer == "No"| answer == "Don't know")


D23_summay <- 
  survey_D23_v1 %>% 
  group_by(answer) %>% 
  count()


#### Pie chart #### 
PieDonut(D23_summay, 
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

### D24 - Do you leverage / use standards in your research software development (e.g., GA4GH, TEI, etc.)? ######
survey_D24_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D24") %>% 
  drop_na() %>% 
  filter(Answer == "Yes" | Answer == "No")

D24_summay <- 
  survey_D24_v1 %>% 
  group_by(Answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D24_summay, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie1)


### D25 - Do you implement software security best practices in your development efforts? ######
survey_D25_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D25") %>% 
  drop_na() %>% 
  filter(Answer == "Yes" | Answer == "No")

D25_summay <- 
  survey_D25_v1 %>% 
  group_by(Answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D25_summay, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie1)


### D26 - How would you describe your personal level of skills in programming/coding? ######
survey_D26_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D26) %>% 
  unnest(D26) %>% 
  rename(answer = D26)

#Link to TC3
survey_D26_v1_tc3 <- 
  survey_D26_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  mutate(order = ifelse(
    answer == "Expert (recognized authority)", 1, ifelse(
      answer == "Fundamental Awareness (basic knowledge)", 2, ifelse(
        answer == "Advanced (applied theory)", 3, ifelse(
          answer == "Intermediate (practical application)", 4, 5
        ))))) %>%
  drop_na()

##add percentage
Workflow.D26 <- 
  survey_D26_v1_tc3 %>% 
  unique()

nHR <- filter(Workflow.D26, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.D26, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#90
nSSH <- filter(Workflow.D26, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33

Workflow_Health <- filter(Workflow.D26, TC3=="Health Research") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D26, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D26, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 

ggplot(Workflow_Tri2, aes(x=reorder(answer, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")



### D27 - How much project time do you spend programming/coding on average per project (% of total project time)? ######
survey_D27_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D27) %>% 
  unnest(D27) %>% 
  rename(answer = D27) %>% 
  mutate(answer = as.integer(answer),
         percentage  = ifelse(
           answer <25, "0-25", ifelse(
             answer >24 & answer < 51, "25-50", ifelse(
               answer > 50 & answer < 76, "50-75", "75-100"
                 ))),
         order = ifelse(
           percentage == "0-25", 1, ifelse(
             percentage == "25-50", 2, ifelse(
               percentage == "50-75", 3, 4
               ))))

#Link to TC3
survey_D27_v1_tc3 <- 
  survey_D27_v1 %>% 
  left_join(domain1, by = "Internal.ID")


##add percentage
Workflow.D27 <- 
  survey_D27_v1_tc3 %>% 
  unique()

nHR <- filter(Workflow.D27, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.D27, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#80
nSSH <- filter(Workflow.D27, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #28

Workflow_Health <- filter(Workflow.D27, TC3=="Health Research") %>%
  group_by(TC3, percentage, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D27, TC3=="Sciences and Engineering") %>%
  group_by(TC3, percentage, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D27, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, percentage, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 

ggplot(Workflow_Tri2, aes(x=reorder(percentage, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")



### D28 - Are there factors preventing you from developing research software? ######
survey_D28_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "D28") %>% 
  drop_na()

D28_summay <- 
  survey_D28_v1 %>% 
  group_by(Answer) %>% 
  count()

#### Pie chart #### 
PieDonut(D28_summay, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie1)




### D30 - When you consider your own software, what do you see as the biggest barrier(s) to sustainability? ######
survey_D30_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, D30) %>% 
  unnest(D30) # n = 145

#Link to TC3
survey_D30_v1_tc3 <- 
  survey_D30_v1 %>% 
  left_join(domain1, by = "Internal.ID")


##add percentage
Workflow.D30 <- 
  survey_D30_v1_tc3 %>% 
  unique()

nHR <- filter(Workflow.D30, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #30
nSE <- filter(Workflow.D30, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#78
nSSH <- filter(Workflow.D30, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #30

Workflow_Health <- filter(Workflow.D30, TC3=="Health Research") %>%
  group_by(TC3, D30) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.D30, TC3=="Sciences and Engineering") %>%
  group_by(TC3, D30) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.D30, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, D30) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 

ggplot(Workflow_Tri2, aes(x=reorder(D30, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")




# E ############################################################################################
### E1 - Choose the 3 most common means which you have found other people’s research software you use in your research. ######
survey_E1_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E1")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_E1_v1$Question),'h___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_E1_v2 <- cbind(separete_v3, survey_E1_v1)

#Clean the data
survey_E1_v3 <- 
  survey_E1_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Directly from_authors_collaborators_", "Directly from authors/collaborators", ifelse(
      Answer_q == "Community experts___discipline_adoption_", "Community experts / discipline adoption", ifelse(
        Answer_q == "From scholarly_publications__e_g___software_publication__supplementary_materials__", "From scholarly publications (e.g., software publication, supplementary materials)", ifelse(
          Answer_q == "Courses _workshops__training_opportunities_", "Courses, workshops, training opportunities", ifelse(
            Answer_q == "Conferences ", "Conferences", ifelse(
              Answer_q == "Research software_author_s_website_", "Research software author's website", ifelse(
                Answer_q == "Local regional_support_personnel_", "Local/regional support personnel", ifelse(
                  Answer_q == "Software hosting_development_website__e_g___GitHub__", "Software hosting/development website (e.g., GitHub)", ifelse(
                    Answer_q == "Software curation_preservation_repository__e_g___Figshare__Zenodo__", "Software curation/preservation repository (e.g., Figshare, Zenodo)", ifelse(
                      Answer_q == "Domain specific_software_registry__e_g___Astrophysics_Source_Code_Library__", "Domain specific_software_registry (e.g., Astrophysics Source Code Library)", ifelse(
                        Answer_q == "Science gateways___research_platforms___virtual_research_environments__VREs____or_web_application_services_", "Science gateways / research platforms / virtual research environments (VREs), or web application/services", "Other"
                        )))))))))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_E1_v3_tc3 <- 
  survey_E1_v3 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.E1 <- 
  survey_E1_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.E1, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.E1, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#84
nSSH <- filter(Workflow.E1, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #39

Workflow_Health <- filter(Workflow.E1, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E1, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E1, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### E2 - Can you estimate the amount of time (% of your project time) you spend looking for the right research software for your research? ######
survey_E2_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E2) %>% 
  unnest(E2) %>% 
  rename(answer = E2) %>% 
  mutate(answer = as.integer(answer),
         percentage  = ifelse(
           answer <25, "0-25", ifelse(
             answer >24 & answer < 51, "25-50", ifelse(
               answer > 50 & answer < 76, "50-75", "75-100"
             ))),
         order = ifelse(
           percentage == "0-25", 1, ifelse(
             percentage == "25-50", 2, ifelse(
               percentage == "50-75", 3, 4
             ))))

#Link to TC3
survey_E2_v1_tc3 <- 
  survey_E2_v1 %>% 
  left_join(domain1, by = "Internal.ID")


##add percentage
Workflow.E2 <- 
  survey_E2_v1_tc3 %>% 
  unique()

nHR <- filter(Workflow.E2, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #30
nSE <- filter(Workflow.E2, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#73
nSSH <- filter(Workflow.E2, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #26

Workflow_Health <- filter(Workflow.E2, TC3=="Health Research") %>%
  group_by(TC3, percentage, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E2, TC3=="Sciences and Engineering") %>%
  group_by(TC3, percentage, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E2, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, percentage, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Bar plot - TC3 #### 

ggplot(Workflow_Tri2, aes(x=reorder(percentage, order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")




### E3 - On which platform(s) do you use research software? ######
survey_E3_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E3")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_E3_v1$Question),'e___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_E3_v2 <- cbind(separete_v3, survey_E3_v1)

#Clean the data
survey_E3_v3 <- 
  survey_E3_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Personal Desktop_laptop_computer_", "Personal Desktop/laptop computer", ifelse(
      Answer_q == "Lab computers_", "Lab computers", ifelse(
        Answer_q == "Remote ARC_resources__Former_Compute_Canada_Federation_Alliance__Provincial__Institutional__", "Remote ARC resources (Former Compute Canada Federation/Alliance, Provincial, Institutional)", ifelse(
          Answer_q == "Remote Cloud_resources__Former_Compute_Canada_Federation_Alliance__Provincial__Institutional__", "Remote Cloud resources (Former Compute Canada Federation/Alliance, Provincia, Institutional)", ifelse(
            Answer_q == "International ARC_resources__Access_US__EuroHPC_EU__", "International ARC resources (Access-US, EuroHPC-EU)", ifelse(
              Answer_q == "International Cloud_resources__EOSC_EU__", "International Cloud resources (EOSC-EU)", ifelse(
                Answer_q == "Commercial Cloud_resources__AWS__Azure__Google_Cloud_etc___", "Commercial Cloud resources (AWS, Azure, Google Cloud etc.)", ifelse(
                  Answer_q == "Mobile device_", "Mobile device", "Other" 
                  ))))))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_E3_v3_tc3 <- 
  survey_E3_v3 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.E3 <- 
  survey_E3_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.E3, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #34
nSE <- filter(Workflow.E3, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#83
nSSH <- filter(Workflow.E3, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #38

Workflow_Health <- filter(Workflow.E3, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E3, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E3, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### E4 - How do you run software and/or access the resources mentioned in the previous question to perform your research? ######
survey_E4_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E4")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_E4_v1$Question),'h___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_E4_v2 <- cbind(separete_v3, survey_E4_v1)

#Clean the data
survey_E4_v3 <- 
  survey_E4_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Run desktop_research_software_on_personal_computer_or_laptop_", "Run desktop research software on personal computer or laptop", ifelse(
      Answer_q == "Run desktop_research_software_on_lab_computers__", "Run desktop research software on lab computers", ifelse(
        Answer_q == "Run research_software_from_the_command_line_via_a_remote_SSH_terminal_connection_", "Run research software from the command line via\na remote SSH/terminal connection", ifelse(
          Answer_q == "Run research_software_from_the_command_line_via_a_remote_connection_to_a_Graphical_desktop__virtual_desktop__remote_desktop__etc____", "Run research software from the command line via a remote connection\nto a Graphical desktop (virtual desktop, remote desktop, etc.)", ifelse(
            Answer_q == "Run research_software_using_a_Graphical_User_Interface_via_a_remote_connection_to_a_Graphical_desktop__virtual_desktop__remote_desktop__etc____", "Run research software using a Graphical User Interface via a remote connection\nto a Graphical desktop (virtual desktop, remote desktop, etc.)", ifelse(
              Answer_q == "Use research_software_through_web_based_science_gateways__research_platforms__virtual_research_environments__VRE__", "Use research software through web based science gateways,\nresearch platforms, virtual research environments (VRE)", "Other"
              ))))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_E4_v3_tc3 <- 
  survey_E4_v3 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.E4 <- 
  survey_E4_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.E4, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.E4, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#82
nSSH <- filter(Workflow.E4, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #38

Workflow_Health <- filter(Workflow.E4, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E4, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E4, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### E5 - Do you use the research software installed and maintained on Canadian ARC resources (e.g., the software installed on ARC platforms such as Cedar, Graham, Niagara, etc.)? ######
survey_E5_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E5) %>% 
  unnest(E5) %>% 
  filter(E5 == "Yes" | E5 == "No" | E5 == "Don't know")



E5_summay <- 
  survey_E5_v1 %>% 
  group_by(E5) %>% 
  count()

#### Pie chart #### 
PieDonut(E5_summay, 
         aes(E5, count= n), 
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
  scale_fill_manual(values =  cb_pie2)
### E6 - Do you use research management services (e.g., CCDB, DMP Assistant) provided/supported by the Alliance? ######
survey_E6_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E6) %>% 
  unnest(E6) %>% 
  filter(E6 == "Yes" | E6 == "No" | E6 == "Don't know")



E6_summay <- 
  survey_E6_v1 %>% 
  group_by(E6) %>% 
  count()

#### Pie chart #### 
PieDonut(E6_summay, 
         aes(E6, count= n), 
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
  scale_fill_manual(values =  cb_pie2)
### E7 - Do you use one of the research software platforms (e.g., FRDR, Canadian Writing Research Collaboratory (CWRC), VirusSeq) funded/supported/maintained by the Alliance? ######
survey_E7_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E7) %>% 
  unnest(E7) %>% 
  filter(E7 == "Yes" | E7 == "No" | E7 == "Don't know")



E7_summay <- 
  survey_E7_v1 %>% 
  group_by(E7) %>% 
  count()

#### Pie chart #### 
PieDonut(E7_summay, 
         aes(E7, count= n), 
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
  scale_fill_manual(values =  cb_pie2)


### E8 - What type of software/tools do you most often use within your research project workflows? ######
survey_E8_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E8")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_E8_v1$Question),'s___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_E8_v2 <- cbind(separete_v3, survey_E8_v1)

#Clean the data
survey_E8_v3 <- 
  survey_E8_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Cloud collaboration_platforms__e_g___OSF__", "Cloud collaboration platforms (e.g., OSF)", ifelse(
      Answer_q == "Commercial data_software__e_g___ESRI__NVIVO__Tableau__", "Commercial data software (e.g., ESRI, NVIVO, Tableau)", ifelse(
        Answer_q == "Commercial storage_provider__e_g___Google_Drive__Dropbox__", "Commercial storage provider (e.g., Google Drive, Dropbox)", ifelse(
          Answer_q == "Continuous integration_platforms__e_g___Travis_CI__Circle_CI__GitHub_Actions__", "Continuous integration platforms (e.g., Travis-CI, Circle-CI, GitHub Actions)", ifelse(
            Answer_q == "Electronic lab_notebooks__e_g___LabArchives__", "Electronic lab notebooks (e.g., LabArchives)", ifelse(
              Answer_q == "MS Office_or_equivalent_", "MS Office or equivalent", ifelse(
                Answer_q == "Open source_data_software__e_g___NumPy__Cesium_ML__OpenRefine__", "Open source data software (e.g., NumPy, Cesium ML, OpenRefine)", ifelse(
                  Answer_q == "Project management_tools__e_g___EMDESK__Asana__", "Project management tools (e.g., EMDESK, Asana)", ifelse(
                    Answer_q == "Secure data_capture_software__e_g___REDCap__", "Secure data capture software (e.g., REDCap)", ifelse(
                      Answer_q == "Version control_software__e_g___Git__Subversion__", "Version control software (e.g., Git, Subversion)", ifelse(
                        Answer_q == "None ", "None", "Other"
                        ))))))))))))%>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_E8_v3_tc3 <- 
  survey_E8_v3 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.E8 <- 
  survey_E8_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.E8, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.E8, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#83
nSSH <- filter(Workflow.E8, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #40

Workflow_Health <- filter(Workflow.E8, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E8, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E8, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### E9 - Who on your team supports the research software you typically use? ######
survey_E9_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E9")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_E9_v1$Question),'e___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_E9_v2 <- cbind(separete_v3, survey_E9_v1)

#Clean the data
survey_E9_v3 <- 
  survey_E9_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Me ", "Me", ifelse(
      Answer_q == "A student__undergraduate_graduate__", "A student (undergraduate/graduate)", ifelse(
        Answer_q == "A postdoctoral_fellow_", "A postdoctoral fellow", ifelse(
          Answer_q == "A sysadmin_on_my_team_", "A sysadmin on my team", ifelse(
            Answer_q == "A sysadmin_from_my_institution_", "A sysadmin from my institution", ifelse(
              Answer_q == "A sysadmin_from_the_Alliance_", "A sysadmin from the Alliance", ifelse(
                Answer_q == "Regional Compute_Organization__e_g__Compute_Ontario__Calcul_Québec__", "Regional Compute Organization (e.g. Compute Ontario, Calcul Québec)", "Other"
                ))))))))%>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_E9_v3_tc3 <- 
  survey_E9_v3 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.E9 <- 
  survey_E9_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.E9, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.E9, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#82
nSSH <- filter(Workflow.E9, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #38

Workflow_Health <- filter(Workflow.E9, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E9, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E9, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### E10 - Can you provide an estimate of the time you and your research team spend installing and supporting the research software that you use? ######
survey_E10_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E10") %>% 
  mutate(question_n = ifelse(
    Question == "Can_you_provide_an_estimate_of_the_time_you_and_your_research_team_spend_installing_and_supporting_the_research_software_that_you_use____The_time_you_spend_installing_and_supporting_the_RS_that_you_use__" , "You", "Team"
  )) %>% 
  select(-Question)

#Clean the data
survey_E10_v2 <- 
  survey_E10_v1 %>% 
  drop_na() %>% 
  unnest(Answer) %>% 
  select(-Ques_num)

#"Team" data
E10_team <- 
  survey_E10_v2 %>% 
  filter(question_n == "Team")

#"You" data
E10_you <- 
  survey_E10_v2 %>% 
  filter(question_n == "You")

E10_you <- 
  E10_you %>% 
  filter(!Answer == "Between 1 and 2 FTEs" & !Answer == "More than 2 FTEs")

#Link "Team" to TC3
E10_team.domain <- 
  E10_team %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() 

#Link "You" to TC3
E10_you.domain <- 
  E10_you %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() 

#Link "Team" to role
E10_team_role <- 
  E10_team %>% 
  left_join(survey_B3_v4, by = "Internal.ID") %>% 
  drop_na() 

#Link "You" to B3 (role)
E10_you_role <- 
  E10_you %>% 
  left_join(survey_B3_v4, by = "Internal.ID") %>% 
  drop_na() %>% 
  select(-question_n) %>%
  unique()

##Summarize tables
#summary table - "Team" and TC3
E10_team_summary <- 
  E10_team.domain %>% 
  group_by(Answer, TC3) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", ">2", ifelse(
      Answer == "Between 1 and 2 FTEs", "1 - 2", ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  print()


#summary table - "You" and TC3
E10_you_summary <- 
  E10_you.domain %>% 
  group_by(Answer, TC3) %>% 
  count() %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", "delete", ifelse(
      Answer == "Between 1 and 2 FTEs", "delete", ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  arrange(-n) %>% 
  filter(!answer_n == "delete") %>% 
  print()

#summary table - "Team" and Role
E10_team_Role_summary <- 
  E10_team_role %>% 
  group_by(Answer, Role_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", ">2", ifelse(
      Answer == "Between 1 and 2 FTEs", "1 - 2", ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  print()


#summary table - "You" and Role
E10_you_Role_summary <- 
  E10_you_role %>% 
  group_by(Answer, Role_n) %>% 
  count() %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", "delete", ifelse(
      Answer == "Between 1 and 2 FTEs", "delete", ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>% 
  arrange(-n) %>% 
  filter(!answer_n == "delete") %>% 
  print()

#link to TC3
survey_E10_v3_TC3 <- 
  survey_E10_v2 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() 

#summarize
survey_E10_v3_TC3.summary <- 
  survey_E10_v3_TC3 %>% 
  group_by(question_n, Answer) %>% 
  count() %>% 
  mutate(order = ifelse(
    Answer == "More than 2 FTEs", 5, ifelse(
      Answer == "Between 1 and 2 FTEs", 4, ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", 1, ifelse(
          Answer=="Between 1/2 and 1 FTE",3,2
        ))))) %>% 
  mutate(answer_n = ifelse(
    Answer == "More than 2 FTEs", ">2", ifelse(
      Answer == "Between 1 and 2 FTEs", "1 - 2", ifelse(
        Answer == "Less than 1/4 of a full time equivalent position (an FTE)", "<1/4", ifelse(
          Answer=="Between 1/2 and 1 FTE","1/2 - 1", "1/4 - 1/2"
        ))))) %>%  
  drop_na()

#sum
survey_E10_v3_TC3.sum <- 
  survey_E10_v3_TC3.summary %>% 
  group_by(question_n) %>% 
  summarise(sum = sum(n))

#merge sum table to summary table
survey_E10_v3_TC3.merged <- 
  survey_E10_v3_TC3.summary %>% 
  left_join(survey_E10_v3_TC3.sum, by = "question_n") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
survey_E10_v3_TC3.sum.flip <- survey_E10_v3_TC3.merged %>% 
  mutate(new_n = ifelse(question_n == "Respondant",
                        -1*proportion, proportion))


#### Bar plots#### 

#"Team" and TC3
ggplot(E10_team_summary, aes(fill=TC3, y=n, x= reorder(answer_n, order))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")

#"You" and TC3
ggplot(E10_you_summary, aes(fill=TC3, y=n, x= reorder(answer_n, order))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp1) + 
  guides(fill=guide_legend(title="Tri-agency"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")


#"Team" and Role
ggplot(E10_team_Role_summary, aes(fill=Role_n, y=n, x= reorder(answer_n, order))) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp_Cad) + 
  guides(fill=guide_legend(title="Role"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")

#"You" and Role
ggplot(E10_you_Role_summary, aes(fill=Role_n, y=n, x= reorder(answer_n, order))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =  cbp_Cad) + 
  guides(fill=guide_legend(title="Role"))+
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  xlab("FTE")+
  ylab("n")


### E11 - ow does your group fund the research software that you use? ######
survey_E11_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E11")

#Clean the data
survey_E11_v2 <-
  survey_E11_v1 %>%
  select(-Question) %>%
  drop_na() 

#Link to TC3
survey_E11_V2_tc3 <- 
  survey_E11_v2 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.E11 <- 
  survey_E11_V2_tc3 %>% 
  unique()

nHR <- filter(Workflow.E11, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #31
nSE <- filter(Workflow.E11, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#70
nSSH <- filter(Workflow.E11, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #36

Workflow_Health <- filter(Workflow.E11, TC3=="Health Research") %>%
  group_by(TC3, Answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E11, TC3=="Sciences and Engineering") %>%
  group_by(TC3, Answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E11, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, Answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(Answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")



### E12 - Approximately how many dollars (CDN) were spent personally over the last calendar year on research software, or software used in research (e.g. Adobe Suite, Excel, Dropbox)? ######
survey_E12_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E12) %>% 
  unnest(E12) %>% 
  rename(answer = E12)

#Link to TC3
survey_E12_v1_tc3 <- 
  survey_E12_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() %>% 
  mutate(order = ifelse(
    answer == "$0 (None)", 1, ifelse(
      answer == "$1 to less than $1000", 2, ifelse(
        answer == "$1000 to less than $5000", 3, ifelse(
          answer == "More than $5000", 4, ifelse(
            answer == "Don’t know", 5, 6)))))) %>% 
  filter(!order == 6) %>% 
  print()

#summarize the data
summary_E12_tc3 <- 
  survey_E12_v1_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()


##add percentage
Workflow.E12 <- 
  survey_E12_v1_tc3 %>% 
  unique()

nHR <- filter(Workflow.E12, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #31
nSE <- filter(Workflow.E12, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#80
nSSH <- filter(Workflow.E12, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #39

Workflow_Health <- filter(Workflow.E12, TC3=="Health Research") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E12, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E12, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plot - TC3 #### 
#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### E13 - Approximately how many dollars (CDN) were spent personally over the last calendar year on research software, or software used in research (e.g. Adobe Suite, Excel, Dropbox)? ######
survey_E13_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E13) %>% 
  unnest(E13) %>% 
  rename(answer = E13)

#Link to TC3
survey_E13_v1_tc3 <- 
  survey_E13_v1 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() %>% 
  mutate(order = ifelse(
    answer == "$0 (None)", 1, ifelse(
      answer == "$1 to less than $1000", 2, ifelse(
        answer == "$1000 to less than $5000", 3, ifelse(
          answer == "$5000 to less than $50K", 4, ifelse(
            answer == "$50K to less than $100K", 5, ifelse(
              answer == "$100K or more", 6, ifelse(
                answer == "Don’t know", 7, 8
                )))))))) %>% 
  filter(!order == 8) %>% 
  print()

#summarize the data
summary_E13_tc3 <- 
  survey_E13_v1_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()


##add percentage
Workflow.E13 <- 
  survey_E13_v1_tc3 %>% 
  unique()

nHR <- filter(Workflow.E13, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #21
nSE <- filter(Workflow.E13, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#59
nSSH <- filter(Workflow.E13, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #27

Workflow_Health <- filter(Workflow.E13, TC3=="Health Research") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E13, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E13, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer, order) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plot - TC3 #### 
#Percentage TC3
ggplot(Workflow_Tri2, aes(x=reorder(answer, -order))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### E14 - What factors do you consider in choosing which research software to use? ######
survey_E14_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E14")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_E14_v1$Question),'e_____',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_E14_v2 <- cbind(separete_v3, survey_E14_v1)

#Clean the data
survey_E14_v3 <- 
  survey_E14_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Ease of_use__", "Ease of use", ifelse(
      Answer_q == "Cost _", "Cost", ifelse(
        Answer_q == "Scalability _", "Scalability", ifelse(
          Answer_q == "Performance ___", "Performance", ifelse(
            Answer_q == "Vendor Agnostic_features__", "Vendor Agnostic features", ifelse(
              Answer_q == "Open source_" , "Open source", ifelse(
                Answer_q == "Sustainability _will_be_around_for_a_long_time__", "Sustainability (will be around for a long time)", ifelse(
                  Answer_q == "Reputation __Citations_", "Reputation / Citations", ifelse(
                    Answer_q == "Documentation ", "Documentation", ifelse(
                      Answer_q == "Support ", "Support", ifelse(
                        Answer_q == "Securely handle_data_", "Securely handle data", ifelse(
                          Answer_q == "State of_the_art___leading_edge_", "State-of-the-art / leading edge", ifelse(
                            Answer_q == "Interoperability ", "Interoperability", ifelse(
                              Answer_q == "Other ", "Other", "Delete"
                              ))))))))))))))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#summarize the data
summary_E14<- 
  survey_E14_v3 %>% 
  group_by(answer) %>% 
  count() %>% 
  print()

#Link to TC3
survey_E14_v3_tc3 <- 
  survey_E14_v3 %>% 
  left_join(domain1, by = "Internal.ID")

#summarize the data
summary_E14_TC3<- 
  survey_E14_v3_tc3 %>% 
  group_by(TC3, answer) %>% 
  count() %>% 
  print()

##add percentage
Workflow.E14 <- 
  survey_E14_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.E14, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #33
nSE <- filter(Workflow.E14, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#77
nSSH <- filter(Workflow.E14, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #40

Workflow_Health <- filter(Workflow.E14, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.E14, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.E14, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3#### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

### E15 - Do you list and/or cite the research software you use in your research outputs? ######
survey_E15_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E15) %>% 
  unnest(E15) %>% 
  rename(answer = E15)

#summarize the data
E15_summay <- 
  survey_E15_v1 %>% 
  group_by(answer) %>% 
  count()


#### Pie chart #### 
PieDonut(E15_summay, 
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
  scale_fill_manual(values =  cbp1)


### E16 - How do you cite or reference other people’s research software? ######
survey_E16_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E16")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_E16_v1$Question),'e___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_E16_v2 <- cbind(separete_v3, survey_E16_v1)

survey_E16_v3 <- 
  survey_E16_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Cite or_reference_the_research_software_directly__", "Cite or reference the research software directly", ifelse(
      Answer_q == "Cite or_reference_publication_s__describing_the_research_software_", "Cite or reference publication(s) describing\nthe research software", "Other"))) %>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)



E16_summay <- 
  survey_E16_v3 %>% 
  group_by(answer) %>% 
  count()

#### Pie chart #### 
PieDonut(E16_summay, 
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
  scale_fill_manual(values =  cb_pie1)


### E17 - Do you participate in the ongoing development and/or support of the research software? ######
survey_E17_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E17) %>% 
  unnest(E17) %>% 
  rename(answer = E17)

#summarize the data
E17_summay <- 
  survey_E17_v1 %>% 
  group_by(answer) %>% 
  count()


#### Pie chart #### 
PieDonut(E17_summay, 
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
  scale_fill_manual(values =  cbp1)



### E18 - Are you sometimes prevented from re-using research software because you lack the resources to install, use, or support it? ######
survey_E18_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, E18) %>% 
  unnest(E18) %>% 
  rename(answer = E18) 

survey_E18_v2 <- 
  survey_E18_v1 %>% 
  filter(answer == "Yes" | answer == "No")

survey_E18_v2_open <- 
  survey_E18_v1 %>% 
  filter(!answer == "Yes") %>% 
  filter(!answer == "No")

# write.csv(survey_E18_v2_open, "q18.csv")

#summarize the data
E18_summay <- 
  survey_E18_v2 %>% 
  group_by(answer) %>% 
  count()


#### Pie chart #### 
PieDonut(E18_summay, 
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
  scale_fill_manual(values =  cbp1)


### E20 - Do your research software needs include storing or processing controlled or sensitive data (i.e. data owned by First Nations, Métis, or Inuit communities, personal data, data subject to ethics protocols, a data sharing agreement or specific security requirements)? ######
survey_E20_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "E20") %>% 
  mutate(question_n = ifelse(
    Question == "Do_your_research_software_needs_include_storing_or_processing_controlled_or_sensitive_data__i_e__data_owned_by_First_Nations__Métis__or_Inuit_communities__personal_data__data_subject_to_ethics_protocols__a_data_sharing_agreement_or_specific_security_requirements_____Data_collected_by_or_about_Indigenous_communities_" , 1, ifelse(
      Question == "Do_your_research_software_needs_include_storing_or_processing_controlled_or_sensitive_data__i_e__data_owned_by_First_Nations__Métis__or_Inuit_communities__personal_data__data_subject_to_ethics_protocols__a_data_sharing_agreement_or_specific_security_requirements_____Data_containing_personal_information_about_individuals_", 2, 3
    )
  )) %>% 
  select(-Question)

#Clean the data
survey_E20_v2 <- 
  survey_E20_v1 %>% 
  drop_na() %>% 
  unnest(Answer) %>% 
  select(-Ques_num)

# "Data collected by or about Indigenous communities" = 1
E20_1 <- 
  survey_E20_v2 %>% 
  filter(question_n == 1)

# "Data containing personal information about individuals" = 2 
E20_2 <- 
  survey_E20_v2 %>% 
  filter(question_n == 2)

# "Data requiring a high level of security for other reasons" = 3
E20_3 <- 
  survey_E20_v2 %>% 
  filter(question_n == 3)

#Summarize data

summary_E20_1 <- 
  E20_1 %>% 
  group_by(Answer) %>% count()

summary_E20_2 <- 
  E20_2 %>% 
  group_by(Answer) %>% count()

summary_E20_3 <- 
  E20_3 %>% 
  group_by(Answer) %>% count()

#### Pie charts #### 

#1
PieDonut(summary_E20_1, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie2)

#2
PieDonut(summary_E20_2, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie2)

#3
PieDonut(summary_E20_3, 
         aes(Answer, count= n), 
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
  scale_fill_manual(values =  cb_pie2)

# F ############################################################################################
### F1 - Have you ever received training* in using research software and/or developing research software? ######
survey_F1_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, F1) %>% 
  unnest(F1) %>% 
  rename(answer = F1) 

#summarize the data
F1_summay <- 
  survey_F1_v1 %>% 
  group_by(answer) %>% 
  count()


#### Pie chart #### 
PieDonut(F1_summay, 
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
  scale_fill_manual(values =  cbp1)


### F2 - Who on your team supports the research software you typically use? ######
survey_F2_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "F2")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_F2_v1$Question),'training___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_F2_v2 <- cbind(separete_v3, survey_F2_v1)

#Clean the data
survey_F2_v3 <- 
  survey_F2_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Self taught_", "Self-taught", ifelse(
      Answer_q == "Student led_training_peer_training_", "Student-led training/peer training", ifelse(
        Answer_q == "Extra curricular_courses_" , "Extra-curricular courses", ifelse(
          Answer_q == "Degree in_a_related_domain_", "Degree in a related domain", ifelse(
            Answer_q == "Local regional_service__e_g___through_libraries_or_IT__or_training_workshops_offered_at_local_or_regional_services__", "Local/regional service (e.g., through libraries or IT, or training/workshops\noffered at local or regional services)", ifelse(
              Answer_q == "Online resources__e_g___Software_Carpentry__StackOverflow__", "Online resources (e.g., Software Carpentry, StackOverflow)", "Other"
              )))))))%>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_F2_v3_tc3 <- 
  survey_F2_v3 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.F2 <- 
  survey_F2_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.F2, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #26
nSE <- filter(Workflow.F2, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#60
nSSH <- filter(Workflow.F2, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #31

Workflow_Health <- filter(Workflow.F2, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.F2, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.F2, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")


### F4 - Would you or your team take advantage of courses in the use and development of research software? ######
survey_F4_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, F4) %>% 
  unnest(F4) %>% 
  rename(answer = F4) %>% 
  filter(answer == "Yes" | answer == "No")

#summarize the data
F4_summay <- 
  survey_F4_v1 %>% 
  group_by(answer) %>% 
  count()


#### Pie chart #### 
PieDonut(F4_summay, 
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
  scale_fill_manual(values =  cb_pie1)

### F5 - Are you a member of professional research software development groups such as the Research Software Engineering Association? ######
survey_F5_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, F5) %>% 
  unnest(F5) %>% 
  rename(answer = F5) %>% 
  filter(answer == "Yes" | answer == "No")

#summarize the data
F5_summay <- 
  survey_F5_v1 %>% 
  group_by(answer) %>% 
  count()


#### Pie chart #### 
PieDonut(F5_summay, 
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
  scale_fill_manual(values =  cb_pie1)



### F6 - Would you be interested in joining a research software community, organisation or program if one was formed in Canada (e.g., CANRSe– Canadian Research Software Expert)? ######
survey_F6_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, F6) %>% 
  unnest(F6) %>% 
  filter(F6 == "Yes" | F6 == "No" | F6 == "Maybe")



F6_summay <- 
  survey_F6_v1 %>% 
  group_by(F6) %>% 
  count()

#### Pie chart #### 
PieDonut(F6_summay, 
         aes(F6, count= n), 
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
  scale_fill_manual(values =  cb_pie2)


### F7 - Who on your team supports the research software you typically use? ######
survey_F7_v1 <- 
  survey_organized %>% 
  filter(Ques_num == "F7")

#Split column "Question" into two to separate the question from the answer
separate_v1 <- data.frame(do.call('rbind', strsplit(as.character(survey_F7_v1$Question),'nisation___',fixed=TRUE)))

separete_v2 <- 
  separate_v1 %>% 
  select(X2)

#Delete "_" from answers
separete_v3 <- stringr::str_replace(separete_v2$X2, "_", " ")

#Bin tables
survey_F7_v2 <- cbind(separete_v3, survey_F7_v1)

#Clean the data
survey_F7_v3 <- 
  survey_F7_v2 %>% 
  select(-Question) %>% 
  rename(Answer_q = separete_v3) %>% 
  drop_na() %>% 
  filter(!Answer == "No" | !Answer == NA) %>% 
  mutate(Answer_n = ifelse(
    Answer_q == "Software development_collaboration__", "Software development collaboration", ifelse(
      Answer_q == "Research collaboration_", "Research collaboration", ifelse(
        Answer_q == "Networking " , "Networking", ifelse(
          Answer_q == "Job opportunities_", "Job opportunities", ifelse(
            Answer_q == "Software best_practices__", "Software best practices", ifelse(
              Answer_q == "Software development_support__help_developing_software_for_your_project__", "Software development support (help developing\nsoftware for your project)", ifelse(
                Answer_q == "Training and_education_","Training and education" , "Other"
                ))))))))%>%
  select(Internal.ID, Answer, Answer_n) %>% 
  rename(answer = Answer_n)

#Link to TC3
survey_F7_v3_tc3 <- 
  survey_F7_v3 %>% 
  left_join(domain1, by = "Internal.ID")

##add percentage
Workflow.F7 <- 
  survey_F7_v3_tc3 %>% 
  unique()

nHR <- filter(Workflow.F7, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #24
nSE <- filter(Workflow.F7, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#58
nSSH <- filter(Workflow.F7, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #23

Workflow_Health <- filter(Workflow.F7, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.F7, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.F7, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health) 

#### Bar plots - TC3 #### 
ggplot(Workflow_Tri2, aes(x=reorder(answer, `%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +
  geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 20) +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  # ggtitle("") +
  guides(fill=guide_legend(title="Tri-agency"))+
  xlab("") + 
  ylab("")

