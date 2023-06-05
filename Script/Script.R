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

sort(unique(survey_B3_v1$Role))# to clean the data

survey_B3_v2 <- 
  survey_B3_v1 %>% 
  mutate(Role_n = ifelse(
    Role == "Administratrice ou administrateur", "Administrator", ifelse(
      Role == "Associee ou associe de recherche", "Research Associate", ifelse(
        Role == "Autre", "Other", ifelse(
          Role == "Bibliothecaire", "Librarian", ifelse(
            Role == "Boursiere postdoctorale ou boursier postdoctoral", "Post-Doctoral Fellow", ifelse(
              Role == "Chercheuse ou chercheur", "Researcher", ifelse(
                Role == "Conseillere a la recherche", "Research Staff", ifelse(
                  Role == "Developpeuse ou developpeur de logiciel de recherche", "Research Software Developer", ifelse(
                    Role == "etudiante ou etudiant au doctorat", "Student (Doctoral)", ifelse(
                      Role == "etudiante ou etudiant de premier cycle", "Student (Undergrad)", ifelse(
                        Role == "etudiante ou etudiant diplome", "Student (Graduate)", ifelse(
                          Role == "Ingenieure ou ingenieur en logiciels de recherche / Experte ou expert", "Research Software Engineer / Expert", ifelse(
                            Role == "Personnel de recherche", "Research Staff", ifelse(
                              Role == "Personnel de soutien", "Research Staff", ifelse(
                                Role == "Professeure ou professeur auxiliaire, professeure ou professeur emerite, professeure ou professeur invite, professeure ou professeur avec mandat limite", "Faculty - Adjunct, emeritus, visiting, or limited-term", ifelse(
                                  Role == "Professeure ou professeur, (incluant professeure ou professeur assistant, professeure ou professeur associe, professeure ou professeur titulaire, professeure ou professeur clinique, professeure ou professeur d_enseignement)", "Faculty - Professor (including assistant/associate/full professor, clinical professor, teaching professor)", ifelse(
                                    Role == "staff in research support unit", "Research Staff", Role
                                    ))))))))))))))))))


##From "Other", avoid duplication and select answers

#Add "2" to rows that I want to select
duplications_v1 <- survey_B3_v2 %>% 
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

sort(unique(survey_B7_v1$B7))# to clean the data

survey_B7_v2 <- 
  survey_B7_v1 %>% 
  mutate(B7_n = ifelse(
    B7 == "Je ne sais pas", "Not sure", ifelse(
      B7 == "Non", "No", ifelse(
        B7 == "Oui", "Yes", B7
        ))))

B7_summay <- 
  survey_B7_v2 %>% 
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


### B8 - Quelles sont les sources de financement pour le budget réservé aux logiciels de recherche de votre groupe? ######
survey_B8_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, B8) %>% 
  unnest(B8)

sort(unique(survey_B8_v1$B8))# to clean the data

survey_B8_v2 <- 
  survey_B8_v1 %>% 
  mutate(B8_n = ifelse(
    B8 == "Il n'est pas finance", "It is not funded", ifelse(
      B8 == "Institutionnel", "Institutional", ifelse(
        B8 == "Organisme international", "International funding", ifelse(
          B8 == "Organisme provincial", "Provincial funding", ifelse(
            B8 == "Subvention ou financement de l_industrie", "Industry grants", ifelse(
              B8 == "IRSC", "CIHR", ifelse(
                B8 == "CRSNG", "NSERC", ifelse(
                  B8 == "CRSH", "SSHRC", ifelse(
                    B8 == "FCI", "CFI", B8
                    ))))))))))
