library(readr)
library(lubridate)
library(stringr)
library(dplyr)

options(device = "CairoWin")



#Data import
survey_df <- read_csv("Citizen initiative.csv")
survey_df

header_lookup <- read_delim("header_lookup.txt", delim = ";")
names(header_lookup)

names(survey_df) <- header_lookup$df_header



# Dates preprocessing
survey_df$date <- str_replace(string = survey_df$date,
                              pattern = "M GMT.*$",
                              replacement = "M")

survey_df$date <- str_replace_all(string = survey_df$date,
                                  pattern = "/",
                                  replacement = "-")

survey_df$date <- ymd_hms(survey_df$date)

survey_df <- survey_df %>% 
                filter(date > "2018-04-27 20:00:00")

#grouping preprocessing
lut_group_eng <- c("Citizen with no active implication in a local citizen's initiative, social project or social organization." = "Citizen_not_active",
                   "Active member of a local citizen's initiative, social project or social organization." = "Active_member",
                   "Citizen with casual volonteering for a local citizen's initiative, social project or social organization." = "Citizen_casual_vol")

lut_group_nl <- c("Een burger die geen deel uitmaakt van een lokaal burgerinitiatief, een sociaal project of een sociale organisatie." = "Citizen_not_active",
                  "Een burger die occasioneel bijdraagt als vrijliger van een lokaal burgerinitiatief, een sociaal project of een sociale organisatie." = "Citizen_casual_vol",
                  "Een actief lid van een lokaal burgerinitiatief, van een sociaal project of een sociale organisatie." = "Active_member")

lut_group_fr <- c("Citoyen.ne sans appartenance à une initiative citoyenne locale, un projet social ou une organisation sociale." = "Citizen_not_active",
                  "Citoyen.ne contribuant de manière occasionnelle en tant que volontaire à une initiative citoyenne locale, un projet social ou une organisation sociale." = "Citizen_casual_vol",
                  "Membre actif d'une initiative citoyenne locale, d'un projet social ou d'une organisation sociale." = "Active_member")


survey_df$grouping_eng <- lut_group_eng[survey_df$grouping_eng]
survey_df$grouping_nl <- lut_group_nl[survey_df$grouping_nl]
survey_df$grouping_fr <- lut_group_fr[survey_df$grouping_fr]

#entrepreuneurs interest preprocessing
lut_int_eng <- c("I am not interested in joining a civic crowdfunding platform." = "Not_interested",
                 "I could be interested to join a civic crowdfunding platform." = "Could_be_interested",
                 "I am ready to join a civic crowdfunding platform." = "Is_ready")

lut_int_nl <- c("Ik ben niet geinteresseerd bij het toetreden van een ‘civic crowdfunding’ platform." = "Not_interested",
                "Ik zou geinteresseerd kunnen zijn in het toetreden van een ‘civic crowdfunding’ platform." = "Could_be_interested",
                "Ik wil deel uitmaken van een ‘civic crowdfunding’ platform." = "Is_ready")

lut_int_fr <- c("Je ne suis pas intéressé.e par l'adhésion à une plateforme de crowdfunding citoyen." = "Not_interested",
                "Je pourrais être intéressé.e par l'adhésion à une plateforme de crowdfunding citoyen." = "Could_be_interested",
                "Je suis prêt.e à rejoindre une plateforme de crowdfunding citoyen." = "Is_ready")

survey_df$e_interest_eng <- lut_int_eng[survey_df$e_interest_eng]
survey_df$e_interest_nl <- lut_int_nl[survey_df$e_interest_nl]
survey_df$e_interest_fr <- lut_int_fr[survey_df$e_interest_fr]

#Likert scales preprocessing
unique(survey_df$e_p1_eng)

lut_likert_nl <- c("Helemaal niet akkoord" = "Strongly disagree",
                   "Niet akkoord" = "Disagree",
                   "Geen mening" = "Neither agree nor disagree",
                   "Akkoord" = "Agree",
                   "Helemaal akkoord" = "Strongly agree")

lut_likert_fr <- c("Pas du tout d'accord" = "Strongly disagree",
                   "Pas d'accord" = "Disagree",
                   "Sans opinion" = "Neither agree nor disagree",
                   "D'accord" = "Agree",
                   "Tout à fait d'accord" = "Strongly agree")

#Likert scale questions preprocessing
survey_df[] <- lapply(colnames(survey_df),
                      function(x) {
                        y <- survey_df[[x]]
                        if (str_detect(x, pattern = "^._.[0-9]_nl$")) {
                          y <- lut_likert_nl[y]
                          }
                        return(y)
                        })

survey_df[] <- lapply(colnames(survey_df),
                      function(x) {
                        y <- survey_df[[x]]
                        if (str_detect(x, pattern = "^._.[0-9]_fr$")) {
                          y <- lut_likert_fr[y]
                        }
                        return(y)
                      })



