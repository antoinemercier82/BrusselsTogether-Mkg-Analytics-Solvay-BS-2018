library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(lavaan)
library(likert)
library(reshape2)
library(RColorBrewer)
library(ggthemes)
library(tibble)
library(broom)
# update_geom_font_defaults(family = font_rc_light)

# options(device = "CairoWin")



# Data import
survey_df <- read_csv("inputs/Citizen initiative.csv")
header_lookup <- read_delim("inputs/header_lookup.txt", delim = ";")
# names(survey_df) <- header_lookup$df_header


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

# Language preprocessing
lut_lang <- c("English" = "English",
              "Nederlands" = "Dutch",
              "Français" = "French")

survey_df$lang <- lut_lang[survey_df$lang]

# Grouping preprocessing
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

# Entrepreuneurs interest preprocessing
lut_e_int_eng <- c("I am not interested in joining a civic crowdfunding platform." = "Not_interested",
                   "I could be interested to join a civic crowdfunding platform." = "Could_be_interested",
                   "I am ready to join a civic crowdfunding platform." = "Is_ready")

lut_e_int_nl <- c("Ik ben niet geinteresseerd bij het toetreden van een ‘civic crowdfunding’ platform." = "Not_interested",
                  "Ik zou geinteresseerd kunnen zijn in het toetreden van een ‘civic crowdfunding’ platform." = "Could_be_interested",
                  "Ik wil deel uitmaken van een ‘civic crowdfunding’ platform." = "Is_ready")

lut_e_int_fr <- c("Je ne suis pas intéressé.e par l'adhésion à une plateforme de crowdfunding citoyen." = "Not_interested",
                  "Je pourrais être intéressé.e par l'adhésion à une plateforme de crowdfunding citoyen." = "Could_be_interested",
                  "Je suis prêt.e à rejoindre une plateforme de crowdfunding citoyen." = "Is_ready")

survey_df$e_interest_eng <- lut_e_int_eng[survey_df$e_interest_eng]
survey_df$e_interest_nl <- lut_e_int_nl[survey_df$e_interest_nl]
survey_df$e_interest_fr <- lut_e_int_fr[survey_df$e_interest_fr]

# Likert scale questions preprocessing
lut_likert_nl <- c("Helemaal niet akkoord" = "Strongly disagree",
                   "Niet akkoord" = "Disagree",
                   "Geen mening" = "Neither agree nor disagree",
                   "Akkoord" = "Agree",
                   "Helemaal akkoord" = "Strongly agree",
                   "Helemaal niet akkoord" = "Strongly disagree",
                   "niet akkoord" = "Disagree",
                   "geen mening" = "Neither agree nor disagree",
                   "akkoord" = "Agree",
                   "helemaal akkoord" = "Strongly agree")

lut_likert_fr <- c("Pas du tout d'accord" = "Strongly disagree",
                   "Pas d'accord" = "Disagree",
                   "Sans opinion" = "Neither agree nor disagree",
                   "D'accord" = "Agree",
                   "Tout à fait d'accord" = "Strongly agree")

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

# Citizen interest preprocessing
lut_c_int_eng <- c("I am not interested in citizen's initiatives." = "Not_interested",
                   "I could be interested to fund a citizen's initiative." = "Could_be_interested_Fund",
                   "I could be interested to volunteer for a citizen's initiative." = "Could_be_interested_Vol",
                   "I am ready to fund a citizen's initiative." = "Is_ready_Fund",
                   "I am ready to volunteer for a citizen's initiative." = "Is_ready_Vol")

lut_c_int_nl <- c("Ik ben niet geinteresseerd door burgerinitiatieven" = "Not_interested",
                  "Ik zou eventueel geinteresseerd kunnen zijn door het financieren van burgerinitiatieven" = "Could_be_interested_Fund",
                  "Ik zou eventueel geinteresseerd kunnen zijn door het financieel te ondersteunen van burgerinitiatieven" = "Could_be_interested_Fund",
                  "Ik zou eventueel geinteresseerd zijn om een vrijwillige bijdrage te leveren aan een burgerinitiatief" = "Could_be_interested_Vol",
                  "Ik ben klaar om burgerinitiatieven te financieren" = "Is_ready_Fund",
                  "Ik ben klaar om vrijwilig mee te draaien in een burgerinitiatief" = "Is_ready_Vol")

lut_c_int_fr <- c("Je ne suis pas intéressé.e par les initiatives citoyennes" = "Not_interested",
                  "Je pourrais être éventuellement intéressé.e par le financement d'une initiative citoyenne" = "Could_be_interested_Fund",
                  "Je pourrais être éventuellement intéressé.e par une contribution en tant que volontaire dans une initiative citoyenne" = "Could_be_interested_Vol",
                  "Je suis prêt.e à financer une initiative citoyenne" = "Is_ready_Fund",
                  "Je suis prêt.e à contribuer en tant que volontaire dans une initiative citoyenne" = "Is_ready_Vol")

survey_df$c_interest_eng <- lut_c_int_eng[survey_df$c_interest_eng]
survey_df$c_interest_nl <- lut_c_int_nl[survey_df$c_interest_nl]
survey_df$c_interest_fr <- lut_c_int_fr[survey_df$c_interest_fr]

# Citizen already investing question preprocessing
lut_c_invest_eng <- c("0\u20AC" = "0",
                      "1 - 99\u20AC /year" = "1_99",
                      "100 - 199\u20AC /year" = "100_199",
                      "200 - 499\u20AC /year" = "200_499",
                      "500\u20AC + /year" = "500_plus")

lut_c_invest_nl <- c("0€" = "0",
                     "1 - 99€ /jaar" = "1_99",
                     "100 - 199€ /jaar" = "100_199",
                     "200 - 499€ /jaar" = "200_499",
                     "500€ + /jaar" = "500_plus")

lut_c_invest_fr <- c("0€" = "0",
                     "1 - 99€ /an" = "1_99",
                     "100 - 199€ /an" = "100_199",
                     "200 - 499€ /an" = "200_499",
                     "500€ + /an" = "500_plus")

# survey_df$c_invest_eng <- lut_c_invest_eng[survey_df$c_invest_eng]
# survey_df$c_invest_nl <- lut_c_invest_nl[survey_df$c_invest_nl]
# survey_df$c_invest_fr <- lut_c_invest_fr[survey_df$c_invest_fr]

replace_amount <- function(x, a, b) {
  mask <- str_detect(x, pattern = paste0("^", a))
  x[which(mask)] <- b
  return(x)
}

survey_df$c_invest_eng <- replace_amount(survey_df$c_invest_eng,
                                         a = "0",
                                         b = "0")
survey_df$c_invest_eng <- replace_amount(survey_df$c_invest_eng,
                                         a = "1 ",
                                         b = "1_99")
survey_df$c_invest_eng <- replace_amount(survey_df$c_invest_eng,
                                         a = "10",
                                         b = "100_199")
survey_df$c_invest_eng <- replace_amount(survey_df$c_invest_eng,
                                         a = "2",
                                         b = "200_499")
survey_df$c_invest_eng <- replace_amount(survey_df$c_invest_eng,
                                         a = "5",
                                         b = "500_plus")


survey_df$c_invest_nl <- replace_amount(survey_df$c_invest_nl,
                                         a = "0",
                                         b = "0")
survey_df$c_invest_nl <- replace_amount(survey_df$c_invest_nl,
                                         a = "1 ",
                                         b = "1_99")
survey_df$c_invest_nl <- replace_amount(survey_df$c_invest_nl,
                                         a = "10",
                                         b = "100_199")
survey_df$c_invest_nl <- replace_amount(survey_df$c_invest_nl,
                                         a = "2",
                                         b = "200_499")
survey_df$c_invest_nl <- replace_amount(survey_df$c_invest_nl,
                                         a = "5",
                                         b = "500_plus")


survey_df$c_invest_fr <- replace_amount(survey_df$c_invest_fr,
                                         a = "0",
                                         b = "0")
survey_df$c_invest_fr <- replace_amount(survey_df$c_invest_fr,
                                         a = "1 ",
                                         b = "1_99")
survey_df$c_invest_fr <- replace_amount(survey_df$c_invest_fr,
                                         a = "10",
                                         b = "100_199")
survey_df$c_invest_fr <- replace_amount(survey_df$c_invest_fr,
                                         a = "2",
                                         b = "200_499")
survey_df$c_invest_fr <- replace_amount(survey_df$c_invest_fr,
                                         a = "5",
                                         b = "500_plus")



# Contrib neighb question preprocessing
survey_df$ec_neighb_nl <- lut_likert_nl[survey_df$ec_neighb_nl]
survey_df$ec_neighb_fr <- lut_likert_fr[survey_df$ec_neighb_fr]

# Location question preprocessing
lut_loc_nl <- c("Brussel" = "Brussels",
                "Brussels" = "Brussels",
                "Vlanderen" = "Flanders",
                "Vlaanderen" = "Flanders",
                "Wallonië" = "Wallonia")

lut_loc_fr <- c("Bruxelles-Capitale" = "Brussels",
                "Flandre" = "Flanders",
                "Wallonie" = "Wallonia")

survey_df$ec_loc_nl <- lut_loc_nl[survey_df$ec_loc_nl]
survey_df$ec_loc_fr <- lut_loc_fr[survey_df$ec_loc_fr]

# Gender question preprocessing
lut_gen_eng <- c("Female" = "Female",
                 "Male" = "Male",
                 "Other/I prefer keep it for myself" = "Other")

lut_gen_nl <- c("een vrow" = "Female",
                "een man" = "Male",
                "anders/Ik hou het liever voor mezelf." = "Other")

lut_gen_fr <- c("Une femme" = "Female",
                "Un homme" = "Male",
                "Autre/Je préfère le garder pour moi" = "Other")

survey_df$ec_gender_eng <- lut_gen_eng[survey_df$ec_gender_eng]
survey_df$ec_gender_nl <- lut_gen_nl[survey_df$ec_gender_nl]
survey_df$ec_gender_fr <- lut_gen_fr[survey_df$ec_gender_fr]

# Age question preprocessing
lut_age_eng <- c("Under 14 years old." = "below_14",
                 "14-17 years old." = "14_17",
                 "18-24 years old." = "18_24",
                 "25-34 years old." = "25_34",
                 "35-44 years old." = "35_44",
                 "45-54 years old." = "45_54",
                 "55-64 years old." = "55_64",
                 "65-74 years old." = "65_74",
                 "75 years or older." = "75_plus")

lut_age_nl <- c("onder 14 jaar" = "below_14",
                "14-17 jaar" = "14_17",
                "18-24jaar" = "18_24",
                "25-34jaar" = "25_34",
                "35-44jaar" = "35_44",
                "45-54jaar" = "45_54",
                "55-64jaar" = "55_64",
                "65-74jaar" = "65_74",
                "75 jaar of ouder" = "75_plus")

lut_age_fr <- c("En dessous de 14 ans." = "below_14",
                "14-17 ans." = "14_17",
                "18-24 ans." = "18_24",
                "25-34 ans." = "25_34",
                "35-44 ans." = "35_44",
                "45-54 ans." = "45_54",
                "55-64 ans." = "55_64",
                "65-74 ans." = "65_74",
                "75 ans ou plus." = "75_plus")

survey_df$ec_age_eng <- lut_age_eng[survey_df$ec_age_eng]
survey_df$ec_age_nl <- lut_age_nl[survey_df$ec_age_nl]
survey_df$ec_age_fr <- lut_age_fr[survey_df$ec_age_fr]

# Education question preprocessing
lut_edu_eng <- c("Primary school or no schooling completed" = "Primary_or_None",
                 "High-School" = "High_school",
                 "Trade/technical/vocational training." = "Technical",
                 "Bachelor’s degree." = "Bachelor",
                 "Master’s degree." = "Master",
                 "Doctorate degree." = "PhD")

lut_edu_nl <- c("Lagere school of zonder diploma" = "Primary_or_None",
                "Humaniora" = "High_school",
                "Technische Humaniora" = "Technical",
                "Bachelor" = "Bachelor",
                "Master" = "Master",
                "Doctoraat" = "PhD")

lut_edu_fr <- c("Primaire ou sans diplôme" = "Primary_or_None",
                "Diplôme secondaire" = "High_school",
                "Diplôme secondaire technique" = "Technical",
                "Bachelier" = "Bachelor",
                "Master" = "Master",
                "Doctorat" = "PhD")

survey_df$ec_edu_eng <- lut_edu_eng[survey_df$ec_edu_eng]
survey_df$ec_edu_nl <- lut_edu_nl[survey_df$ec_edu_nl]
survey_df$ec_edu_fr <- lut_edu_fr[survey_df$ec_edu_fr]



# Merging columns from the different languages
all_cols <- names(survey_df)
eng_cols <- all_cols[str_detect(all_cols, pattern = "eng$")]
nl_cols <- all_cols[str_detect(all_cols, pattern = "nl$")]
fr_cols <- all_cols[str_detect(all_cols, pattern = "fr$")]

merged_cols <- str_replace(eng_cols,
                           pattern = "_eng$",
                           replacement = "")

temp_df <- tbl_df(data.frame(matrix(nrow = nrow(survey_df),
                             ncol = length(merged_cols))))
colnames(temp_df) <- merged_cols

survey_df <- tbl_df(cbind(survey_df, temp_df))

survey_df[is.na(survey_df)] <- ""

for (i in 1:length(merged_cols)) {
  m <- merged_cols[i]
  e <- eng_cols[i]
  n <- nl_cols[i]
  f <- fr_cols[i]
  survey_df[[m]] <- paste0(survey_df[[e]], survey_df[[n]], survey_df[[f]])
}

survey_df$lang <- factor(survey_df$lang, ordered = TRUE,
                         levels = c("English", "Dutch", "French"))
survey_df$ec_gender <- factor(survey_df$ec_gender, ordered = TRUE,
                              levels = c("Female", "Male", "Other"))
survey_df$ec_gender <- factor(survey_df$ec_gender, ordered = TRUE,
                              levels = c("Female", "Male", "Other"))


# Split survey_df_e/survey_df_c

survey_df_e <- survey_df %>%
  filter(grouping == "Active_member") %>% 
  select(date, lang, grouping:e_g6, ec_neighb:ec_edu)

temp_coln <- str_replace(names(survey_df_e), "^e_", "")
temp_coln <- str_replace(temp_coln, "^ec_", "")
temp_coln <- str_replace(temp_coln, "lang", "language")
names(survey_df_e) <- temp_coln

survey_df_e$age <- factor(survey_df_e$age, ordered = TRUE,
                              levels = c("14_and_below",
                                         "14_17",
                                         "18_24",
                                         "25_34",
                                         "35_44",
                                         "45_54",
                                         "55_64",
                                         "65_74",
                                         "75_plus"))

survey_df_e$loc <- factor(survey_df_e$loc, ordered = TRUE,
                          levels = c("Brussels",
                                     "Flanders",
                                     "Wallonia"))

survey_df_e$edu <- factor(survey_df_e$edu, ordered = TRUE,
                          levels = c("Primary_or_None",
                                     "High_school",
                                     "Technical",
                                     "Bachelor",
                                     "Master",
                                     "PhD"))


survey_df_c <- survey_df %>%
  filter(grouping != "Active_member") %>% 
  select(date, lang, grouping, c_interest:ec_edu)

temp_coln <- str_replace(names(survey_df_c), "^c_", "")
temp_coln <- str_replace(temp_coln, "^ec_", "")
temp_coln <- str_replace(temp_coln, "lang", "language")
names(survey_df_c) <- temp_coln

survey_df_c$age <- factor(survey_df_c$age, ordered = TRUE,
                          levels = c("14_and_below",
                                     "14_17",
                                     "18_24",
                                     "25_34",
                                     "35_44",
                                     "45_54",
                                     "55_64",
                                     "65_74",
                                     "75_plus"))

survey_df_c$loc <- factor(survey_df_c$loc, ordered = TRUE,
                          levels = c("Brussels",
                                     "Flanders",
                                     "Wallonia"))

survey_df_c$edu <- factor(survey_df_c$edu, ordered = TRUE,
                          levels = c("Primary_or_None",
                                     "High_school",
                                     "Technical",
                                     "Bachelor",
                                     "Master",
                                     "PhD"))

survey_df_c$grouping <- factor(survey_df_c$grouping, ordered = TRUE,
                               levels = c("Citizen_not_active",
                                          "Citizen_casual_vol"))

survey_df_c$interest <- factor(survey_df_c$interest, ordered = TRUE,
                                  levels = c("Not_interested",
                                             "Could_be_interested_Vol",
                                             "Could_be_interested_Fund",
                                             "Is_ready_Vol",
                                             "Is_ready_Fund"))
names(survey_df)

survey_df %>%
  filter(grouping != "Active_member") %>% 
  count(c_interest) %>%
  rename(interest_total = n) %>%
  mutate(c_interest = reorder(c_interest, interest_total)) %>%
  ggplot(aes(x = c_interest, y = interest_total)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_ipsum_rc(grid = "X")

survey_df_c %>%
  count(interest) %>%
  rename(interest_total = n) %>%
  mutate(interest = reorder(interest, interest_total)) %>%
  ggplot(aes(x = interest, y = interest_total)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_ipsum_rc(grid = "X") +
  scale_fill_ipsum()

convert_to_fact_lik <- function(df, columns) {
  for (column in columns) {
    df[[column]] <- factor(df[[column]], ordered = TRUE,
                           levels = c("Strongly disagree",
                                      "Disagree",
                                      "Neither agree nor disagree",
                                      "Agree",
                                      "Strongly agree"))
  }
  return(df)
}

e_likert_cols <- names(survey_df_e)[5:(length(survey_df_e) - 5)]
c_likert_cols <- names(survey_df_c)[5:(length(survey_df_c) - 6)]

survey_df_e <- convert_to_fact_lik(survey_df_e, e_likert_cols)
survey_df_c <- convert_to_fact_lik(survey_df_c, c_likert_cols)

survey_df_e <- convert_to_fact_lik(survey_df_e, "neighb")
survey_df_c <- convert_to_fact_lik(survey_df_c, "neighb")

survey_df_c$invest <- factor(survey_df_c$invest, ordered = TRUE,
                             levels = c("", "0", "1_99", "100_199",
                                        "200_499", "500_plus"))

survey_df_e_j <- survey_df_e %>% select(date,
                                        language,
                                        grouping,
                                        loc,
                                        gender,
                                        age,
                                        edu) %>% mutate(resp_type = "Entrepreneur")

survey_df_c_j <- survey_df_c %>% select(date,
                                        language,
                                        grouping,
                                        loc,
                                        gender,
                                        age,
                                        edu) %>% mutate(resp_type = "Citizen")

survey_df_j <- survey_df_e_j %>% bind_rows(survey_df_c_j)

survey_df_j$resp_type <- factor(survey_df_j$resp_type, ordered = TRUE,
                                levels = c("Entrepreneur", "Citizen"))



survey_df_j %>%
  ggplot(aes(x = gender, fill = language)) +
  geom_bar() +
  facet_wrap(~ resp_type) +
  labs(title = "Composition of respondants",
       subtitle = "This is a subtitle") +
  theme_ipsum_rc() +
  scale_fill_ipsum()







lik_plot <- function(t, my_df_names, mylevels, factor_levels, p_title) {
  tab <- rownames_to_column(data.frame(unclass(t)), "rownames_col")
  names(tab) <- my_df_names
  tab$interest <- factor(tab$interest, ordered = T, levels = factor_levels)
  
  numlevels <- length(tab[1,]) - 1
  numcenter <- ceiling(numlevels / 2) + 1
  tab$midvalues <- tab[,numcenter] / 2
  tab2 <- cbind(tab[,1],
                tab[,2:ceiling(numlevels / 2)],
                tab$midvalues,
                tab$midvalues,
                tab[,numcenter:numlevels + 1])
  
  colnames(tab2) <- c("outcome",
                      mylevels[1:floor(numlevels / 2)],
                      "midlow",
                      "midhigh",
                      mylevels[numcenter:numlevels])
  
  numlevels <- length(mylevels) + 1
  point1 <- 2
  point2 <- ((numlevels) / 2) + 1
  point3 <- point2 + 1
  point4 <- numlevels + 1
  mymin <- (ceiling(max(rowSums(tab2[,point1:point2])) * 4) / 4) * -100
  mymax <- (ceiling(max(rowSums(tab2[,point3:point4])) * 4) / 4) * 100
  
  numlevels <- length(tab[1,]) - 1
  temp.rows <- length(tab2[,1])
  pal <- brewer.pal((numlevels - 1),"BrBG")
  pal[ceiling(numlevels / 2)] <- "#DFDFDF"
  legend.pal <- pal
  pal <- c(pal[1:(ceiling(numlevels / 2) - 1)],
           pal[ceiling(numlevels / 2)],
           pal[ceiling(numlevels / 2)],
           pal[(ceiling(numlevels / 2) + 1):(numlevels - 1)])
  
  tab3 <- melt(tab2, id = "outcome")
  tab3$col <- rep(pal, each = temp.rows)
  tab3$value <- tab3$value * 100
  tab3$outcome <- str_wrap(tab3$outcome, width = 60)
  tab3$outcome <- factor(tab3$outcome, ordered = T,
                         levels = str_wrap(factor_levels, width = 60))
  highs <- na.omit(tab3[(length(tab3[,1]) / 2) + 1:length(tab3[,1]),])
  lows <- na.omit(tab3[1:(length(tab3[,1]) / 2),])
  lows <- lows[rev(rownames(lows)),]
  
  highs$col <- factor(highs$col, ordered = T, levels = c("#018571",
                                                         "#80CDC1",
                                                         "#DFDFDF"))
  lows$col <- factor(lows$col, ordered = T, levels = c("#A6611A",
                                                       "#DFC27D",
                                                       "#DFDFDF"))
  
  highs$outcome <- factor(highs$outcome, ordered = T,
                          levels = rev(levels(highs$outcome)))
  lows$outcome <- factor(lows$outcome, ordered = T,
                         levels = rev(levels(lows$outcome)))
  
  # lows$outcome
  # highs$outcome <- factor(highs$outcome, ordered = T,
  #                        levels = tab2$outcome[order(-(tab2[,5] + tab2[,6] + tab2[,7]))])
  
  ggplot() +
    geom_bar(data = highs, aes(x = outcome, y = value, fill = col), position = "stack", stat = "identity") +
    geom_bar(data = lows, aes(x = outcome, y = -value, fill = col), position = "stack", stat = "identity") +
    geom_hline(yintercept = 0, color = c("white")) +
    scale_fill_identity("Percent", labels = mylevels, breaks = legend.pal, guide = "legend") + 
    theme_fivethirtyeight() +
    coord_flip() +
    theme_ipsum_rc() +
    labs(title = p_title, y = "",x = "") +
    theme(plot.title = element_text(size = 14, hjust = 0.5)) +
    theme(axis.text.y = element_text(hjust = 0)) +
    theme(legend.position = "bottom") +
    scale_y_continuous(breaks = seq(-100,100,25), limits = c(-100,100), labels = function(x) paste0(x, "%")) +
    theme(plot.margin = unit(c(1,1,1,0), "cm")) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
}



header_lookup$df_header
which(header_lookup$df_header == "e_p1_eng")
which(header_lookup$df_header == "e_g6_eng")

which(names(survey_df_e) == "p1")
which(names(survey_df_e) == "g6")

questions_e <- header_lookup$google_f_header[5:15]
quest_code_e <- names(survey_df_e)[5:15]
survey_df_e_df <- as.data.frame(survey_df_e)
names(survey_df_e_df)[5:15] <- questions_e




header_lookup$df_header
which(header_lookup$df_header == "c_p1_eng")
which(header_lookup$df_header == "c_g6_eng")

which(names(survey_df_c) == "p1")
which(names(survey_df_c) == "g6")

questions_c <- header_lookup$google_f_header[17:27]
quest_code_c <- names(survey_df_c)[5:15]
survey_df_c_df <- as.data.frame(survey_df_c)
names(survey_df_c_df)[5:15] <- questions_c

my_df_names <- c("interest", "Strongly disagree", "Disagree", "Neither agree nor disagree",
                 "Agree", "Strongly agree")
mylevels <- c("Strongly disagree", "Disagree", "Neither agree nor disagree",
              "Agree", "Strongly agree")
factor_levels <- c("Not_interested",
                   "Could_be_interested_Vol",
                   "Could_be_interested_Fund",
                   "Is_ready_Vol",
                   "Is_ready_Fund")




my_df_names_c_p <- c("interest", "Strongly disagree", "Disagree", "Neither agree nor disagree",
                     "Agree", "Strongly agree")
mylevels_c_p <- c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                  "Agree", "Strongly agree")
factor_levels_c_p <- c("Personally funding a citizen's initiative is enough to satisfy my desire to engage in this project.",
                       "I would like to participate in debates on local public issues.",
                       "It is relatively easy for citizens to find places where local public issues are collectively debated.",
                       "I would be more inclined to contribute financially to a project on a civic crowdfunding platform if I had the possibility to express my disagreement about some aspects of the project.",
                       "Face-to-face interactions are necessary for collective action.")

temp_c_p <- survey_df_c_df[,5:9]

temp_c_p_gathered <- temp_c_p %>% 
  gather(key = "question", value = "answer")

temp_c_p_gathered$answer <- factor(temp_c_p_gathered$answer, ordered = T,
                                   levels = mylevels_c_p)

t_c_p <- prop.table(table(temp_c_p_gathered$question,
                          temp_c_p_gathered$answer), 1)
c_p_title <- "Citizens - Participative questions\n"






my_df_names_c_g <- c("interest", "Strongly disagree", "Disagree", "Neither agree nor disagree",
                 "Agree", "Strongly agree")
mylevels_c_g <- c("Strongly disagree", "Disagree", "Neither agree nor disagree",
              "Agree", "Strongly agree")
factor_levels_c_g <- c("Civic crowdfunding represents a direct threat to public funding of services.",
                       "I regard crowdfunded citizen's initiatives as a sort of _Do-It-Yourself government_.",
                       "The local government is able to identify the projects that are most wanted by citizens.",
                       "Civic crowdfunding platforms could play a role of intermediary between citizen's initiatives and the local government.",
                       "General interest issues are addressed more efficiently by crowdfunded citizen's initiatives than by the local government.",
                       "General interest issues are addressed more efficiently by crowdfunded citizen's initiatives than by non-profit organizations subsidized by the local government.")

temp_c_g <- survey_df_c_df[,10:15]

temp_c_g_gathered <- temp_c_g %>% 
  gather(key = "question", value = "answer")

temp_c_g_gathered$answer <- factor(temp_c_g_gathered$answer, ordered = T,
                                   levels = mylevels_c_g)

t_c_g <- prop.table(table(temp_c_g_gathered$question,
                          temp_c_g_gathered$answer), 1)
c_g_title <- "Citizens - Government questions\n"



# tab <- rownames_to_column(data.frame(unclass(t_c_g)), "rownames_col")
# names(tab) <- my_df_names_c_g
# tab$interest <- factor(tab$interest, ordered = T, levels = factor_levels_c_g)
# 
# numlevels <- length(tab[1,]) - 1
# numcenter <- ceiling(numlevels / 2) + 1
# tab$midvalues <- tab[,numcenter] / 2
# tab2 <- cbind(tab[,1],
#               tab[,2:ceiling(numlevels / 2)],
#               tab$midvalues,
#               tab$midvalues,
#               tab[,numcenter:numlevels + 1])
# 
# colnames(tab2) <- c("outcome",
#                     mylevels_c_g[1:floor(numlevels / 2)],
#                     "midlow",
#                     "midhigh",
#                     mylevels_c_g[numcenter:numlevels])
# 
# numlevels <- length(mylevels_c_g) + 1
# point1 <- 2
# point2 <- ((numlevels) / 2) + 1
# point3 <- point2 + 1
# point4 <- numlevels + 1
# mymin <- (ceiling(max(rowSums(tab2[,point1:point2])) * 4) / 4) * -100
# mymax <- (ceiling(max(rowSums(tab2[,point3:point4])) * 4) / 4) * 100
# 
# numlevels <- length(tab[1,]) - 1
# temp.rows <- length(tab2[,1])
# pal <- brewer.pal((numlevels - 1),"BrBG")
# pal[ceiling(numlevels / 2)] <- "#DFDFDF"
# legend.pal <- pal
# pal <- c(pal[1:(ceiling(numlevels / 2) - 1)],
#          pal[ceiling(numlevels / 2)],
#          pal[ceiling(numlevels / 2)],
#          pal[(ceiling(numlevels / 2) + 1):(numlevels - 1)])
# 
# tab3 <- melt(tab2, id = "outcome")
# tab3$col <- rep(pal, each = temp.rows)
# tab3$value <- tab3$value * 100
# tab3$outcome <- str_wrap(tab3$outcome, width = 60)
# tab3$outcome <- factor(tab3$outcome, ordered = T,
#                        levels = str_wrap(factor_levels_c_g, width = 60))
# highs <- na.omit(tab3[(length(tab3[,1]) / 2) + 1:length(tab3[,1]),])
# lows <- na.omit(tab3[1:(length(tab3[,1]) / 2),])
# lows <- lows[rev(rownames(lows)),]
# 
# highs$col <- factor(highs$col, ordered = T, levels = c("#018571",
#                                                        "#80CDC1",
#                                                        "#DFDFDF"))
# lows$col <- factor(lows$col, ordered = T, levels = c("#A6611A",
#                                                      "#DFC27D",
#                                                      "#DFDFDF"))
# # lows$outcome
# # highs$outcome <- factor(highs$outcome, ordered = T,
# #                        levels = tab2$outcome[order(-(tab2[,5] + tab2[,6] + tab2[,7]))])
# 
# highs$outcome <- factor(highs$outcome, ordered = T,
#                         levels = rev(levels(highs$outcome)))
# lows$outcome <- factor(lows$outcome, ordered = T,
#                         levels = rev(levels(lows$outcome)))
# 
# ggplot() +
#   geom_bar(data = highs, aes(x = outcome, y = value, fill = col), position = "stack", stat = "identity") +
#   geom_bar(data = lows, aes(x = outcome, y = -value, fill = col), position = "stack", stat = "identity") +
#   geom_hline(yintercept = 0, color = c("white")) +
#   scale_fill_identity("Percent", labels = mylevels_c_g, breaks = legend.pal, guide = "legend") + 
#   theme_fivethirtyeight() +
#   coord_flip() +
#   theme_ipsum_rc() +
#   labs(title = c_g_title, y = "",x = "") +
#   theme(plot.title = element_text(size = 14, hjust = 0.5)) +
#   theme(axis.text.y = element_text(hjust = 0)) +
#   theme(legend.position = "bottom") +
#   scale_y_continuous(breaks = seq(-100,100,25), limits = c(-100,100), labels = function(x) paste0(x, "%")) +
#   theme(plot.margin = unit(c(1,1,1,0), "cm")) +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank())



lik_plot(t_c_g, my_df_names_c_g, mylevels_c_g, factor_levels_c_g, c_g_title)



survey_df_c %>%
  count(interest) %>%
  rename(interest_count = n) %>%
  mutate(interest = reorder(interest, interest_count)) %>%
  ggplot(aes(x = interest, y = interest_count)) +
  geom_col() +
  theme_ipsum_rc(grid = "X") +
  scale_fill_ipsum() +
  coord_flip() +
  labs(title = "Distribution of responses to interest question", x = "") +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,0), "cm"))



my_df_names <- c("interest", "Strongly disagree", "Disagree", "Neither agree nor disagree",
                 "Agree", "Strongly agree")
mylevels <- c("Strongly disagree", "Disagree", "Neither agree nor disagree",
              "Agree", "Strongly agree")
factor_levels <- c("Not_interested",
                   "Could_be_interested_Vol",
                   "Could_be_interested_Fund",
                   "Is_ready_Vol",
                   "Is_ready_Fund")

t_p1 <- prop.table(table(survey_df_c$interest, survey_df_c$p1), 1)
p1_title <- "Personally funding a citizen's initiative is enough to\nsatisfy my desire to engage in this project.\n"

lik_plot(t_p1, my_df_names, mylevels, factor_levels, p1_title)

t_p2 <- prop.table(table(survey_df_c$interest, survey_df_c$p2), 1)
p2_title <- "I would like to participate in debates on local public issues.\n"

lik_plot(t_p2, my_df_names, mylevels, factor_levels, p2_title)

t_p3 <- prop.table(table(survey_df_c$interest, survey_df_c$p3), 1)
p3_title <- "It is relatively easy for citizens to find places where\nlocal public issues are collectively debated.\n"

lik_plot(t_p3, my_df_names, mylevels, factor_levels, p3_title)

t_p4 <- prop.table(table(survey_df_c$interest, survey_df_c$p4), 1)
p4_title <- "I would be more inclined to contribute financially to\na project on a civic crowdfunding platform if I had the possibility\nto express my disagreement about some aspects of the project.\n"

lik_plot(t_p4, my_df_names, mylevels, factor_levels, p4_title)

t_p5 <- prop.table(table(survey_df_c$interest, survey_df_c$p5), 1)
p5_title <- "Face-to-face interactions are necessary for collective action.\n"

lik_plot(t_p5, my_df_names, mylevels, factor_levels, p5_title)



t_g1 <- prop.table(table(survey_df_c$interest, survey_df_c$g1), 1)
g1_title <- "Civic crowdfunding represents a direct threat to public funding of services.\n"

lik_plot(t_g1, my_df_names, mylevels, factor_levels, g1_title)

t_g2 <- prop.table(table(survey_df_c$interest, survey_df_c$g2), 1)
g2_title <- "I regard crowdfunded citizen's initiatives as\na sort of _Do-It-Yourself government_.\n"

lik_plot(t_g2, my_df_names, mylevels, factor_levels, g2_title)

t_g3 <- prop.table(table(survey_df_c$interest, survey_df_c$g3), 1)
g3_title <- "The local government is able to identify\nthe projects that are most wanted by citizens.\n"

lik_plot(t_g3, my_df_names, mylevels, factor_levels, g3_title)

t_g4 <- prop.table(table(survey_df_c$interest, survey_df_c$g4), 1)
g4_title <- "Civic crowdfunding platforms could play a role of\nintermediary between citizen's initiatives and the local government.\n"

lik_plot(t_g4, my_df_names, mylevels, factor_levels, g4_title)

t_g5 <- prop.table(table(survey_df_c$interest, survey_df_c$g5), 1)
g5_title <- "General interest issues are addressed more efficiently\nby crowdfunded citizen's initiatives than by the local government.\n"

lik_plot(t_g5, my_df_names, mylevels, factor_levels, g5_title)

t_g6 <- prop.table(table(survey_df_c$interest, survey_df_c$g6), 1)
g6_title <- "General interest issues are addressed more efficiently\nby crowdfunded citizen's initiatives than by\nnon-profit organizations subsidized by the local government.\n"

lik_plot(t_g6, my_df_names, mylevels, factor_levels, g6_title)



survey_df_c %>%
  filter(invest != "") %>% 
  count(invest) %>%
  rename(invest_count = n) %>%
  mutate(invest = reorder(invest, invest_count)) %>%
  ggplot(aes(x = invest, y = invest_count)) +
  geom_col() +
  theme_ipsum_rc(grid = "X") +
  scale_fill_ipsum() +
  coord_flip() +
  labs(title = "Distribution of responses to past investment question", x = "") +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,0), "cm"))



my_df_names_c_inv <- c("interest", "Strongly disagree", "Disagree", "Neither agree nor disagree",
                 "Agree", "Strongly agree")
mylevels_c_inv <- c("Strongly disagree", "Disagree", "Neither agree nor disagree",
              "Agree", "Strongly agree")
factor_levels_c_inv <- c("0",
                         "1_99",
                         "100_199",
                         "200_499",
                         "500_plus")

survey_df_c_inv <- survey_df_c %>% 
  filter(invest != "")

t_p1 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$p1), 1)
p1_title <- "Personally funding a citizen's initiative is enough to\nsatisfy my desire to engage in this project.\n"

lik_plot(t_p1, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, p1_title)

t_p2 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$p2), 1)
p2_title <- "I would like to participate in debates on local public issues.\n"

lik_plot(t_p2, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, p2_title)

t_p3 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$p3), 1)
p3_title <- "It is relatively easy for citizens to find places where\nlocal public issues are collectively debated.\n"

lik_plot(t_p3, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, p3_title)

t_p4 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$p4), 1)
p4_title <- "I would be more inclined to contribute financially to\na project on a civic crowdfunding platform if I had the possibility\nto express my disagreement about some aspects of the project.\n"

lik_plot(t_p4, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, p4_title)

t_p5 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$p5), 1)
p5_title <- "Face-to-face interactions are necessary for collective action.\n"

lik_plot(t_p1, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, p1_title)




t_g1 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$g1), 1)
g1_title <- "Civic crowdfunding represents a direct threat to public funding of services.\n"

lik_plot(t_g1, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, g1_title)

t_g2 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$g2), 1)
g2_title <- "I regard crowdfunded citizen's initiatives as\na sort of _Do-It-Yourself government_.\n"

lik_plot(t_g2, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, g2_title)

t_g3 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$g3), 1)
g3_title <- "The local government is able to identify\nthe projects that are most wanted by citizens.\n"

lik_plot(t_g3, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, g3_title)

t_g4 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$g4), 1)
g4_title <- "Civic crowdfunding platforms could play a role of\nintermediary between citizen's initiatives and the local government.\n"

lik_plot(t_g4, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, g4_title)

t_g5 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$g5), 1)
g5_title <- "General interest issues are addressed more efficiently\nby crowdfunded citizen's initiatives than by the local government.\n"

lik_plot(t_p1, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, p1_title)

t_g6 <- prop.table(table(survey_df_c_inv$invest, survey_df_c_inv$g6), 1)
g6_title <- "General interest issues are addressed more efficiently\nby crowdfunded citizen's initiatives than by\nnon-profit organizations subsidized by the local government.\n"

lik_plot(t_g6, my_df_names_c_inv, mylevels_c_inv,
         factor_levels_c_inv, g6_title)













# Confirmatory Factor Analysis

meta <- data_frame(question = header_lookup$google_f_header[17:27],
                   variable = names(survey_df_c)[5:15])

meta

survey_df_c_mat <- survey_df_c %>% 
  filter(gender %in% c("Female", "Male")) %>% 
  select(language:edu) %>% 
  data.matrix() %>% 
  as_data_frame() %>% 
  mutate(male = as.numeric(gender == 2))

survey_df_c_mat[1:10,]

model <- "
partcip_will =~ p2 + p1 + p3 + p4 + p5
civic_crowd_gov =~ g4 + g1 + g2 + g3 + g5 + g6
"

cat(model)

fit <- cfa(model = model, data = survey_df_c_mat)

summary(fit)

summary(fit, standardized = TRUE)

parameterestimates(fit)

sfit <- standardizedsolution(fit)

sfit %>% 
  filter(op == "=~")

x <- fitmeasures(fit, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))

cbind(read.table(text = names(x)), x)

# %>% 
#   as_data_frame()

inspect(fit, "r2")

inspect(fit, "sampstat")

fit2 <- cfa(model = model, data = survey_df_c_mat,
           orthogonal = TRUE)

summary(fit2, standardized = TRUE)

parameterestimates(fit2)

sfit3 <- standardizedsolution(fit2)

sfit3 %>% 
  filter(op == "=~")

fitmeasures(fit2, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))

inspect(fit2, "r2")

inspect(fit2, "sampstat")

anova(fit, fit2)

modificationindices(fit) %>% 
  arrange(desc(mi)) %>% 
  head(10)

modificationindices(fit2) %>% 
  arrange(desc(mi)) %>% 
  head(10)

model3 <- "
partcip_will =~ p2 + p1 + p3 + p4 + p5
civic_crowd_gov =~ g4 + g1 + g2 + g3 + g5 + g6
p1 ~~ p5
"

model4 <- "
partcip_will =~ p2 + p1 + p3 + p4 + p5
civic_crowd_gov =~ g4 + g1 + g2 + g3 + g5 + g6
p2 ~~ p4
"

cat(model3)

fit3 <- cfa(model = model3, data = survey_df_c_mat)

summary(fit3, standardized = TRUE)

parameterestimates(fit3)

sfit3 <- standardizedsolution(fit3)

sfit3 %>% 
  filter(op == "=~")

fitmeasures(fit3, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))

inspect(fit3, "r2")

inspect(fit3, "sampstat")

anova(fit, fit3)


fit4 <- cfa(model = model4, data = survey_df_c_mat,
            orthogonal = TRUE)

summary(fit4, standardized = TRUE)

parameterestimates(fit4)

sfit4 <- standardizedsolution(fit4)

sfit4 %>% 
  filter(op == "=~")

fitmeasures(fit4, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))

inspect(fit4, "r2")

inspect(fit4, "sampstat")

anova(fit2, fit4)

x <- fitmeasures(fit, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))

options(scipen = 999, digits = 3) # Print fewer digits

cbind(read.table(text = names(x)), round(x, 3))

  
fits <- list(fit = fit, fit3 = fit3, fit4 = fit4)

round(sapply(fits, function(X) 
  fitmeasures(X, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))), 3)


pfit3 <- data.frame(predict(fit3))
survey_df_c_mat2 <- cbind(survey_df_c_mat, pfit3)

head(survey_df_c_mat2)

survey_df_c_mat2 %>% 
  ggplot(aes(x = partcip_will, y = civic_crowd_gov)) +
  geom_point()

nrow(survey_df_c)
nrow(survey_df_c_mat2)
nrow(pfit3)

survey_df_c <- survey_df_c %>% 
  filter(gender %in% c("Female", "Male")) %>% 
  cbind(partcip_will = survey_df_c_mat2$partcip_will,
        civic_crowd_gov = survey_df_c_mat2$civic_crowd_gov)

survey_df_c %>% 
  ggplot(aes(y = partcip_will, x = gender, fill = gender)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw particip_will and gender",
       subtitle = "This is a subtitle") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

survey_df_c %>% 
  ggplot(aes(y = partcip_will, x = age, fill = age)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw particip_will and age",
       subtitle = "This is a subtitle") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())



survey_df_c %>% 
  ggplot(aes(y = civic_crowd_gov, x = gender, fill = gender)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw civic_crowd_gov and gender",
       subtitle = "This is a subtitle") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

survey_df_c %>% 
  ggplot(aes(y = civic_crowd_gov, x = age, fill = age)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw civic_crowd_gov and age",
       subtitle = "This is a subtitle") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


survey_df_c %>% 
  ggplot(aes(y = partcip_will, x = interest, fill = interest)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw particip_will and interest",
       x = "") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  coord_flip() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,0), "cm"))

survey_df_c %>% 
  # factor(interest, ordered = T,
  #        levels = rev(levels(survey_df_c$interest))) %>% 
  ggplot(aes(y = civic_crowd_gov, x = interest, fill = interest)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw civic_crowd_gov and interest",
       x = "") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  coord_flip() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,0), "cm"))
survey_df_c %>% 
  ggplot(aes(y = partcip_will, x = invest, fill = invest)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw particip_will and past investment",
       subtitle = "This is a subtitle") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  coord_flip()



survey_df_c %>% 
  ggplot(aes(y = civic_crowd_gov, x = invest, fill = invest)) +
  geom_boxplot(alpha = .5) +
  labs(title = "Relationship btw civic_crowd_gov and past investment",
       subtitle = "This is a subtitle") +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  coord_flip()

sem_model <- "
partcip_will =~ p2 + p1 + p3 + p4 + p5
civic_crowd_gov =~ g4 + g1 + g2 + g3 + g5 + g6
p1 ~~ p5
interest ~ partcip_will
interest ~ civic_crowd_gov
"
cat(sem_model)

semfit <- sem(sem_model, survey_df_c_mat)

summary(semfit)

p_semfit <- parameterestimates(semfit)

p_semfit %>% 
  filter(op == "~")


