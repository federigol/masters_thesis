#0. Load packages -------------------------------------------------------------------

packages <- c("tidyverse", "plyr", "reshape2",
              "scales", "readxl", "Rmisc", "xlsx",
              "ggthemes", "eeptools", "splitstackshape",
              "stringr", "plm", "ggplot2", "likert", "mgsub")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}

library(tidyverse)
library(plyr)
library(reshape2)
library(scales)
library(readxl)
library(Rmisc)
library(xlsx)
library(ggthemes)
library(eeptools)
library(splitstackshape)
library(stringr)
library(plm)
library(ggplot2)
library(likert)
library(mgsub)

#1. Import and merge data ---------------------------------------------------

#Upload database, split assets column, delete incorrect birth dates
database <- read.csv("database.csv")
asset_vars <- c("assets1", "assets2","assets3","assets4","assets5","assets6","assets7","assets8","assets9","assets10", "assets11", "assets12", "assets13")
database <- separate(database, assets, asset_vars, sep = ",")
database <- subset(database, date_of_birth != "2003-01-01" & date_of_birth != "2004-01-01" & date_of_birth != "2005-01-01" & date_of_birth != "2007-01-01" & date_of_birth != "2008-01-01" & date_of_birth != "2010-01-01" & date_of_birth != "2017-01-01" & date_of_birth != "2018-01-01" & date_of_birth != "2019-01-01" & date_of_birth != "1788-01-01" & date_of_birth != "1886-01-01" & date_of_birth != "1866-01-01" & date_of_birth != "1884-01-01" & date_of_birth != "1889-01-01" & date_of_birth != "1893-01-01" & date_of_birth != "1900-01-01" & date_of_birth != "2096-01-01")

#Exclude people who were recruited before 2018, who are younger than 18, have education level <7 and do not own a smartphone
sample <- subset(database, date_of_birth != "2003-01-01" & date_of_birth != "2004-01-01" & date_of_birth != "2007-01-01" & date_of_birth != "2008-01-01" & date_of_birth != "2010-01-01" & date_of_birth != "2017-01-01" & date_of_birth != "2018-01-01" & date_of_birth != "2019-01-01" & date_of_birth != "1788-01-01" & date_of_birth != "1886-01-01")
sample <- subset(sample, recruitment_start != "2017-08-24 00:00:00+00:00")
sample <- subset(sample, education != "Standard 8" & education != "Primary" & education != "Form 1" & education != "Form 2" & education != "Form 3" & education != "Form 4" & education != "Form 5" & education != "Form 6" & education != "Form 7" & education != "Illiterate" & education != "Pre-School" & education != "Unwilling to Say" & education != "" & education != "None")
sample <- subset(sample, assets1 == "Smartphone" | assets2 == "Smartphone" | assets3 == "Smartphone" | assets4 == "Smartphone" | assets5 == "Smartphone" | assets6 == "Smartphone" | assets7 == "Smartphone" | assets8 == "Smartphone")

#Rename values
sample$education <- as.character(mapvalues(sample$education, from = c("Secondary", "Completed Secondary ", "High School", "Graduation", "Masters", "College", "University"), to = c("Secondary/High School", "Secondary/High School", "Secondary/High School", "College/University", "College/University", "College/University", "College/University")))
sample$education_factor <- factor(sample$education, levels = c("Secondary/High School", "College/University"), ordered = TRUE)
sample$date_of_birth <- as.Date(sample$date_of_birth)
sample$age <- floor(age_calc(sample$date_of_birth, units = "years"))
sample$roof <- as.character(mapvalues(sample$roof, from = c("", "Cement/Concrete", "Cloth", "Concrete/Cement", "Container", "Housing container", "Iron Sheets", "Other", "Straw/Thatch/Soil/Mud", "Tarp/Cloth", "Thatch / Wood / Mud", "Tile", "Iron sheets"), to = c("Other", "Concrete/cement", "Other", "Concrete/cement", "Other", "Other", "Iron sheets", "Other", "Other", "Other", "Other", "Other", "Iron sheets")))
sample$wall <- as.character(mapvalues(sample$wall, from = c("", "Baked Brick", "Baked Bricks", "Grass", "Poles And Mud", "Stones", "Sun dried brick", "Sun Dried Brick", "Wall types here", "Wall Types Here", "Woods"), to = c("Other", "Baked bricks", "Baked bricks", "Other", "Poles and mud", "Other", "Baked bricks", "Baked bricks", "Other", "Other", "Other")))
sample$toilet <- as.character(mapvalues(sample$toilet, from = c("Free Community Toilet", "Open Pit/Latrine", "Inside Home", "Own Flush Toilet", "Paid Community Toilet", "Paid community toilet", "Shared Community", "Shared flush toilet", "Shared Flush Toilet", "Shared outhouse", "Shared Outhouse", "Own outhouse"), to = c("Free community toilet", "Open pit/latrine", "Own flush toilet", "Own flush toilet", "Paid community toilet", "Paid community toilet", "Shared toilet (free or paid)", "Shared toilet (free or paid)", "Shared toilet (free or paid)", "Shared toilet (free or paid)", "Shared toilet (free or paid)", "Other")))

main_sample <- read_xlsx("smsleopard_sample_main_study.xlsx")
main_sample$code <- str_sub(main_sample$link1, start= -8)
main_data <- read_xlsx("main_data.xlsx")
main_data <- subset(main_data, session.label == "wave_4" | session.label == "wave_3" | session.label == "wave_2" | session.label == "wave_1")
main_data <- merge(main_data, main_sample, by.x = "participant.code", by.y = "code")
main_data <- main_data[,c(1, 2, 5, 10, 16, 17, 26, 27, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 72, 73, 74, 75)]
main_data <- setNames(main_data, c("participant_code", "id", "pages", "start_time", "session_code", "session_label", "language", "consent", "age1", "gender1", "ward1", "in_kibra", "income_last_month", "hh_size", "no_left_house", "reasons_left_house", "reasons_left_house_other", "neighbors_contact_number", "neighbors_contact_places", "neighbors_contact_other_places", "resp_contact_number", "resp_contact_places", "resp_contact_other_places", "definition", "strictness", "neighbors_soc_dis", "difficult", "obstacles", "obstacles_other", "avoided_contact", "avoided_contact_places", "avoided_contact_other_places", "avoided_touching", "situations", "situations_other", "effective", "neighbors_expectation", "satisfaction", "satisfaction_soc_dis", "payment", "number1", "number", "first_name", "last_name", "link"))
main_data <- merge(main_data, sample, by.x = "number", by.y = "contact", all.x = TRUE)

# 2. Clean data and create vars -------------------------------------------

#Remove unnecessary characters from vars where respondents could select multiple answers
vars_remove_chars <- c("reasons_left_house", "neighbors_contact_places", "resp_contact_places", "definition", "obstacles", "avoided_contact_places", "situations")
remove_chars <- function(x) {mgsub(x, c("'", "\\[", "\\]"), c("", "", ""))}
main_data <- main_data %>% mutate_at(vars_remove_chars, remove_chars)

#Create dummy vars for each choice in select multiple questions
main_data$rlh_church_mosque <- as.integer(str_detect(main_data$reasons_left_house,"church"))
main_data$rlh_health <- as.integer(str_detect(main_data$reasons_left_house,"doctor"))
main_data$rlh_friends_family <- as.integer(str_detect(main_data$reasons_left_house,"friends"))
main_data$rlh_work <- as.integer(str_detect(main_data$reasons_left_house,"work"))
main_data$rlh_food_shopping <- as.integer(str_detect(main_data$reasons_left_house,"Food"))
main_data$rlh_other_shopping <- as.integer(str_detect(main_data$reasons_left_house,"Other sh"))
main_data$rlh_physical <- as.integer(str_detect(main_data$reasons_left_house,"Physical"))
main_data$rlh_bored <- as.integer(str_detect(main_data$reasons_left_house,"bored"))
main_data$rlh_privacy <- as.integer(str_detect(main_data$reasons_left_house,"privacy"))
main_data$rlh_toilet <- as.integer(str_detect(main_data$reasons_left_house,"toilet"))
main_data$rlh_education <- as.integer(str_detect(main_data$reasons_left_house,"school"))
main_data$rlh_drinks <- as.integer(str_detect(main_data$reasons_left_house,"drinks"))
main_data$rlh_event <- as.integer(str_detect(main_data$reasons_left_house,"funeral"))
main_data$rlh_water <- as.integer(str_detect(main_data$reasons_left_house,"water"))

main_data$ncp_food_shops <- as.integer(str_detect(main_data$neighbors_contact_places,"Food"))
main_data$ncp_other_shops <- as.integer(str_detect(main_data$neighbors_contact_places,"Other sh"))
main_data$ncp_church_mosque <- as.integer(str_detect(main_data$neighbors_contact_places,"mosque"))
main_data$ncp_health_care <- as.integer(str_detect(main_data$neighbors_contact_places,"Doctor"))
main_data$ncp_school_uni <- as.integer(str_detect(main_data$neighbors_contact_places,"School"))
main_data$ncp_community_centre <- as.integer(str_detect(main_data$neighbors_contact_places,"Community"))
main_data$ncp_home <- as.integer(str_detect(main_data$neighbors_contact_places,"home"))
main_data$ncp_public_place <- as.integer(str_detect(main_data$neighbors_contact_places,"public place"))
main_data$ncp_public_transport <- as.integer(str_detect(main_data$neighbors_contact_places,"Public"))
main_data$ncp_private_transport <- as.integer(str_detect(main_data$neighbors_contact_places,"Private t"))
main_data$ncp_toilet <- as.integer(str_detect(main_data$neighbors_contact_places,"toilet"))
main_data$ncp_water_joint <- as.integer(str_detect(main_data$neighbors_contact_places,"water j"))
main_data$ncp_resto_bar <- as.integer(str_detect(main_data$neighbors_contact_places,"restaurant"))
main_data$ncp_work <- as.integer(str_detect(main_data$neighbors_contact_places,"work"))

main_data$rcp_food_shops <- as.integer(str_detect(main_data$resp_contact_places,"Food"))
main_data$rcp_other_shops <- as.integer(str_detect(main_data$resp_contact_places,"Other sh"))
main_data$rcp_church_mosque <- as.integer(str_detect(main_data$resp_contact_places,"Church"))
main_data$rcp_health_care <- as.integer(str_detect(main_data$resp_contact_places,"Doctor"))
main_data$rcp_school_uni <- as.integer(str_detect(main_data$resp_contact_places,"School"))
main_data$rcp_community_centre <- as.integer(str_detect(main_data$resp_contact_places,"Community"))
main_data$rcp_own_home <- as.integer(str_detect(main_data$resp_contact_places,"own home"))
main_data$rcp_friends_home <- as.integer(str_detect(main_data$resp_contact_places,"friend"))
main_data$rcp_public_place <- as.integer(str_detect(main_data$resp_contact_places,"public place"))
main_data$rcp_public_transport <- as.integer(str_detect(main_data$resp_contact_places,"Public transport"))
main_data$rcp_private_transport <- as.integer(str_detect(main_data$resp_contact_places,"Private t"))
main_data$rcp_toilet <- as.integer(str_detect(main_data$resp_contact_places,"toilet"))
main_data$rcp_water_joint <- as.integer(str_detect(main_data$resp_contact_places,"water j"))
main_data$rcp_resto_bar <- as.integer(str_detect(main_data$resp_contact_places,"restaurant"))
main_data$rcp_work <- as.integer(str_detect(main_data$resp_contact_places,"work"))
main_data$rcp_sports_pitch <- as.integer(str_detect(main_data$resp_contact_places,"pitch"))

main_data$def_distance <- as.integer(str_detect(main_data$definition,"feet"))
main_data$def_home <- as.integer(str_detect(main_data$definition,"home as"))
main_data$def_travel <- as.integer(str_detect(main_data$definition,"travelling"))
main_data$def_friends_relatives <- as.integer(str_detect(main_data$definition,"friends"))
main_data$def_avoid_busy_places <- as.integer(str_detect(main_data$definition,"markets"))
main_data$def_avoid_public_transport <- as.integer(str_detect(main_data$definition,"transportation"))
main_data$def_worship <- as.integer(str_detect(main_data$definition,"worship"))
main_data$def_isolation <- as.integer(str_detect(main_data$definition,"flatmates"))
main_data$def_own_needs <- as.integer(str_detect(main_data$definition,"own needs"))
main_data$def_government <- as.integer(str_detect(main_data$definition,"government"))

main_data$obs_food <- as.integer(str_detect(main_data$obstacles,"food"))
main_data$obs_religion <- as.integer(str_detect(main_data$obstacles,"religious"))
main_data$obs_phys_activities <- as.integer(str_detect(main_data$obstacles,"phys"))
main_data$obs_work <- as.integer(str_detect(main_data$obstacles,"work"))
main_data$obs_health_care <- as.integer(str_detect(main_data$obstacles,"health"))
main_data$obs_friends_family <- as.integer(str_detect(main_data$obstacles,"friends"))
main_data$obs_toilet <- as.integer(str_detect(main_data$obstacles,"toilet"))
main_data$obs_water <- as.integer(str_detect(main_data$obstacles,"water"))
main_data$obs_lonely <- as.integer(str_detect(main_data$obstacles,"lonely"))
main_data$obs_bored <- as.integer(str_detect(main_data$obstacles,"bored"))
main_data$obs_group_pressure <- as.integer(str_detect(main_data$obstacles,"pressure"))
main_data$obs_privacy <- as.integer(str_detect(main_data$obstacles,"privacy"))
main_data$obs_other_people <- as.integer(str_detect(main_data$obstacles,"respecting"))

main_data$acp_food_shops <- as.integer(str_detect(main_data$avoided_contact_places,"Food"))
main_data$acp_other_shops <- as.integer(str_detect(main_data$avoided_contact_places,"Other sh"))
main_data$acp_church_mosque <- as.integer(str_detect(main_data$avoided_contact_places,"Church"))
main_data$acp_health_care <- as.integer(str_detect(main_data$avoided_contact_places,"Doctor"))
main_data$acp_school_uni <- as.integer(str_detect(main_data$avoided_contact_places,"School"))
main_data$acp_community_centre <- as.integer(str_detect(main_data$avoided_contact_places,"Community"))
main_data$acp_friend_in_Nairobi <- as.integer(str_detect(main_data$avoided_contact_places,"within Nairobi"))
main_data$acp_friend_elsewhere <- as.integer(str_detect(main_data$avoided_contact_places,"outside of Nairobi"))
main_data$acp_toilet <- as.integer(str_detect(main_data$avoided_contact_places,"toilet"))
main_data$acp_water_joint <- as.integer(str_detect(main_data$avoided_contact_places,"Water j"))
main_data$acp_resto_bar <- as.integer(str_detect(main_data$avoided_contact_places,"restaurant"))
main_data$acp_event <- as.integer(str_detect(main_data$avoided_contact_places,"event"))
main_data$acp_sports_facility <- as.integer(str_detect(main_data$avoided_contact_places,"Sport"))

main_data$situations_stroke <- as.integer(str_detect(main_data$situations,"Stroke"))
main_data$situations_hug <- as.integer(str_detect(main_data$situations,"Hug"))
main_data$situations_kiss <- as.integer(str_detect(main_data$situations,"Kiss"))
main_data$situations_shake_hands <- as.integer(str_detect(main_data$situations,"Shake"))
main_data$situations_stand_sit_closer <- as.integer(str_detect(main_data$situations,"closer"))

#Create vars for age bracket, completion and wave; replace NA's for gender and age with values from database
main_data <- main_data %>% 
  mutate(age1 = ifelse(is.na(age1), age, age1))
main_data$age_bracket <- main_data$age1
main_data$age_bracket<-cut(main_data$age_bracket, breaks=c(18, 24, 30, 36, 120), right = FALSE)
main_data$completion <- as.numeric(main_data$pages >23)
main_data$wave <- as.numeric(mapvalues(main_data$session_label, from = c("wave_1", "wave_2", "wave_3", "wave_4"), to = c("1", "2", "3", "4")))
main_data$session_label <- mapvalues(main_data$session_label, from = c("wave_1", "wave_2", "wave_3", "wave_4"), to = c("wave 1", "wave 2", "wave 3", "wave 4"))
main_data <- main_data %>% 
  mutate(gender1 = ifelse(is.na(gender1), gender, gender1))

#Create 2nd wave variable for fixed effects regression
main_data$wave_ <- main_data$wave

#Transform class of vars
main_data$effective <- main_data$effective %>% 
  mapvalues(from = c("a", "b", "c", "d", "e"), to = c("1", "2", "3", "4", "5")) %>%
  as.numeric(main_data$effective)

#Rename values
main_data$gender1  <- mapvalues(main_data$gender1, from = c("a", "b", "c", "2", "3"), to = c("Male", "Female", "Other", "Female", "Male"))
main_data$ward1  <- mapvalues(main_data$ward1, from = c("a", "b", "c", "d", "e", "f", "g"), to = c("Laini Saba", "Lindi", "Makina", "Woodley/Kenyatta Golf Course", "Sarang'ombe", "Don't know", "I don't live in Kibra anymore"))
main_data$in_kibra  <- mapvalues(main_data$in_kibra, from = c("0", "1"), to = c("No", "Yes"))
main_data$age_bracket <- mapvalues(main_data$age_bracket, from = c("[18,24)", "[24,30)", "[30,36)", "[36,120)"), to = c("18-23", "24-29", "30-35", "36 and older"))
main_data$language <- mapvalues(main_data$language, from = c("en", "swh"), to = c("English", "Kiswahili"))

#Convert values for respondent's physical contacts from wave 1 to guarantee comparability with other waves and question about neighbors contacts (this is necessary due to a change in the survey after wave 1)
main_data$resp_contact_number <- ifelse(main_data$resp_contact_number > 100, 100, main_data$resp_contact_number)

#Remove PII variables and write csv
pii_vars <- c("number", "number1", "first_name.x", "last_name.x", "date_of_birth", "first_name.y", "last_name.y")
main_data <- main_data[, !names(main_data) %in% pii_vars]
write_csv(main_data, "data_masters_thesis.csv")

# 3. Convert strings to factors and create subsets -------------------------------------------------------

main_data <- read.csv("data_masters_thesis.csv", stringsAsFactors = FALSE)

#Create factored variables for scale questions
main_data$strictness_scale <- main_data$strictness %>%
  mapvalues(from = c("1", "2", "3", "4", "5"), to = c("I don't practice social distancing", "Not very strictly", "Somewhat strictly", "Very strictly", "Extremely strictly")) %>%
  factor(levels = c("I don't practice social distancing", "Not very strictly", "Somewhat strictly", "Very strictly", "Extremely strictly"), ordered = TRUE)

main_data$difficulty_scale <- main_data$difficult %>%
  mapvalues(from = c("1", "2", "3", "4", "5"), to = c("Not difficult at all", "A bit difficult", "Somewhat difficult", "Very difficult", "Extremely difficult")) %>%
  factor(levels = c("Not difficult at all", "A bit difficult", "Somewhat difficult", "Very difficult", "Extremely difficult"), ordered = TRUE)

main_data$effectiveness_scale <- main_data$effective %>%
  mapvalues(from = c("1", "2", "3", "4", "5"), to = c("Very ineffective", "Rather ineffective", "Neither effective nor ineffective", "Rather effective", "Very effective")) %>%
  factor(levels = c("Very ineffective", "Rather ineffective", "Neither effective nor ineffective", "Rather effective", "Very effective"), ordered = TRUE)

main_data$satisfaction_scale <- main_data$satisfaction %>%
  mapvalues(from = c("1", "2", "3", "4", "5"), to = c("Strongly dissatisfied", "Dissatisfied", "Neither dissatisfied nor satisfied", "Satisfied", "Strongly satisfied")) %>%
  factor(levels = c("Strongly dissatisfied", "Dissatisfied", "Neither dissatisfied nor satisfied", "Satisfied", "Strongly satisfied"), ordered = TRUE)

main_data$satisfaction_soc_dis_scale <- main_data$satisfaction_soc_dis %>%
  mapvalues(from = c("1", "2", "3", "4", "5"), to = c("Strongly negative", "Rather negative", "Neither negative nor positive", "Rather positive", "Strongly positive")) %>%
  factor(levels = c("Strongly negative", "Rather negative", "Neither negative nor positive", "Rather positive", "Strongly positive"), ordered = TRUE)

#Create subsets
main_data_c <- subset(main_data, pages >23)
main_data_kibra <- subset(main_data_c, ward1 != "I don't live in Kibra anymore")
main_data_kibra_yesterday <- subset(main_data_c, in_kibra == "Yes")
wave_1 <- subset(main_data, wave == 1)
wave_2 <- subset(main_data, wave == 2)
wave_3 <- subset(main_data, wave == 3)
wave_4 <- subset(main_data, wave == 4)
wave_1_c <- subset(main_data_c, wave == 1)
wave_2_c <- subset(main_data_c, wave == 2)
wave_3_c <- subset(main_data_c, wave == 3)
wave_4_c <- subset(main_data_c, wave == 4)
wave_1_kibra <- subset(wave_1_c, ward1 != "I don't live in Kibra anymore")
wave_2_kibra <- subset(wave_2_c, ward1 != "I don't live in Kibra anymore")
wave_3_kibra <- subset(wave_3_c, ward1 != "I don't live in Kibra anymore")
wave_4_kibra <- subset(wave_4_c, ward1 != "I don't live in Kibra anymore")
wave_1_kibra_yesterday <- subset(wave_1_c, in_kibra == "Yes")
wave_2_kibra_yesterday <- subset(wave_2_c, in_kibra == "Yes")
wave_3_kibra_yesterday <- subset(wave_3_c, in_kibra == "Yes")
wave_4_kibra_yesterday <- subset(wave_4_c, in_kibra == "Yes")

#Add wide format and variables which include demographics, change order of answers
wide_format <- reshape(main_data_c, idvar = "id", timevar = "wave", direction = "wide")
wide_format$age1.1 <- ifelse(is.na(wide_format$age1.1), wide_format$age1.2, wide_format$age1.1)
wide_format$age1.1 <- ifelse(is.na(wide_format$age1.1), wide_format$age1.3, wide_format$age1.1)
wide_format$age1.1 <- ifelse(is.na(wide_format$age1.1), wide_format$age1.4, wide_format$age1.1)
wide_format$gender1.1 <- ifelse(is.na(wide_format$gender1.1), wide_format$gender1.2, wide_format$gender1.1)
wide_format$gender1.1 <- ifelse(is.na(wide_format$gender1.1), wide_format$gender1.3, wide_format$gender1.1)
wide_format$gender1.1 <- ifelse(is.na(wide_format$gender1.1), wide_format$gender1.4, wide_format$gender1.1)
wide_format$ward1.1 <- ifelse(is.na(wide_format$ward1.1), wide_format$ward1.2, wide_format$ward1.1)
wide_format$ward1.1 <- ifelse(is.na(wide_format$ward1.1), wide_format$ward1.3, wide_format$ward1.1)
wide_format$ward1.1 <- ifelse(is.na(wide_format$ward1.1), wide_format$ward1.4, wide_format$ward1.1)
wide_format$ward1.1 <- factor(wide_format$ward1.1, levels = c("Laini Saba", "Lindi", "Makina", "Sarang'ombe", "Woodley/Kenyatta Golf Course", "Don't know", "I don't live in Kibra anymore"), ordered = TRUE)
wide_format$hh_size.1 <- ifelse(is.na(wide_format$hh_size.1), wide_format$hh_size.2, wide_format$hh_size.1)
wide_format$hh_size.1 <- ifelse(is.na(wide_format$hh_size.1), wide_format$hh_size.3, wide_format$hh_size.1)
wide_format$hh_size.1 <- ifelse(is.na(wide_format$hh_size.1), wide_format$hh_size.4, wide_format$hh_size.1)
wide_format$roof.1 <- ifelse(is.na(wide_format$roof.1), wide_format$roof.2, wide_format$roof.1)
wide_format$roof.1 <- ifelse(is.na(wide_format$roof.1), wide_format$roof.3, wide_format$roof.1)
wide_format$roof.1 <- ifelse(is.na(wide_format$roof.1), wide_format$roof.4, wide_format$roof.1)
wide_format$roof.1 <- factor(wide_format$roof.1, levels = c("Iron sheets", "Concrete/cement", "Other"), ordered = TRUE)
wide_format$education.1 <- ifelse(is.na(wide_format$education.1), wide_format$education.2, wide_format$education.1)
wide_format$education.1 <- ifelse(is.na(wide_format$education.1), wide_format$education.3, wide_format$education.1)
wide_format$education.1 <- ifelse(is.na(wide_format$education.1), wide_format$education.4, wide_format$education.1)
wide_format$education.1 <- factor(wide_format$education.1, levels = c("Secondary/High School", "College/University"), ordered = TRUE)
wide_format$toilet.1 <- ifelse(is.na(wide_format$toilet.1), wide_format$toilet.2, wide_format$toilet.1)
wide_format$toilet.1 <- ifelse(is.na(wide_format$toilet.1), wide_format$toilet.3, wide_format$toilet.1)
wide_format$toilet.1 <- ifelse(is.na(wide_format$toilet.1), wide_format$toilet.4, wide_format$toilet.1)
wide_format$toilet.1 <- factor(wide_format$toilet.1, levels = c("Own flush toilet", "Free community toilet", "Paid community toilet", "Shared toilet (free or paid)", "Open pit/latrine"), ordered = TRUE)
wide_format_kibra <- subset(wide_format, ward1.1 != "I don't live in Kibra anymore")

#Delete observations where, after checking the data, it becomes obvious that different respondents have answered in different rounds on the same number
#Results in two data frames for the fixed effects regressions
main_data_kibra_fixed <- main_data_kibra[!(main_data_kibra$id == "945" | main_data_kibra$id == "676" | main_data_kibra$id == "766" | main_data_kibra$id == "577" | main_data_kibra$id == "152" | main_data_kibra$id == "928" | main_data_kibra$id == "849" | main_data_kibra$id == "334" | main_data_kibra$id == "36" | main_data_kibra$id == "228" | main_data_kibra$id == "266" | main_data_kibra$id == "891" | main_data_kibra$id == "405" | main_data_kibra$id == "154"),]
main_data_kibra_yesterday_fixed <- main_data_kibra_yesterday[!(main_data_kibra_yesterday$id == "945" | main_data_kibra_yesterday$id == "676" | main_data_kibra_yesterday$id == "766" | main_data_kibra_yesterday$id == "577" | main_data_kibra_yesterday$id == "152" | main_data_kibra_yesterday$id == "928" | main_data_kibra_yesterday$id == "849" | main_data_kibra_yesterday$id == "334" | main_data_kibra_yesterday$id == "36" | main_data_kibra_yesterday$id == "228" | main_data_kibra_yesterday$id == "266" | main_data_kibra_yesterday$id == "891" | main_data_kibra_yesterday$id == "405" | main_data_kibra_yesterday$id == "154"),]

#Create dataframes for Likert graphs
strictness <- as.data.frame(wide_format_kibra[, c("strictness_scale.1", "strictness_scale.2", "strictness_scale.3", "strictness_scale.4")])
difficulty <- as.data.frame(wide_format_kibra[, c("difficulty_scale.1", "difficulty_scale.2", "difficulty_scale.3", "difficulty_scale.4")])
effectiveness <- as.data.frame(wide_format_kibra[, c("effectiveness_scale.1", "effectiveness_scale.2", "effectiveness_scale.3", "effectiveness_scale.4")])
satisfaction <- as.data.frame(wide_format_kibra[, c("satisfaction_scale.1", "satisfaction_scale.2", "satisfaction_scale.3", "satisfaction_scale.4")])
satisfaction_soc_dis <- as.data.frame(wide_format_kibra[, c("satisfaction_soc_dis_scale.1", "satisfaction_soc_dis_scale.2", "satisfaction_soc_dis_scale.3", "satisfaction_soc_dis_scale.4")])

# 4. Data analysis with ggplot2 --------------------------------------------------------

#Upload Busara Theme
busara_theme <- theme(
  plot.title  =  element_text(size  =  18, hjust  =  0.5, face="bold"),
  plot.subtitle  =  element_text(hjust  =  0.5),
  text  =  element_text(size = 15, family = "Muli", color = "black"),
  panel.border  =  element_blank(),
  axis.text.x  =  element_text(angle  =  0, hjust  =  0.5),
  panel.grid.major  =  element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  axis.line.x  =  element_line(color = "black", size  =  1),
  legend.position = "right")

#4.1 Completion rates
#By Wave
df10 <- summarySE(main_data, measurevar = "completion", groupvars = "session_label", na.rm = TRUE)
df10$completion <- round(df10$completion*100, digits = 2)
df10$ci <- df10$ci*100

p10 <- ggplot(df10, mapping = aes(x = session_label, y = completion, fill = session_label)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "Completion rate in %", x = "", title = "Completion rate by wave")+
  geom_text(data = df10, nudge_y =0.7, nudge_x = -0.2, aes(label = completion))+
  geom_errorbar(data = df10, aes(ymin=completion-ci, ymax=completion+ci), width=.2)+
  coord_cartesian(ylim=c(0, 25))
p10 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")

rm(df10, p10)

#By gender
lm111 <- lm(completion ~ gender1, wave_1)
lm112 <- lm(completion ~ gender1, wave_2)
lm113 <- lm(completion ~ gender1, wave_3)
lm114 <- lm(completion ~ gender1, wave_4)

df11 <- main_data %>% 
  dplyr::group_by(session_label, gender1, completion) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% 
  dplyr::group_by(session_label, gender1) %>% 
  dplyr::mutate(Percent = 100*Count/sum(Count))
df11 <- subset(df11, completion == 1)
df11$Percent <- round(df11$Percent, digits = 2)

p11 <- ggplot(df11, mapping = aes(x=gender1, y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "Completion rate in %", x = "", title = "Completion rate by gender and wave", fill = "", caption = "")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")+
  coord_cartesian(ylim=c(0, 25))
p11 + busara_theme

rm(lm111, lm112, lm113, lm114, df11, p11)

#by age
lm121 <- lm(completion ~ age, wave_1)
lm122 <- lm(completion ~ age, wave_2)
lm123 <- lm(completion ~ age, wave_3)
lm124 <- lm(completion ~ age, wave_4)

df12 <- main_data %>% dplyr::group_by(session_label, age_bracket, completion) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% 
  dplyr::group_by(session_label, age_bracket) %>% 
  dplyr::mutate(Percent = 100*Count/sum(Count))
df12 <- subset(df12, completion == 1)
df12$Percent <- round(df12$Percent, digits = 2)

p12 <- ggplot(df12, mapping = aes(x=age_bracket, y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "Completion rate in %", x = "", title = "   Completion rate by age and wave", fill = "", caption = "")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p12 + busara_theme

rm(lm121, lm122, lm123, lm124, df12, p12)

#by education
lm131 <- lm(completion ~ education, wave_1)
lm132 <- lm(completion ~ education, wave_2)
lm133 <- lm(completion ~ education, wave_3)
lm134 <- lm(completion ~ education, wave_4)

df13 <- main_data %>% dplyr::group_by(session_label, education_factor, completion) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% 
  dplyr::group_by(session_label, education_factor) %>% 
  dplyr::mutate(Percent = 100*Count/sum(Count))
df13 <- subset(df13, completion == 1)
df13$Percent <- round(df13$Percent, digits = 2)

p13 <- ggplot(df13, mapping = aes(x=education_factor, y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "Completion rate in %", x = "", title = "Completion rate by education and wave", fill = "", caption = "Education status based on data 
       collected at respondents' recruitment")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) +
  scale_fill_brewer(palette = "Paired")
p13 + busara_theme

rm(lm131, lm132, lm133, lm134, df13, p13)

#4.2 Sample Composition of people who live in Kibra
#ward
df15 <- wide_format %>% dplyr::group_by(ward1.1) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup()

p15 <- ggplot(df15, mapping = aes (x = ward1.1, y = Count))+
  geom_bar(stat = "summary", fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Ward of respondents", fill = "", caption = "Includes all respondents who have 
       completed at least one survey")+
  geom_text(data = df15, nudge_y =5, nudge_x = -0.0, aes(label = Count))
p15 + busara_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(df15, p15)

#number of valid observations per wave
df16 <- main_data_kibra %>% dplyr::group_by(session_label) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup()

p16 <- ggplot(df16, mapping = aes (x = session_label, y = Count))+
  geom_bar(stat = "summary", fill="#0033a1")+
  theme(panel.background=element_blank())+
  geom_text(data = df16, nudge_y =9, nudge_x = -0.0, aes(label = Count))+
  labs(y = "Number of respondents", x = "", title = "Number of surveys completed by
       residents of Kibera", fill = "", caption = "")
p16 + busara_theme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(df16, p16)

#number of respondents with x valid observations
vars_satisfaction <- c("satisfaction.1", "satisfaction.2", "satisfaction.3", "satisfaction.4")
df161 <- wide_format_kibra[,vars_satisfaction]
df161$na_count <- apply(is.na(df161), 1, sum)

rm(vars_satisfaction, df161)

#did the person spend the day before the survey in Kibra
p162 <- ggplot(main_data_kibra, mapping = aes(x = session_label, fill= in_kibra))+
  geom_bar(position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Did the respondent spend the day
  before the survey in Kibra?", fill = "", caption = "Includes all respondents from Kibera")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.0), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p162 + busara_theme + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(p162)

#language
p17 <- ggplot(main_data_kibra, mapping = aes(x = session_label, fill= language))+
  geom_bar(position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Choice of language by wave", fill = "", caption = "Includes all respondents from Kibera")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.0), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p17 + busara_theme + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(p17) 

#age
p18 <- ggplot(wide_format_kibra, mapping = aes (x = age1.1))+
  geom_histogram(binwidth = 3, fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Age of respondents", fill = "", caption = "Includes all respondents from Kibera 
       who have completed at least one survey")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.0), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 
p18 + busara_theme

rm(p18)

#gender
df19 <- wide_format_kibra %>% dplyr::group_by(gender1.1) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup()

p19 <- ggplot(df19, mapping = aes (x = gender1.1, y = Count))+
  geom_bar(stat = "summary", fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Gender of respondents", fill = "", caption = "Includes all respondents from Kibera 
       who have completed at least one survey")+
  geom_text(data = df19, nudge_y =5, nudge_x = -0.0, aes(label = Count))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.0), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 
p19 + busara_theme

rm(df19, p19)

#Household size
p20 <- ggplot(wide_format_kibra, mapping = aes (x = hh_size.1))+
  geom_histogram(binwidth = 1, fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Household size of respondents", fill = "", caption = "Includes all respondents from Kibera 
       who have completed at least one survey")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.0), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 
p20 + busara_theme

rm(p20)

#income last month
lm21 <- plm(income_last_month ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df210 <- summarySE(main_data_kibra, measurevar = "income_last_month", groupvars = "session_label", na.rm = TRUE)
df210$income_last_month <- round(df210$income_last_month, digits = 0)

p210 <- ggplot(df210, mapping = aes(x = session_label, y = income_last_month)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "Kenyan Shilling", x = "", title = "Average of last month's income by wave", caption = "Includes all respondents from Kibera")+
  geom_text(data = df210, nudge_y =250, nudge_x = -0.2, aes(label = income_last_month))+
  geom_errorbar(data = df210, aes(ymin=income_last_month-ci, ymax=income_last_month+ci), width=.2)
p210 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")

p211 <- ggplot(wave_1_kibra) +
  geom_violin(mapping = aes(y = income_last_month, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Income in KSH", x = "Density", title = "Last month's income of respondents (wave 1)", caption = "Includes all respondents from Kibera
       Not showing outliers >35000 KSH (n=1)")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 8, vjust = 5))+scale_y_continuous(limits = c(0, 35000))+coord_flip()
p211 + busara_theme

p212 <- ggplot(wave_2_kibra) +
  geom_violin(mapping = aes(y = income_last_month, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Income in KSH", x = "Density", title = "Last month's income of respondents (wave 2)", caption = "Includes all respondents from Kibera")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 8, vjust = 5))+scale_y_continuous(limits = c(0, 30000))+coord_flip()
p212 + busara_theme

p213 <- ggplot(wave_3_kibra) +
  geom_violin(mapping = aes(y = income_last_month, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Income in KSH", x = "Density", title = "Last month's income of respondents (wave 3)", caption = "Includes all respondents from Kibera
       Not showing outliers >35000 KSH (n=1)")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 8, vjust = 5))+scale_y_continuous(limits = c(0, 35000))+coord_flip()
p213 + busara_theme

p214 <- ggplot(wave_4_kibra) +
  geom_violin(mapping = aes(y = income_last_month, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Income in KSH", x = "Density", title = "Last month's income of respondents (wave 4)", caption = "Includes all respondents from Kibera
       Not showing outliers >35000 KSH (n=1)")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 8, vjust = 5))+scale_y_continuous(limits = c(0, 35000))+coord_flip()
p214 + busara_theme

rm(lm21, df210, p210, p211, p212, p213, p214)

#Removing highest number from each wave to run model without outliers
main_data_kibra_fixed_new <- subset(main_data_kibra_fixed, participant_code != "cevca554" & participant_code != "5g2xb8gl" & participant_code != "xyu7y3cr" & participant_code != "b3znw9da")
lm211 <- plm(income_last_month ~ wave_, data = main_data_kibra_fixed_new, model = "within", index = c("id", "wave"))

rm(lm211)

#education
df23 <- wide_format_kibra %>% dplyr::group_by(education.1) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup()

p23 <- ggplot(df23, mapping = aes (x = education.1, y = Count)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Education of respondents", caption = "Includes all respondents from Kibera
  Based on data collected at respondents' recruitment")+
  theme(plot.title = element_text(hjust = 0.1), plot.caption = element_text(size = 8, vjust = 5))+
  geom_text(data = df23, nudge_y =7, nudge_x = -0.0, aes(label = Count))
p23 + busara_theme

rm(df23, p23)

#roof
df24 <- wide_format_kibra %>% dplyr::group_by(roof.1) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup()

p24 <- ggplot(df24, mapping = aes (x = roof.1, y = Count)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Roof type of respondents' houses", caption = "Includes all respondents from Kibera
  Based on data collected at respondents' recruitment")+
  theme(plot.title = element_text(hjust = 0.1), plot.caption = element_text(size = 8, vjust = 5))+
  geom_text(data = df24, nudge_y =10, nudge_x = -0.0, aes(label = Count))
p24 + busara_theme

rm(df24, p24) 

#toilet
df25 <- wide_format_kibra %>% dplyr::group_by(toilet.1) %>% 
  dplyr::summarise(Count=n()) %>% dplyr::ungroup()

p25 <- ggplot(df25, mapping = aes (x = toilet.1, y = Count)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Main type of toilet used by respondents", caption = "Includes all respondents from Kibera
       Based on data collected at respondents' recruitment")+
  theme(plot.title = element_text(hjust = 0.1), plot.caption = element_text(size = 8, vjust = 5))+
  geom_text(data = df25, nudge_y =5, nudge_x = -0.0, aes(label = Count))
p25 + busara_theme + theme(axis.text.x = element_text(angle = 30, hjust = 1))

rm(df25, p25)

#4.3 Social Distancing Practices
#number of times respondents left home
lm2611 <- lm(no_left_house ~ gender1, data = wave_1_kibra_yesterday)
lm2612 <- lm(no_left_house ~ gender1, data = wave_2_kibra_yesterday)
lm2613 <- lm(no_left_house ~ gender1, data = wave_3_kibra_yesterday)
lm2614 <- lm(no_left_house ~ gender1, data = wave_4_kibra_yesterday)

lm2621 <- lm(no_left_house ~ age1, data = wave_1_kibra_yesterday)
lm2622 <- lm(no_left_house ~ age1, data = wave_2_kibra_yesterday)
lm2623 <- lm(no_left_house ~ age1, data = wave_3_kibra_yesterday)
lm2624 <- lm(no_left_house ~ age1, data = wave_4_kibra_yesterday)

lm2631 <- lm(no_left_house ~ hh_size, data = wave_1_kibra_yesterday)
lm2632 <- lm(no_left_house ~ hh_size, data = wave_2_kibra_yesterday)
lm2633 <- lm(no_left_house ~ hh_size, data = wave_3_kibra_yesterday)
lm2634 <- lm(no_left_house ~ hh_size, data = wave_4_kibra_yesterday)

lm2641 <- lm(no_left_house ~ toilet, data = wave_1_kibra_yesterday)
lm2642 <- lm(no_left_house ~ toilet, data = wave_2_kibra_yesterday)
lm2643 <- lm(no_left_house ~ toilet, data = wave_3_kibra_yesterday)
lm2644 <- lm(no_left_house ~ toilet, data = wave_4_kibra_yesterday)

lm2651 <- lm(no_left_house ~ income_last_month, data = wave_1_kibra_yesterday)
lm2652 <- lm(no_left_house ~ income_last_month, data = wave_2_kibra_yesterday)
lm2653 <- lm(no_left_house ~ income_last_month, data = wave_3_kibra_yesterday)
lm2654 <- lm(no_left_house ~ income_last_month, data = wave_4_kibra_yesterday)

lm2661 <- lm(no_left_house ~ education, data = wave_1_kibra_yesterday)
lm2662 <- lm(no_left_house ~ education, data = wave_2_kibra_yesterday)
lm2663 <- lm(no_left_house ~ education, data = wave_3_kibra_yesterday)
lm2664 <- lm(no_left_house ~ education, data = wave_4_kibra_yesterday)

lm267 <- plm(no_left_house ~ income_last_month, data = main_data_kibra_yesterday_fixed, model = "within", index = c("id", "wave"))
lm268 <- plm(no_left_house ~ wave_, data = main_data_kibra_yesterday_fixed, model = "within", index = c("id", "wave"))

df26 <- summarySE(main_data_kibra_yesterday, measurevar = "no_left_house", groupvars = "session_label", na.rm = TRUE)
df26$no_left_house <- round(df26$no_left_house, digits = 2)

p26 <- ggplot(df26, mapping = aes(x = session_label, y = no_left_house)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "", x = "", title = "How many times did respondents leave their home
       on the day before the survey?", caption = "Includes all respondents who were 
       in Kibera the day before the survey")+
  geom_text(data = df26, nudge_y =0.2, nudge_x = -0.2, aes(label = no_left_house))+
  geom_errorbar(data = df26, aes(ymin=no_left_house-ci, ymax=no_left_house+ci), width=.2)
p26 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

p260 <- ggplot(main_data_kibra_yesterday, mapping = aes(x = session_label, y = no_left_house)) +
  geom_boxplot(fill="#0033a1")+
  labs(y = "", x = "", title = "How many times did respondents leave their home
       on the day before the survey?", caption = "Includes all respondents who were 
       in Kibera the day before the survey")
p260 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm2611, lm2612, lm2613, lm2614, lm2621, lm2622, lm2623, lm2624, lm2631, lm2632, lm2633, lm2634, lm2641, lm2642, lm2643, lm2644, lm2651, lm2652, lm2653, lm2654, lm2661, lm2662, lm2663, lm2664, lm267, lm268, df26, p26, p260)

#Reasons why respondents left their homes
rlh <- main_data_kibra_yesterday[, c("session_label", "rlh_church_mosque", "rlh_health", "rlh_friends_family", "rlh_work", "rlh_food_shopping", "rlh_other_shopping", "rlh_physical", "rlh_bored", "rlh_privacy", "rlh_toilet", "rlh_education", "rlh_drinks", "rlh_event", "rlh_water")]
rlh <- melt(rlh, "session_label")
rlh$value[is.na(rlh$value)] <- 0

df27 <- rlh %>% dplyr::group_by(variable, session_label, value) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(variable, session_label) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
df27 <- subset(df27, value == 1)
df27$variable <- mapvalues(df27$variable, from = c("rlh_church_mosque", "rlh_health", "rlh_friends_family", "rlh_work", "rlh_food_shopping", "rlh_other_shopping", "rlh_physical", "rlh_bored", "rlh_privacy", "rlh_toilet", "rlh_education", "rlh_drinks", "rlh_event", "rlh_water"), to = c("Going to church/mosque", "Doctor/Hospital/Pharmacy", "Meeting friends/family", "Going to work/business", "Food shopping", "Other shopping", "Physical activity", "Getting bored", "Lack of privacy at home", "Using a shared toilet", "Going to school/university", "Getting drinks/going to a bar", "Attending an event (f.ex. funeral)", "Getting water in a water joint"))

p27 <- ggplot(df27, mapping = aes(x = reorder(variable, -Percent), y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "% of respondents", x = "", title = "Reasons why respondents left their homes", fill = "", caption = "Includes all respondents who were
       in Kibera the day before the survey")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p27 + busara_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(rlh, df27, p27)

#number of estimated physical contacts of neighbors
lm2811 <- lm(neighbors_contact_number ~ gender1, data = wave_1_kibra_yesterday)
lm2812 <- lm(neighbors_contact_number ~ gender1, data = wave_2_kibra_yesterday)
lm2813 <- lm(neighbors_contact_number ~ gender1, data = wave_3_kibra_yesterday)
lm2814 <- lm(neighbors_contact_number ~ gender1, data = wave_4_kibra_yesterday)

lm2821 <- lm(neighbors_contact_number ~ age1, data = wave_1_kibra_yesterday)
lm2822 <- lm(neighbors_contact_number ~ age1, data = wave_2_kibra_yesterday)
lm2823 <- lm(neighbors_contact_number ~ age1, data = wave_3_kibra_yesterday)
lm2824 <- lm(neighbors_contact_number ~ age1, data = wave_4_kibra_yesterday)

lm2831 <- lm(neighbors_contact_number ~ hh_size, data = wave_1_kibra_yesterday)
lm2832 <- lm(neighbors_contact_number ~ hh_size, data = wave_2_kibra_yesterday)
lm2833 <- lm(neighbors_contact_number ~ hh_size, data = wave_3_kibra_yesterday)
lm2834 <- lm(neighbors_contact_number ~ hh_size, data = wave_4_kibra_yesterday)

lm2841 <- lm(neighbors_contact_number ~ toilet, data = wave_1_kibra_yesterday)
lm2842 <- lm(neighbors_contact_number ~ toilet, data = wave_2_kibra_yesterday)
lm2843 <- lm(neighbors_contact_number ~ toilet, data = wave_3_kibra_yesterday)
lm2844 <- lm(neighbors_contact_number ~ toilet, data = wave_4_kibra_yesterday)

lm2851 <- lm(neighbors_contact_number ~ education, data = wave_1_kibra_yesterday)
lm2852 <- lm(neighbors_contact_number ~ education, data = wave_2_kibra_yesterday)
lm2853 <- lm(neighbors_contact_number ~ education, data = wave_3_kibra_yesterday)
lm2854 <- lm(neighbors_contact_number ~ education, data = wave_4_kibra_yesterday)

lm2861 <- lm(neighbors_contact_number ~ income_last_month, data = wave_1_kibra_yesterday)
lm2862 <- lm(neighbors_contact_number ~ income_last_month, data = wave_2_kibra_yesterday)
lm2863 <- lm(neighbors_contact_number ~ income_last_month, data = wave_3_kibra_yesterday)
lm2864 <- lm(neighbors_contact_number ~ income_last_month, data = wave_4_kibra_yesterday)

lm287 <- plm(neighbors_contact_number ~ income_last_month, data = main_data_kibra_yesterday_fixed, model = "within", index = c("id", "wave"))
lm288 <- plm(neighbors_contact_number ~ wave_, data = main_data_kibra_yesterday_fixed, model = "within", index = c("id", "wave"))

df28 <- summarySE(main_data_kibra_yesterday, measurevar = "neighbors_contact_number", groupvars = "session_label", na.rm = TRUE)
df28$neighbors_contact_number <- round(df28$neighbors_contact_number, digits = 2)

p28 <- ggplot(df28, mapping = aes(x = session_label, y = neighbors_contact_number)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "", x = "", title = "Estimated number of neighbors' physical contacts
  on the day before the survey", caption = "Includes all respondents who were 
       in Kibera the day before the survey")+
  geom_text(data = df28, nudge_y =0.5, nudge_x = -0.2, aes(label = neighbors_contact_number))+
  geom_errorbar(data = df28, aes(ymin=neighbors_contact_number-ci, ymax=neighbors_contact_number+ci), width=.2)
p28 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

p280 <- ggplot(main_data_kibra_yesterday, mapping = aes(x = session_label, y = neighbors_contact_number)) +
  geom_boxplot(fill="#0033a1")+
  labs(y = "", x = "", title = "Estimated number of neighbors' physical contacts
  on the day before the survey", caption = "Includes all respondents who were 
       in Kibera the day before the survey
       Not showing outliers >25 (n = 20)")
p280 + 
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))+
  scale_y_continuous(limits = c(0, 25)) + 
  busara_theme

rm(lm2811, lm2812, lm2813, lm2814, lm2821, lm2822, lm2823, lm2824, lm2831, lm2832, lm2833, lm2834, lm2841, lm2842, lm2843, lm2844, lm2851, lm2852, lm2853, lm2854, lm2861, lm2862, lm2863, lm2864, lm287, lm288, df28, p28, p280, p281)

#Estimated places of physical contacts of neighbors yesterday
ncp <- main_data_kibra_yesterday[c("session_label", "ncp_food_shops", "ncp_other_shops", "ncp_church_mosque", "ncp_health_care", "ncp_school_uni", "ncp_community_centre", "ncp_home", "ncp_public_place", "ncp_public_transport", "ncp_private_transport", "ncp_toilet", "ncp_water_joint", "ncp_resto_bar", "ncp_work")]
ncp <- melt(ncp, "session_label")
ncp$value[is.na(ncp$value)] <- 0

df29 <- ncp %>% dplyr::group_by(variable, session_label, value) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(variable, session_label) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
df29 <- subset(df29, value == 1)
df29$variable <- mapvalues(df29$variable, from = c("ncp_food_shops", "ncp_other_shops", "ncp_church_mosque", "ncp_health_care", "ncp_school_uni", "ncp_community_centre", "ncp_home", "ncp_public_place", "ncp_public_transport", "ncp_private_transport", "ncp_toilet", "ncp_water_joint", "ncp_resto_bar", "ncp_work"), to = c("Food shops", "Other shops", "Church/mosque", "Doctor/hospital/pharmacy", "School/university", "Community centre", "At home", "On the street/in a public place", "Public transport", "Private transport", "Shared toilet", "Shared water joint", "In a restaurant/bar", "At work"))

p29 <- ggplot(df29, mapping = aes(x = reorder(variable, -Percent), y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "% of respondents", x = "", title = "Estimated places of physical contacts of neighbors yesterday", fill = "", caption = "Includes all respondents who were
       in Kibera the day before the survey")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p29 + busara_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(ncp, df29, p29)

#number of physical contacts of respondents
lm3011 <- lm(resp_contact_number ~ gender1, data = wave_1_kibra_yesterday)
lm3012 <- lm(resp_contact_number ~ gender1, data = wave_2_kibra_yesterday)
lm3013 <- lm(resp_contact_number ~ gender1, data = wave_3_kibra_yesterday)
lm3014 <- lm(resp_contact_number ~ gender1, data = wave_4_kibra_yesterday)

lm3021 <- lm(resp_contact_number ~ age1, data = wave_1_kibra_yesterday)
lm3022 <- lm(resp_contact_number ~ age1, data = wave_2_kibra_yesterday)
lm3023 <- lm(resp_contact_number ~ age1, data = wave_3_kibra_yesterday)
lm3024 <- lm(resp_contact_number ~ age1, data = wave_4_kibra_yesterday)

lm3031 <- lm(resp_contact_number ~ hh_size, data = wave_1_kibra_yesterday)
lm3032 <- lm(resp_contact_number ~ hh_size, data = wave_2_kibra_yesterday)
lm3033 <- lm(resp_contact_number ~ hh_size, data = wave_3_kibra_yesterday)
lm3034 <- lm(resp_contact_number ~ hh_size, data = wave_4_kibra_yesterday)

lm3041 <- lm(resp_contact_number ~ toilet, data = wave_1_kibra_yesterday)
lm3042 <- lm(resp_contact_number ~ toilet, data = wave_2_kibra_yesterday)
lm3043 <- lm(resp_contact_number ~ toilet, data = wave_3_kibra_yesterday)
lm3044 <- lm(resp_contact_number ~ toilet, data = wave_4_kibra_yesterday)

lm3051 <- lm(resp_contact_number ~ education, data = wave_1_kibra_yesterday)
lm3052 <- lm(resp_contact_number ~ education, data = wave_2_kibra_yesterday)
lm3053 <- lm(resp_contact_number ~ education, data = wave_3_kibra_yesterday)
lm3054 <- lm(resp_contact_number ~ education, data = wave_4_kibra_yesterday)

lm3061 <- lm(resp_contact_number ~ income_last_month, data = wave_1_kibra_yesterday)
lm3062 <- lm(resp_contact_number ~ income_last_month, data = wave_2_kibra_yesterday)
lm3063 <- lm(resp_contact_number ~ income_last_month, data = wave_3_kibra_yesterday)
lm3064 <- lm(resp_contact_number ~ income_last_month, data = wave_4_kibra_yesterday)

lm307 <- plm(resp_contact_number ~ income_last_month, data = main_data_kibra_yesterday_fixed, model = "within", index = c("id", "wave"))
lm308 <- plm(resp_contact_number ~ wave_, data = main_data_kibra_yesterday_fixed, model = "within", index = c("id", "wave"))

df30 <- summarySE(main_data_kibra_yesterday, measurevar = "resp_contact_number", groupvars = "session_label", na.rm = TRUE)
df30$resp_contact_number <- round(df30$resp_contact_number, digits = 2)

p30 <- ggplot(df30, mapping = aes(x = session_label, y = resp_contact_number)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "", x = "", title = "Number of respondents' physical contacts
  on the day before the survey", caption = "Includes all respondents who were 
       in Kibera the day before the survey")+
  geom_text(data = df30, nudge_y =0.40, nudge_x = -0.2, aes(label = resp_contact_number))+
  geom_errorbar(data = df30, aes(ymin=resp_contact_number-ci, ymax=resp_contact_number+ci), width=.2)
p30 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

p300 <- ggplot(main_data_kibra_yesterday, mapping = aes(x = session_label, y = resp_contact_number)) +
  geom_boxplot(fill="#0033a1")+
  labs(y = "", x = "", title = "Number of respondents' physical contacts
  on the day before the survey", caption = "Includes all respondents who were 
       in Kibera the day before the survey
       Not showing outliers >25 (n = 14)")
p300 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))+
  scale_y_continuous(limits = c(0, 25))

rm(lm3011, lm3012, lm3013, lm3014, lm3021, lm3022, lm3023, lm3024, lm3031, lm3032, lm3033, lm3034, lm3041, lm3042, lm3043, lm3044, lm3051, lm3052, lm3053, lm3054, lm3061, lm3062, lm3063, lm3064, lm307, lm308, df30, p30, p300)

#Places of physical contacts of respondents yesterday
rcp <- main_data_kibra_yesterday[c("session_label", "rcp_food_shops", "rcp_other_shops", "rcp_church_mosque", "rcp_health_care", "rcp_school_uni", "rcp_community_centre", "rcp_own_home", "rcp_friends_home", "rcp_public_place", "rcp_public_transport", "rcp_private_transport", "rcp_toilet", "rcp_water_joint", "rcp_resto_bar", "rcp_work", "rcp_sports_pitch")]
rcp <- melt(rcp, "session_label")
rcp$value[is.na(rcp$value)] <- 0

df31 <- rcp %>% dplyr::group_by(variable, session_label, value) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(variable, session_label) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
df31 <- subset(df31, value == 1)
df31$variable <- mapvalues(df31$variable, from = c("rcp_food_shops", "rcp_other_shops", "rcp_church_mosque", "rcp_health_care", "rcp_school_uni", "rcp_community_centre", "rcp_own_home", "rcp_friends_home", "rcp_public_place", "rcp_public_transport", "rcp_private_transport", "rcp_toilet", "rcp_water_joint", "rcp_resto_bar", "rcp_work", "rcp_sports_pitch"), to = c("Food shops", "Other shops", "Church/mosque", "Doctor/hospital/pharmacy", "School/university", "Community centre", "Respondent's home", "Someone else's home", "On the street/in a public place", "Public transport", "Private transport", "Shared toilet", "Shared water joint", "In a restaurant/bar", "At work", "On a sports pitch"))

p31 <- ggplot(df31, mapping = aes(x = reorder(variable, -Percent), y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "% of respondents", x = "", title = "Places of respondents' physical contacts", fill = "", caption = "Includes all respondents who were
       in Kibera the day before the survey")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p31 + busara_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(rcp, df31, p31)

#Self-reported social distancing strictness
lm3211 <- lm(strictness ~ gender1, data = wave_1_kibra)
lm3212 <- lm(strictness ~ gender1, data = wave_2_kibra)
lm3213 <- lm(strictness ~ gender1, data = wave_3_kibra)
lm3214 <- lm(strictness ~ gender1, data = wave_4_kibra)

lm3221 <- lm(strictness ~ age1, data = wave_1_kibra)
lm3222 <- lm(strictness ~ age1, data = wave_2_kibra)
lm3223 <- lm(strictness ~ age1, data = wave_3_kibra)
lm3224 <- lm(strictness ~ age1, data = wave_4_kibra)

lm3231 <- lm(strictness ~ hh_size, data = wave_1_kibra)
lm3232 <- lm(strictness ~ hh_size, data = wave_2_kibra)
lm3233 <- lm(strictness ~ hh_size, data = wave_3_kibra)
lm3234 <- lm(strictness ~ hh_size, data = wave_4_kibra)

lm3241 <- lm(strictness ~ education, data = wave_1_kibra)
lm3242 <- lm(strictness ~ education, data = wave_2_kibra)
lm3243 <- lm(strictness ~ education, data = wave_3_kibra)
lm3244 <- lm(strictness ~ education, data = wave_4_kibra)

lm3251 <- lm(strictness ~ income_last_month, data = wave_1_kibra)
lm3252 <- lm(strictness ~ income_last_month, data = wave_2_kibra)
lm3253 <- lm(strictness ~ income_last_month, data = wave_3_kibra)
lm3254 <- lm(strictness ~ income_last_month, data = wave_4_kibra)

lm3261 <- lm(strictness ~ toilet, data = wave_1_kibra)
lm3262 <- lm(strictness ~ toilet, data = wave_2_kibra)
lm3263 <- lm(strictness ~ toilet, data = wave_3_kibra)
lm3264 <- lm(strictness ~ toilet, data = wave_4_kibra)

lm327 <- plm(strictness ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm328 <- plm(strictness ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df32 <- summarySE(main_data_kibra, measurevar = "strictness", groupvars = "session_label", na.rm = TRUE)
df32$strictness <- round(df32$strictness, digits = 2)

p32 <- ggplot(df32, mapping = aes(x = session_label, y = strictness)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "", x = "", title = "Average of self-reported social distancing strictness
  on a scale from 1 to 5", caption = "Includes all respondents from Kibera")+
  geom_text(data = df32, nudge_y =0.15, nudge_x = -0.25, aes(label = strictness))+
  geom_errorbar(data = df32, aes(ymin=strictness-ci, ymax=strictness+ci), width=.2)
p32 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), axis.text.y = element_blank(), axis.ticks.y = element_blank())

names(strictness) = c("wave 1", "wave 2", "wave 3", "wave 4")
p321<-likert(strictness)
plot(p321, centered = FALSE, col=c("tomato2", "chocolate2", "azure2", "cornflowerblue", "blue3"))+
  ggtitle("How strictly do you practice social distancing?")+
  labs(caption = "
       Includes all respondents from Kibera")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))+
  busara_theme

rm(lm3211, lm3212, lm3213, lm3214, lm3221, lm3222, lm3223, lm3224, lm3231, lm3232, lm3233, lm3234, lm3241, lm3242, lm3243, lm3244, lm3251, lm3252, lm3253, lm3254, lm3261, lm3262, lm3263, lm3264, lm327, lm328, df32, p32, p321)

#Difficulty to practice social distancing
lm3311 <- lm(difficult ~ gender1, data = wave_1_kibra)
lm3312 <- lm(difficult ~ gender1, data = wave_2_kibra)
lm3313 <- lm(difficult ~ gender1, data = wave_3_kibra)
lm3314 <- lm(difficult ~ gender1, data = wave_4_kibra)

lm3321 <- lm(difficult ~ age1, data = wave_1_kibra)
lm3322 <- lm(difficult ~ age1, data = wave_2_kibra)
lm3323 <- lm(difficult ~ age1, data = wave_3_kibra)
lm3324 <- lm(difficult ~ age1, data = wave_4_kibra)

lm3331 <- lm(difficult ~ hh_size, data = wave_1_kibra)
lm3332 <- lm(difficult ~ hh_size, data = wave_2_kibra)
lm3333 <- lm(difficult ~ hh_size, data = wave_3_kibra)
lm3334 <- lm(difficult ~ hh_size, data = wave_4_kibra)

lm3341 <- lm(difficult ~ education, data = wave_1_kibra)
lm3342 <- lm(difficult ~ education, data = wave_2_kibra)
lm3343 <- lm(difficult ~ education, data = wave_3_kibra)
lm3344 <- lm(difficult ~ education, data = wave_4_kibra)

lm3351 <- lm(difficult ~ toilet, data = wave_1_kibra)
lm3352 <- lm(difficult ~ toilet, data = wave_2_kibra)
lm3353 <- lm(difficult ~ toilet, data = wave_3_kibra)
lm3354 <- lm(difficult ~ toilet, data = wave_4_kibra)

lm3261 <- lm(difficult ~ income_last_month, data = wave_1_kibra)
lm3262 <- lm(difficult ~ income_last_month, data = wave_2_kibra)
lm3263 <- lm(difficult ~ income_last_month, data = wave_3_kibra)
lm3264 <- lm(difficult ~ income_last_month, data = wave_4_kibra)

lm337 <- plm(difficult ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm338 <- plm(difficult ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df33 <- summarySE(main_data_kibra, measurevar = "difficult", groupvars = "session_label", na.rm = TRUE)
df33$difficult <- round(df33$difficult, digits = 2)

p33 <- ggplot(df33, mapping = aes(x = session_label, y = difficult)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "", x = "", title = "How difficult is it for you to practice social distancing?
  (scale from 1 to 5)", caption = "Includes all respondents from Kibera
       who have reported to practice social distancing")+
  geom_text(data = df33, nudge_y =0.1, nudge_x = -0.25, aes(label = difficult))+
  geom_errorbar(data = df33, aes(ymin=difficult-ci, ymax=difficult+ci), width=.2)
p33 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), axis.text.y = element_blank(), axis.ticks.y = element_blank())

names(difficulty) = c("wave 1", "wave 2", "wave 3", "wave 4")
p331<-likert(difficulty)
plot(p331, centered = FALSE, col=c("tomato2", "chocolate2", "azure2", "cornflowerblue", "blue3"), group.order = c("wave 1", "wave 2", "wave 3", "wave 4"))+
  ggtitle("How difficult is it for you to practice social distancing?")+
  labs(caption = "Includes all respondents from Kibera
       who have reported to practice social distancing")+
  busara_theme+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm3311, lm3312, lm3313, lm3314, lm3321, lm3322, lm3323, lm3324, lm3331, lm3332, lm3333, lm3334, lm3341, lm3342, lm3343, lm3344, lm3351, lm3352, lm3353, lm3354, lm3361, lm3362, lm3363, lm3364, lm337, lm338, df33, p33, p331)


 #4.4 Attitudes towards social distancing
#Good social distancing practices
def <- main_data_kibra[c("session_label", "def_distance", "def_home", "def_travel", "def_friends_relatives", "def_avoid_busy_places", "def_avoid_public_transport", "def_worship", "def_isolation", "def_own_needs", "def_government")]
def <- melt(def, "session_label")

df34 <- def %>% dplyr::group_by(variable, session_label, value) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(variable, session_label) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
df34 <- subset(df34, value == 1)
df34$variable <- mapvalues(df34$variable, from = c("def_distance", "def_home", "def_travel", "def_friends_relatives", "def_avoid_busy_places", "def_avoid_public_transport", "def_worship", "def_isolation", "def_own_needs", "def_government"), to = c("Staying 6feet/1.5m 
away from others", "Staying home as 
much as possible", "Not travelling", "Not visiting friends 
or relatives", "Avoiding busy public 
places like markets", "Avoiding public transportation", "Not visiting places 
of worship", "Isolating yourself from everyone
including family and flatmates", "Thinking of your 
own needs first", "Exactly follow 
government advice"))

p34 <- ggplot(df34, mapping = aes(x = reorder(variable, -Percent), y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "% of respondents", x = "", title = "Which behaviors are most important
  to practice 'good social distancing'?", fill = "", caption = "Includes all respondents from Kibera")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p34 + busara_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(def, df34, p34)

#Number of neighbors that supposedly practice SD
lm3511 <- lm(neighbors_soc_dis ~ gender1, data = wave_1_kibra)
lm3512 <- lm(neighbors_soc_dis ~ gender1, data = wave_2_kibra)
lm3513 <- lm(neighbors_soc_dis ~ gender1, data = wave_3_kibra)
lm3514 <- lm(neighbors_soc_dis ~ gender1, data = wave_4_kibra)

lm3521 <- lm(neighbors_soc_dis ~ age1, data = wave_1_kibra)
lm3522 <- lm(neighbors_soc_dis ~ age1, data = wave_2_kibra)
lm3523 <- lm(neighbors_soc_dis ~ age1, data = wave_3_kibra)
lm3524 <- lm(neighbors_soc_dis ~ age1, data = wave_4_kibra)

lm3531 <- lm(neighbors_soc_dis ~ hh_size, data = wave_1_kibra)
lm3532 <- lm(neighbors_soc_dis ~ hh_size, data = wave_2_kibra)
lm3533 <- lm(neighbors_soc_dis ~ hh_size, data = wave_3_kibra)
lm3534 <- lm(neighbors_soc_dis ~ hh_size, data = wave_4_kibra)

lm3541 <- lm(neighbors_soc_dis ~ toilet, data = wave_1_kibra)
lm3542 <- lm(neighbors_soc_dis ~ toilet, data = wave_2_kibra)
lm3543 <- lm(neighbors_soc_dis ~ toilet, data = wave_3_kibra)
lm3544 <- lm(neighbors_soc_dis ~ toilet, data = wave_4_kibra)

lm3551 <- lm(neighbors_soc_dis ~ education, data = wave_1_kibra)
lm3552 <- lm(neighbors_soc_dis ~ education, data = wave_2_kibra)
lm3553 <- lm(neighbors_soc_dis ~ education, data = wave_3_kibra)
lm3554 <- lm(neighbors_soc_dis ~ education, data = wave_4_kibra)

lm3561 <- lm(neighbors_soc_dis ~ income_last_month, data = wave_1_kibra)
lm3562 <- lm(neighbors_soc_dis ~ income_last_month, data = wave_2_kibra)
lm3563 <- lm(neighbors_soc_dis ~ income_last_month, data = wave_3_kibra)
lm3564 <- lm(neighbors_soc_dis ~ income_last_month, data = wave_4_kibra)

lm357 <- plm(neighbors_soc_dis ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm358 <- plm(neighbors_soc_dis ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df35 <- summarySE(main_data_kibra, measurevar = "neighbors_soc_dis", groupvars = "session_label", na.rm = TRUE)
df35$neighbors_soc_dis <- round(df35$neighbors_soc_dis, digits = 2)

p35 <- ggplot(df35, mapping = aes(x = session_label, y = neighbors_soc_dis)) +
  geom_bar(stat = "summary", fill="#0033a1")+
  labs(y = "", x = "", title = "Out of the 10 neighbours that live closest
  to you, how many practice social distancing?", caption = "Includes all respondents from Kibera")+
  geom_text(data = df35, nudge_y =0.2, nudge_x = -0.25, aes(label = neighbors_soc_dis))+
  geom_errorbar(data = df35, aes(ymin=neighbors_soc_dis-ci, ymax=neighbors_soc_dis+ci), width=.2)
p35 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm3511, lm3512, lm3513, lm3514, lm3521, lm3522, lm3523, lm3524, lm3531, lm3532, lm3533, lm3534, lm3541, lm3542, lm3543, lm3544, lm3551, lm3552, lm3553, lm3554, lm3561, lm3562, lm3563, lm3564, lm357, lm358, df35, p35)

#Effectiveness of SD measures
lm3611 <- lm(effective ~ gender1, data = wave_1_kibra)
lm3612 <- lm(effective ~ gender1, data = wave_2_kibra)
lm3613 <- lm(effective ~ gender1, data = wave_3_kibra)
lm3614 <- lm(effective ~ gender1, data = wave_4_kibra)

lm3621 <- lm(effective ~ age1, data = wave_1_kibra)
lm3622 <- lm(effective ~ age1, data = wave_2_kibra)
lm3623 <- lm(effective ~ age1, data = wave_3_kibra)
lm3624 <- lm(effective ~ age1, data = wave_4_kibra)

lm3631 <- lm(effective ~ hh_size, data = wave_1_kibra)
lm3632 <- lm(effective ~ hh_size, data = wave_2_kibra)
lm3633 <- lm(effective ~ hh_size, data = wave_3_kibra)
lm3634 <- lm(effective ~ hh_size, data = wave_4_kibra)

lm3641 <- lm(effective ~ education, data = wave_1_kibra)
lm3642 <- lm(effective ~ education, data = wave_2_kibra)
lm3643 <- lm(effective ~ education, data = wave_3_kibra)
lm3644 <- lm(effective ~ education, data = wave_4_kibra)

lm3651 <- lm(effective ~ toilet, data = wave_1_kibra)
lm3652 <- lm(effective ~ toilet, data = wave_2_kibra)
lm3653 <- lm(effective ~ toilet, data = wave_3_kibra)
lm3654 <- lm(effective ~ toilet, data = wave_4_kibra)

lm3661 <- lm(effective ~ income_last_month, data = wave_1_kibra)
lm3662 <- lm(effective ~ income_last_month, data = wave_2_kibra)
lm3663 <- lm(effective ~ income_last_month, data = wave_3_kibra)
lm3664 <- lm(effective ~ income_last_month, data = wave_4_kibra)

lm367 <- plm(effective ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm368 <- plm(effective ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df36 <- summarySE(main_data_kibra, measurevar = "effective", groupvars = "session_label", na.rm = TRUE)
df36$effective <- round(df36$effective, digits = 2)

p36 <- ggplot(df36, mapping = aes(x = session_label, y = effective)) +
  geom_bar(stat = "summary", fill="#0036a1")+
  labs(y = "", x = "", title = "How effective are social distancing measures to slow down 
  the spread of the coronavirus in your community? 
  (on a scale from 1 to 5)", caption = "Includes all respondents from Kibera")+
  geom_text(data = df36, nudge_y =0.15, nudge_x = -0.25, aes(label = effective))+
  geom_errorbar(data = df36, aes(ymin=effective-ci, ymax=effective+ci), width=.2)
p36 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), axis.text.y = element_blank(), axis.ticks.y = element_blank())

names(effectiveness) = c("wave 1", "wave 2", "wave 3", "wave 4")
p361<-likert(effectiveness)
plot(p361, centered = FALSE, col=c("tomato2", "chocolate2", "azure2", "cornflowerblue", "blue3"), group.order = c("wave 1", "wave 2", "wave 3", "wave 4"))+
  ggtitle("How effective are social distancing measures to slow down 
  the spread of the coronavirus in your community? ")+
  labs(caption = "
       Includes all respondents from Kibera")+
  busara_theme +
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm3611, lm3612, lm3613, lm3614, lm3621, lm3622, lm3623, lm3624, lm3631, lm3632, lm3633, lm3634, lm3641, lm3642, lm3643, lm3644, lm3651, lm3652, lm3653, lm3654, lm3661, lm3662, lm3663, lm3664, lm367, lm368, df36, p36, p361)

#Obstacles in practicing SD
obstacles <- main_data_kibra[c("session_label", "obs_food", "obs_religion", "obs_phys_activities", "obs_work", "obs_health_care", "obs_friends_family", "obs_toilet", "obs_water", "obs_lonely", "obs_bored", "obs_group_pressure", "obs_privacy", "obs_other_people")]
obstacles <- melt(obstacles, "session_label")
obstacles$value[is.na(obstacles$value)] <- 0

df37 <- obstacles %>% dplyr::group_by(variable, session_label, value) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(variable, session_label) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
df37 <- subset(df37, value == 1)
df37$variable <- mapvalues(df37$variable, from = c("obs_food", "obs_religion", "obs_phys_activities", "obs_work", "obs_health_care", "obs_friends_family", "obs_toilet", "obs_water", "obs_lonely", "obs_bored", "obs_group_pressure", "obs_privacy", "obs_other_people"), to = c("Need for food", "Need for religious activities", "Need for physical activities", "Need to work or
need to find work", "Need for health care", "Need for meeting friends/family", "Shared toilet", "Need to get water", "Feeling lonely", "Feeling bored", "Group pressure", "Lack of privacy at home", "Other people not respecting distance"))

p37 <- ggplot(df37, mapping = aes(x = reorder(variable, -Percent), y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "% of respondents", x = "", title = "Over the past week, what have been your 
  biggest obstacles in practicing social distancing?", fill = "", caption = "Includes all respondents from Kibera")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p37 + busara_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(obstacles, df37, p37)

 #4.5 Spatial dynamics
#Situations where respondent stayed at home
lm3811 <- lm(avoided_contact ~ gender1, data = wave_1_kibra)
lm3812 <- lm(avoided_contact ~ gender1, data = wave_2_kibra)
lm3813 <- lm(avoided_contact ~ gender1, data = wave_3_kibra)
lm3814 <- lm(avoided_contact ~ gender1, data = wave_4_kibra)

lm3821 <- lm(avoided_contact ~ age1, data = wave_1_kibra)
lm3822 <- lm(avoided_contact ~ age1, data = wave_2_kibra)
lm3823 <- lm(avoided_contact ~ age1, data = wave_3_kibra)
lm3824 <- lm(avoided_contact ~ age1, data = wave_4_kibra)

lm3831 <- lm(avoided_contact ~ hh_size, data = wave_1_kibra)
lm3832 <- lm(avoided_contact ~ hh_size, data = wave_2_kibra)
lm3833 <- lm(avoided_contact ~ hh_size, data = wave_3_kibra)
lm3834 <- lm(avoided_contact ~ hh_size, data = wave_4_kibra)

lm3841 <- lm(avoided_contact ~ education, data = wave_1_kibra)
lm3842 <- lm(avoided_contact ~ education, data = wave_2_kibra)
lm3843 <- lm(avoided_contact ~ education, data = wave_3_kibra)
lm3844 <- lm(avoided_contact ~ education, data = wave_4_kibra)

lm3851 <- lm(avoided_contact ~ toilet, data = wave_1_kibra)
lm3852 <- lm(avoided_contact ~ toilet, data = wave_2_kibra)
lm3853 <- lm(avoided_contact ~ toilet, data = wave_3_kibra)
lm3854 <- lm(avoided_contact ~ toilet, data = wave_4_kibra)

lm3861 <- lm(avoided_contact ~ income_last_month, data = wave_1_kibra)
lm3862 <- lm(avoided_contact ~ income_last_month, data = wave_2_kibra)
lm3863 <- lm(avoided_contact ~ income_last_month, data = wave_3_kibra)
lm3864 <- lm(avoided_contact ~ income_last_month, data = wave_4_kibra)

lm387 <- plm(avoided_contact ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm388 <- plm(avoided_contact ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df38 <- summarySE(main_data_kibra, measurevar = "avoided_contact", groupvars = "session_label", na.rm = TRUE)
df38$avoided_contact <- round(df38$avoided_contact*100, digits = 2)
df38$ci <- df38$ci*100

p38 <- ggplot(df38, mapping = aes(x = session_label, y = avoided_contact)) +
  geom_bar(stat = "summary", fill="#0038a1")+
  labs(y = "% of respondents", x = "", title = "Did you experience situations in the past week where 
  you really would've liked to go somewhere but you didn’t 
  because you wanted to avoid contact with other people?", caption = "Includes all respondents from Kibera")+
  geom_text(data = df38, nudge_y =4, nudge_x = -0.25, aes(label = avoided_contact))+
  geom_errorbar(data = df38, aes(ymin=avoided_contact-ci, ymax=avoided_contact+ci), width=.2)
p38 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm3811, lm3812, lm3813, lm3814, lm3821, lm3822, lm3823, lm3824, lm3831, lm3832, lm3833, lm3834, lm3841, lm3842, lm3843, lm3844, lm3851, lm3852, lm3853, lm3854, lm3861, lm3862, lm3863, lm3864, lm387, lm388, df38, p38)

#Places where respondents would've like to go
acp <- main_data_kibra[c("session_label", "acp_food_shops", "acp_other_shops", "acp_church_mosque", "acp_health_care", "acp_school_uni", "acp_community_centre", "acp_friend_in_Nairobi", "acp_friend_elsewhere", "acp_toilet", "acp_water_joint", "acp_resto_bar", "acp_event", "acp_sports_facility")]
acp <- melt(acp, "session_label")
acp$value[is.na(acp$value)] <- 0

df39 <- acp %>% dplyr::group_by(variable, session_label, value) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(variable, session_label) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
df39 <- subset(df39, value == 1)
df39$variable <- mapvalues(df39$variable, from = c("acp_food_shops", "acp_other_shops", "acp_church_mosque", "acp_health_care", "acp_school_uni", "acp_community_centre", "acp_friend_in_Nairobi", "acp_friend_elsewhere", "acp_toilet", "acp_water_joint", "acp_resto_bar", "acp_event", "acp_sports_facility"), 
                           to = c("Food shops", "Other shops", "Church/mosque", "Doctor/hospital/pharmacy", "School/university", "Community centre", "Visit friends/relatives 
    within Nairobi", "Visit friends/relatives 
    outside of Nairobi", "Shared toilet", "Water joint", "Restaurant/bar", "To an event, f.ex. funeral", "Sports facility"))

p39 <- ggplot(df39, mapping = aes(x = reorder(variable, -Percent), y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "% of respondents", x = "", title = "Where would you have liked to go
  in these situations?", fill = "", caption = "Includes all respondents from Kibera")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p39 + busara_theme +
  theme(axis.text.x = element_text(angle = 55, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(acp, df39, p39)

#Situations where respondent avoided touching
lm4011 <- lm(avoided_touching ~ gender1, data = wave_1_kibra)
lm4012 <- lm(avoided_touching ~ gender1, data = wave_2_kibra)
lm4013 <- lm(avoided_touching ~ gender1, data = wave_3_kibra)
lm4014 <- lm(avoided_touching ~ gender1, data = wave_4_kibra)

lm4021 <- lm(avoided_touching ~ age1, data = wave_1_kibra)
lm4022 <- lm(avoided_touching ~ age1, data = wave_2_kibra)
lm4023 <- lm(avoided_touching ~ age1, data = wave_3_kibra)
lm4024 <- lm(avoided_touching ~ age1, data = wave_4_kibra)

lm4031 <- lm(avoided_touching ~ hh_size, data = wave_1_kibra)
lm4032 <- lm(avoided_touching ~ hh_size, data = wave_2_kibra)
lm4033 <- lm(avoided_touching ~ hh_size, data = wave_3_kibra)
lm4034 <- lm(avoided_touching ~ hh_size, data = wave_4_kibra)

lm4041 <- lm(avoided_touching ~ education, data = wave_1_kibra)
lm4042 <- lm(avoided_touching ~ education, data = wave_2_kibra)
lm4043 <- lm(avoided_touching ~ education, data = wave_3_kibra)
lm4044 <- lm(avoided_touching ~ education, data = wave_4_kibra)

lm4051 <- lm(avoided_touching ~ toilet, data = wave_1_kibra)
lm4052 <- lm(avoided_touching ~ toilet, data = wave_2_kibra)
lm4053 <- lm(avoided_touching ~ toilet, data = wave_3_kibra)
lm4054 <- lm(avoided_touching ~ toilet, data = wave_4_kibra)

lm4061 <- lm(avoided_touching ~ income_last_month, data = wave_1_kibra)
lm4062 <- lm(avoided_touching ~ income_last_month, data = wave_2_kibra)
lm4063 <- lm(avoided_touching ~ income_last_month, data = wave_3_kibra)
lm4064 <- lm(avoided_touching ~ income_last_month, data = wave_4_kibra)

lm407 <- plm(avoided_touching ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm408 <- plm(avoided_touching ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df40 <- summarySE(main_data_kibra, measurevar = "avoided_touching", groupvars = "session_label", na.rm = TRUE)
df40$avoided_touching <- round(df40$avoided_touching*100, digits = 2)
df40$ci <- df40$ci*100

p40 <- ggplot(df40, mapping = aes(x = session_label, y = avoided_touching)) +
  geom_bar(stat = "summary", fill="#0040a1")+
  labs(y = "% of respondents", x = "", title = "Did you experience situations in the past week where you 
       wanted to touch/be closer to someone but
       you didn`t because of social distancing?", caption = "Includes all respondents from Kibera")+
  geom_text(data = df40, nudge_y =4, nudge_x = -0.25, aes(label = avoided_touching))+
  geom_errorbar(data = df40, aes(ymin=avoided_touching-ci, ymax=avoided_touching+ci), width=.2)
p40 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm4011, lm4012, lm4013, lm4014, lm4021, lm4022, lm4023, lm4024, lm4031, lm4032, lm4033, lm4034, lm4041, lm4042, lm4043, lm4044, lm4051, lm4052, lm4053, lm4054, lm4061, lm4062, lm4063, lm4064, lm407, lm408, df40, p40)

#What respondents would've liked to do
ats <- main_data_kibra[c("session_label", "situations_stroke",  "situations_hug", "situations_kiss", "situations_stand_sit_closer", "situations_shake_hands")]
ats <- melt(ats, "session_label")
ats$value[is.na(ats$value)] <- 0

df41 <- ats %>% dplyr::group_by(variable, session_label, value) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(variable, session_label) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
df41 <- subset(df41, value == 1)
df41$variable <- mapvalues(df41$variable, from = c("situations_stroke", "situations_hug", "situations_kiss", "situations_shake_hands", "situations_stand_sit_closer"), to = c("Stroke someone", "Hug someone", "Kiss someone", "Shake hands", "Stand or sit 
  closer to someone"))

p41 <- ggplot(df41, mapping = aes(x = reorder(variable, -Percent), y= Percent, fill=session_label)) +
  geom_bar(stat = "identity", position="dodge")+
  labs(y = "% of respondents", x = "", title = "What would you have liked to do
  in these situations?", fill = "", caption = "Includes all respondents from Kibera")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p41 + busara_theme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(ats, df41, p41)

 #4.6 Life satisfaction
#Overall
lm4211 <- lm(satisfaction ~ gender1, data = wave_1_kibra)
lm4212 <- lm(satisfaction ~ gender1, data = wave_2_kibra)
lm4213 <- lm(satisfaction ~ gender1, data = wave_3_kibra)
lm4214 <- lm(satisfaction ~ gender1, data = wave_4_kibra)

lm4221 <- lm(satisfaction ~ age1, data = wave_1_kibra)
lm4222 <- lm(satisfaction ~ age1, data = wave_2_kibra)
lm4223 <- lm(satisfaction ~ age1, data = wave_3_kibra)
lm4224 <- lm(satisfaction ~ age1, data = wave_4_kibra)

lm4231 <- lm(satisfaction ~ hh_size, data = wave_1_kibra)
lm4232 <- lm(satisfaction ~ hh_size, data = wave_2_kibra)
lm4233 <- lm(satisfaction ~ hh_size, data = wave_3_kibra)
lm4234 <- lm(satisfaction ~ hh_size, data = wave_4_kibra)

lm4241 <- lm(satisfaction ~ education, data = wave_1_kibra)
lm4242 <- lm(satisfaction ~ education, data = wave_2_kibra)
lm4243 <- lm(satisfaction ~ education, data = wave_3_kibra)
lm4244 <- lm(satisfaction ~ education, data = wave_4_kibra)

lm4251 <- lm(satisfaction ~ toilet, data = wave_1_kibra)
lm4252 <- lm(satisfaction ~ toilet, data = wave_2_kibra)
lm4253 <- lm(satisfaction ~ toilet, data = wave_3_kibra)
lm4254 <- lm(satisfaction ~ toilet, data = wave_4_kibra)

lm4261 <- lm(satisfaction ~ income_last_month, data = wave_1_kibra)
lm4262 <- lm(satisfaction ~ income_last_month, data = wave_2_kibra)
lm4263 <- lm(satisfaction ~ income_last_month, data = wave_3_kibra)
lm4264 <- lm(satisfaction ~ income_last_month, data = wave_4_kibra)

lm427 <- plm(satisfaction ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm428 <- plm(satisfaction ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df42 <- summarySE(main_data_kibra, measurevar = "satisfaction", groupvars = "session_label", na.rm = TRUE)
df42$satisfaction <- round(df42$satisfaction, digits = 2)

p42 <- ggplot(df42, mapping = aes(x = session_label, y = satisfaction)) +
  geom_bar(stat = "summary", fill="#0042a1")+
  labs(y = "", x = "", title = "All things considered, how satisfied are you
  with your life as a whole these days?
  (1=Strongly dissatisfied, 5=Strongly satisfied)", caption = "Includes all respondents from Kibera")+
  geom_text(data = df42, nudge_y =0.12, nudge_x = -0.25, aes(label = satisfaction))+
  geom_errorbar(data = df42, aes(ymin=satisfaction-ci, ymax=satisfaction+ci), width=.2)
p42 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

names(satisfaction) = c("wave 1", "wave 2", "wave 3", "wave 4")
p421<-likert(satisfaction)
plot(p421, centered = FALSE, col=c("tomato2", "chocolate2", "azure2", "cornflowerblue", "blue3"), group.order = c("wave 1", "wave 2", "wave 3", "wave 4"))+
  ggtitle("All things considered, how satisfied are you
  with your life as a whole these days?")+
  labs(caption = "
       Includes all respondents from Kibera")+
  busara_theme +
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm4211, lm4212, lm4213, lm4214, lm4221, lm4222, lm4223, lm4224, lm4231, lm4232, lm4233, lm4234, lm4241, lm4242, lm4243, lm4244, lm4251, lm4252, lm4253, lm4254, lm4261, lm4262, lm4263, lm4264, lm427, lm428, df42, p42, p421)

#Effect of social distancing on life satisfaction
lm4311 <- lm(satisfaction_soc_dis ~ gender1, data = wave_1_kibra)
lm4312 <- lm(satisfaction_soc_dis ~ gender1, data = wave_2_kibra)
lm4313 <- lm(satisfaction_soc_dis ~ gender1, data = wave_3_kibra)
lm4314 <- lm(satisfaction_soc_dis ~ gender1, data = wave_4_kibra)

lm4321 <- lm(satisfaction_soc_dis ~ age1, data = wave_1_kibra)
lm4322 <- lm(satisfaction_soc_dis ~ age1, data = wave_2_kibra)
lm4323 <- lm(satisfaction_soc_dis ~ age1, data = wave_3_kibra)
lm4324 <- lm(satisfaction_soc_dis ~ age1, data = wave_4_kibra)

lm4331 <- lm(satisfaction_soc_dis ~ hh_size, data = wave_1_kibra)
lm4332 <- lm(satisfaction_soc_dis ~ hh_size, data = wave_2_kibra)
lm4333 <- lm(satisfaction_soc_dis ~ hh_size, data = wave_3_kibra)
lm4334 <- lm(satisfaction_soc_dis ~ hh_size, data = wave_4_kibra)

lm4341 <- lm(satisfaction_soc_dis ~ education, data = wave_1_kibra)
lm4342 <- lm(satisfaction_soc_dis ~ education, data = wave_2_kibra)
lm4343 <- lm(satisfaction_soc_dis ~ education, data = wave_3_kibra)
lm4344 <- lm(satisfaction_soc_dis ~ education, data = wave_4_kibra)

lm4351 <- lm(satisfaction_soc_dis ~ toilet, data = wave_1_kibra)
lm4352 <- lm(satisfaction_soc_dis ~ toilet, data = wave_2_kibra)
lm4353 <- lm(satisfaction_soc_dis ~ toilet, data = wave_3_kibra)
lm4354 <- lm(satisfaction_soc_dis ~ toilet, data = wave_4_kibra)

lm4361 <- lm(satisfaction_soc_dis ~ income_last_month, data = wave_1_kibra)
lm4362 <- lm(satisfaction_soc_dis ~ income_last_month, data = wave_2_kibra)
lm4363 <- lm(satisfaction_soc_dis ~ income_last_month, data = wave_3_kibra)
lm4364 <- lm(satisfaction_soc_dis ~ income_last_month, data = wave_4_kibra)

lm437 <- plm(satisfaction_soc_dis ~ income_last_month, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))
lm438 <- plm(satisfaction_soc_dis ~ wave_, data = main_data_kibra_fixed, model = "within", index = c("id", "wave"))

df43 <- summarySE(main_data_kibra, measurevar = "satisfaction_soc_dis", groupvars = "session_label", na.rm = TRUE)
df43$satisfaction_soc_dis <- round(df43$satisfaction_soc_dis, digits = 2)

p43 <- ggplot(df43, mapping = aes(x = session_label, y = satisfaction_soc_dis)) +
  geom_bar(stat = "summary", fill="#0043a1")+
  labs(y = "", x = "", title = "How do social distancing and the lack of physical
  contacts affect your overall satisfaction with life?
  (1=Strongly negative, 5=Strongly positive)", caption = "Includes all respondents from Kibera")+
  geom_text(data = df43, nudge_y =0.12, nudge_x = -0.25, aes(label = satisfaction_soc_dis))+
  geom_errorbar(data = df43, aes(ymin=satisfaction_soc_dis-ci, ymax=satisfaction_soc_dis+ci), width=.2)
p43 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

names(satisfaction_soc_dis) = c("wave 1", "wave 2", "wave 3", "wave 4")
p431<-likert(satisfaction_soc_dis)
plot(p431, centered = FALSE, col=c("tomato2", "chocolate2", "azure2", "cornflowerblue", "blue3"), group.order = c("wave 1", "wave 2", "wave 3", "wave 4"))+
  ggtitle("How do social distancing and the lack of physical
  contacts affect your overall satisfaction with life?")+
  labs(caption = "Includes all respondents from Kibera")+
  busara_theme +
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(lm4311, lm4312, lm4313, lm4314, lm4321, lm4322, lm4323, lm4324, lm4331, lm4332, lm4333, lm4334, lm4341, lm4342, lm4343, lm4344, lm4351, lm4352, lm4353, lm4354, lm4361, lm4362, lm4363, lm4364, lm437, lm438, df43, p43, p431)

#Income by Gender and Wave
df44 <- summarySE(main_data_kibra, measurevar = "income_last_month", groupvars = c("session_label", "gender1"), na.rm = TRUE)
p44 <- ggplot(main_data_kibra, mapping = aes(x = session_label, y = income_last_month, fill= gender1))+
  geom_bar(stat = "summary", position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Number of respondents", x = "", title = "Income by gender and wave", fill = "", caption = "Includes all respondents from Kibera")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.0), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Paired")
p44 + busara_theme + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.4, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) 

rm(df44, p44) 

#satisfaction_soc_dis by gender
wide_format_kibra_male <- subset(wide_format_kibra, gender1.1 == "Male")
wide_format_kibra_female <- subset(wide_format_kibra, gender1.1 == "Female")
satisfaction_soc_dis_male <- wide_format_kibra_male[, c("satisfaction_soc_dis_scale.1", "satisfaction_soc_dis_scale.2", "satisfaction_soc_dis_scale.3", "satisfaction_soc_dis_scale.4")]
satisfaction_soc_dis_male <- as.data.frame(satisfaction_soc_dis_male)
satisfaction_soc_dis_female <- wide_format_kibra_female[, c("satisfaction_soc_dis_scale.1", "satisfaction_soc_dis_scale.2", "satisfaction_soc_dis_scale.3", "satisfaction_soc_dis_scale.4")]
satisfaction_soc_dis_female <- as.data.frame(satisfaction_soc_dis_female)

names(satisfaction_soc_dis_male) = c("wave 1", "wave 2", "wave 3", "wave 4")
p45<-likert(satisfaction_soc_dis_male)
plot(p45, centered = FALSE, col=c("tomato2", "chocolate2", "azure2", "cornflowerblue", "blue3"), group.order = c("wave 1", "wave 2", "wave 3", "wave 4"))+
  ggtitle("How do social distancing and the lack of physical
  contacts affect your overall satisfaction with life? (males)")+
  labs(caption = "Includes all respondents from Kibera")+
  busara_theme +
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

names(satisfaction_soc_dis_female) = c("wave 1", "wave 2", "wave 3", "wave 4")
p46<-likert(satisfaction_soc_dis_female)
plot(p46, centered = FALSE, col=c("tomato2", "chocolate2", "azure2", "cornflowerblue", "blue3"), group.order = c("wave 1", "wave 2", "wave 3", "wave 4"))+
  ggtitle("How do social distancing and the lack of physical
  contacts affect your overall satisfaction with life? (females)")+
  labs(caption = "Includes all respondents from Kibera")+
  busara_theme +
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(satisfaction_soc_dis_male, satisfaction_soc_dis_female, wide_format_kibra_male, wide_format_kibra_female, p45, p46)

#Left the house yesterday by gender
df47 <- summarySE(main_data_kibra_yesterday, measurevar = "no_left_house", groupvars = c("session_label", "gender1"), na.rm = TRUE)
df47$no_left_house <- round(df47$no_left_house, digits = 2)

p47 <- ggplot(df47, mapping = aes(x = session_label, y = no_left_house, fill = gender1)) +
  geom_bar(stat = "summary", position = "dodge")+
  labs(y = "", x = "", fill = "Gender", title = "How many times did respondents leave their home
       on the day before the survey?", caption = "Includes all respondents who were 
       in Kibera the day before the survey")+
  scale_fill_brewer(palette = "Paired")
p47 + busara_theme +
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))

rm(df47, p47)
