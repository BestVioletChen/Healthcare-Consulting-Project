# CLEANING/TIDYING DATA
#Tidy to columns subject, time, wavelength, exposure, visibility
# subject|time|wavelength|exposure|visibility|
# 1      |0   |white     |+1      |Y         |
# 1      |0   |white     | 0      |Y         |
# 1      |0   |white     |-1      |Y         |
# 1      |0   |415nm     |+1      |Y         |
# 1      |0   |415nm     | 0      |Y         |
# 1      |0   |415nm     |-1      |Y         |   #and so on


# NAs (participant did not attend follow-up) = 9
# - (image was blury/could not be interpreted) = -9

# import packages and data
pacman::p_load(openxlsx, tidyr, magrittr, dplyr, stringr, ggplot2)

# Goal: Create dataframes that follow principles of tidy data
part1 <- data.frame(read.xlsx("time interval and observations_MSSP.xlsx", 
                    sheet = "Part 1 observations", fillMergedCells = TRUE))

#create column name for day, wavelength, and exposure
col_names <- paste(part1[1,], part1[2,], part1[3, ])
colnames(part1) <- col_names

#pivot_longer
part1 %<>% pivot_longer(cols = "0 hour  White -1":"Day 21 550nm 0.97902097902098195", names_to = "time", values_to = "value")
colnames(part1) <-   c("Subject", "BMI", "MM", "fat", "Days" ,"Gender","skin_tone", "time_light_exp", "value" )

#create  filter out extra rows         
part1 %<>% filter(Subject != "Subject" & Subject != "Subject ") %>% 
           filter(BMI != "male ")

#recode missing/unusable observations
part1$value <-  recode(part1$value, "Y" = "1", 
                                    "N" = "0",
                                    "-" = "9",
                                    .missing = "-9")

#create new separate columns for time, wavelength, and exposure
#separate strings using regular expressions

#create time column
part1 <- part1 %>%  mutate(time_text =str_match(part1$time_light_exp, "Day \\d*\\d|\\d hour")[,1],
                  wave_length = str_match(part1$time_light_exp, "(White|415nm|460nm|550nm)")[,1],
                  exposure = str_match(part1$time_light_exp, "(-1|1|0)$")[,1])
#part1$time <-  as.numeric(str_match(part1$time_text, "\\d*$"))
part1$exposure <- as.character(part1$exposure)
part1$wave_length <- as.factor(part1$wave_length)


# lgc <-part1$time_text == "0 hour"
# part1[lgc,12] <- 0
#   
# lgc <-part1$time_text == "3 hour"
# part1[lgc,12] <- 3/24


part1$time_text <- factor(part1$time_text, 
                              levels = c("0 hour", "3 hour", "Day 1", "Day 3", "Day 7", "Day 14", "Day 21"))

part1$time_text <-part1$time_text %>% recode("0 hour" = "0h",
                                             "3 hour" = "3h",
                                             "Day 1" = "1d",
                                             "Day 3" = "3d",
                                             "Day 7" = "7d",
                                             "Day 14" = "14d",
                                             "Day 21" = "21d",)

part1$time_n <-part1$time_text %>% recode("0h" = 0,
                                          "3h" = 0.125,
                                          "1d" = 1,
                                          "3d" = 3,
                                          "7d" = 7,
                                          "14d" = 14,
                                          "21d" = 21,)


part1_clean <- part1%>%filter(value<9 & value>-9) %>% na.omit()
part1_clean$skin_tone <- as.numeric(part1_clean$skin_tone)
part1_clean$MM <-  as.numeric(part1_clean$MM)
part1_clean$fat <-  as.numeric(part1_clean$fat)
part1_clean$BMI <-  as.numeric(part1_clean$BMI)
part1_clean$value <- as.numeric(part1_clean$value)
part1_clean$Gender <- as.factor(part1_clean$Gender)
part1_clean$Subject <- as.numeric(part1_clean$Subject)

part1_clean$exposure <- as.factor(part1_clean$exposure) 
part1_clean$exp_n1 <- if_else(part1_clean$exposure == -1, 1, 0)
part1_clean$exp_p1 <- if_else(part1_clean$exposure == 1, 1, 0)

subj <- part1_clean %>% filter(!duplicated(Subject)) %>% dplyr::select("Subject", "fat", "MM", "BMI", "Gender", "skin_tone")

mean(subj$fat)
sd(subj$fat)

part1_clean$fat_scale <- (part1_clean$fat - mean(subj$fat)) / sd(subj$fat)
part1_clean$mm_scale <- (part1_clean$MM - mean(subj$MM)) / sd(subj$MM)


cols <- c("time_text", "wave_length", "exposure")
prop <- part1_clean %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(m_val = mean(value),
            n = n()) %>% 
  mutate(light = paste(wave_length, exposure, sep = " "),
         prop_conf_int = 1.96*sqrt((m_val*(1-m_val))/n))


#------------------------------------------------------------------------------#
part2 <- read.xlsx("time interval and observations_MSSP.xlsx", 
                    sheet = "Part 2 observations ", fillMergedCells = TRUE)
part3 <- read.xlsx("time interval and observations_MSSP.xlsx", 
                    sheet = "Part 3 Results of review", fillMergedCells = TRUE)
#------------------------------------------------------------------------------#