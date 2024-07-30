######## ASSIGNMENT 2


# Load necessary libraries
library(dplyr) 
library(ggplot2) 
library(readxl) 
library(tidyr) 

# Load the dataset
file_path <- "C:/Users/amwikali/Desktop/MSc. DATA SCIENCE/LMDS 1st Sem/Data Visualization/COVID-19 Survey Student Responses.csv"
survey <- read.csv(file_path)

# Summary statistics
summary(survey)

# Classify students by age groups
survey <- survey %>%
  mutate(Age_Group = case_when(
    `Age.of.Subject` <= 11 ~ "Elementary School Students",
    `Age.of.Subject` >= 12 & `Age.of.Subject` <= 14 ~ "Junior High School Students",
    `Age.of.Subject` >= 15 & `Age.of.Subject` <= 17 ~ "Senior High School Students",
    `Age.of.Subject` >= 18 ~ "College Students"
  ))

# 1. Age Distribution of Surveyed Students
age_counts <- survey %>%
  group_by(`Age.of.Subject`) %>%
  summarise(count = n())

ggplot(age_counts, aes(x = `Age.of.Subject`, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Age Distribution of Surveyed Students",
       x = "Age",
       y = "Number of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  coord_flip()

# 2. Distribution of Students by Level of Schooling
age_group_counts <- survey %>%
  group_by(Age_Group) %>%
  summarise(count = n())

colors <- c("Elementary School Students" = "red",
            "Junior High School Students" = "blue",
            "Senior High School Students" = "purple",
            "College Students" = "orange")

ggplot(age_group_counts, aes(x = "", y = count, fill = Age_Group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Students by Level of Schooling",
       fill = "Age Group",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = colors) +
  theme_void() +
  theme(legend.position = "right")

# 3. Average Time Spent on Online Class by Level of Schooling
time_online_avg <- survey %>%
  group_by(Age_Group) %>%
  summarise(avg_time_online = mean(`Time.spent.on.Online.Class`))

ggplot(time_online_avg, aes(x = Age_Group, y = avg_time_online, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Time Spent on Online Class by Level of Schooling",
       x = "Level of Schooling",
       y = "Average Time (hours)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# 4. Distribution of Rating of Online Class by Level of Schooling
survey <- survey %>%
  mutate(Numerical_Rating = case_when(
    `Rating.of.Online.Class.experience` == "Excellent" ~ 5,
    `Rating.of.Online.Class.experience` == "Good" ~ 4,
    `Rating.of.Online.Class.experience` == "Average" ~ 3,
    `Rating.of.Online.Class.experience` == "Poor" ~ 2,
    `Rating.of.Online.Class.experience` == "Very Poor" ~ 1
  ))

ggplot(survey, aes(x = Age_Group, y = Numerical_Rating, fill = Age_Group)) +
  geom_violin() +
  labs(title = "Distribution of Rating of Online Class by Level of Schooling",
       x = "Level of Schooling",
       y = "Rating of Online Class") +
  theme_minimal() +
  scale_y_continuous(labels = c(
    "Very Poor",
    "Poor",
    "Average",
    "Good",
    "Excellent"
  ), breaks = 1:5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = element_blank())

# 5. Average Time Spent on Various Activities by Level of Schooling
avg_time_spent <- survey %>%
  group_by(Age_Group) %>%
  summarise(avg_time_online = mean(`Time.spent.on.Online.Class`),
            avg_time_self_study = mean(`Time.spent.on.self.study`),
            avg_time_fitness = mean(`Time.spent.on.fitness`),
            avg_time_sleep = mean(`Time.spent.on.sleep`),
            avg_time_social_media = mean(`Time.spent.on.social.media`),
            avg_time_tv = mean(`Time.spent.on.TV`))

avg_time_spent_long <- avg_time_spent %>%
  pivot_longer(cols = c(avg_time_online, avg_time_self_study, avg_time_fitness,
                        avg_time_sleep, avg_time_social_media, avg_time_tv), 
               names_to = "Activity", values_to = "Avg_Time_spent")

ggplot(avg_time_spent_long, aes(x = Age_Group, y = Avg_Time_spent, fill = Activity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Time Spent on Various Activities by Level of Schooling",
       x = "Level of Schooling",
       y = "Average Time Spent (hours)",
       fill = "Activity") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "College Students" = "College",
    "Junior High School Students" = "JHS",
    "Senior High School Students" = "SHS",
    "Elementary School Students" = "Elem"
  ))

# 6. Health Issues Reported by Level of Schooling
ggplot(survey, aes(x = Age_Group, fill = `Health.issue.during.lockdown`)) +
  geom_bar() +
  labs(title = "Health Issues Reported by Level of Schooling in New Delhi",
       x = "Level of Schooling",
       y = "Count",
       fill = "Health Issue") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "College Students" = "College",
    "Junior High School Students" = "JHS",
    "Senior High School Students" = "SHS",
    "Elementary School Students" = "Elem"
  ))

# 7. Heatmap  for top 3 social media platforms by level of schooling
# Preparing data for heatmap
social_media_heatmap_data <- survey %>%
  group_by(`Age_Group`, `Prefered.social.media.platform`) %>%
  summarise(count = n()) %>%
  arrange(`Age_Group`, desc(count)) %>%
  group_by(`Age_Group`) %>%
  slice_head(n = 3) %>%
  ungroup()

# Plotting heatmap
ggplot(social_media_heatmap_data, aes(x = `Prefered.social.media.platform`, y = `Age_Group`, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Time Spent on Top 3 Social Media Platforms by Level of Schooling",
       x = "Social Media Platform",
       y = "Level of Schooling",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.Faceted bar chart for devices used for online learning by level of schooling
ggplot(devices_count, aes(x = `Medium.for.online.class`, y = count, fill = `Medium.for.online.class`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ `Age_Group`, scales = "free_y") +
  labs(title = "Devices Used for Online Learning by Level of Schooling",
       x = "Device Used for Online Learning",
       y = "Count",
       fill = "Device") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

