---
title: "Case Study 2: Bellabeat Analysis"
author: "Christopher Battillo"
date: "2023-08-07"
email: "christopher.battillo6144@gmail.com"
linkedin: "www.linkedin.com/in/christopher-battillo"
output: html_document
---

install.packages('janitor')
install.packages('skimr')
install.packages('tidyverse')
library(janitor)
library(skimr)
library(tidyverse)

setwd('C:/Users/chris_bontvjs/Documents')
getwd()


## Inspecting the datasets

## After setting directory, applied CSV files to "easy to read" names
daily_activity <- read_csv('Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv')
daily_sleep <- read_csv('Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv')
heartrate <- read_csv('Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv')
weight_log <- read_csv('Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv')

head(daily_activity)
head(daily_sleep)
head(heartrate)
head(weight_log)

str(daily_activity)
str(daily_sleep)
str(heartrate)
str(weight_log)


## Cleaning the datasets

## Finding out how many participants are in the dataset
n_unique(daily_activity$Id)
n_unique(daily_sleep$Id)
n_unique(heartrate$Id)
n_unique(weight_log$Id)

sum(duplicated(daily_activity))
sum(duplicated(heartrate))
sum(duplicated(weight_log))
sum(duplicated(daily_sleep))

## daily_sleep has 3 duplicates that must be removed. daily_activity has 33 unique ID's

duplicates <- daily_sleep[duplicated(daily_sleep) | duplicated(daily_sleep, fromLast = TRUE),]
View(duplicates)
daily_sleep <- daily_sleep[!duplicated(daily_sleep), ]
View(daily_sleep)

## This brought daily_sleep rows down from 413 to 410. 3 duplicate rows were removed. Still need to practice more on understanding of removing duplicates

sum(duplicated(daily_sleep))

any(is.na(daily_activity))
any(is.na(daily_sleep))
any(is.na(heartrate))
any(is.na(weight_log))

## This confirms there are no duplicates in daily_sleep, as well as no N.A values in daily_activity, daily_sleep, and heartrate. weight_log still has N.A values

head(weight_log)

## Only 2 rows in the entire "Fat" column has data. Since we have no steakholders to communicate with, the only choice is to remove the whole column

weight_log <- weight_log[, -which(names(weight_log) == 'Fat')]
head(weight_log)

rm(duplicates)


## Process the datasets

## Our goals is to give a general overview of the important parts of the data using the daily data. 'dailyActivity_merged' has all the columns merged into it titled 'daily', which helps to simplify the process. There are, however, some useful insights we can gather from the other sheets

daily_activity %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

## Creates a summary that combines the TotalSteps, TotalDistance, and SedentaryMinutes

daily_activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>% 
  summary()

## Creates a summary that combines VeryActiveMinutes, FairlyActiveMinutes, and SedentaryMinutes

daily_activity %>%
  select(Calories) %>%
  summary()

## Creates a summary of calories

daily_sleep %>%
  select(TotalMinutesAsleep, TotalTimeInBed, TotalSleepRecords) %>%
  summary()

## Creates a summary of TotalMinutesAsleep. TotalTimeInBed, and TotalSleepRecords

weight_log %>%
  select(WeightPounds, BMI) %>%
  summary()

## Creates a... nevermind you get it

heartrate %>%
  select(Value) %>%
  summary()


## Analysis and data visualizations, Daily Sleep plot

daily_sleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv", show_col_types = FALSE)

# Making a day_of_week column inside daily_sleep for plots

daily_sleep <- daily_sleep %>%
  mutate(day_of_week = weekdays(as.Date(SleepDay, format = "%m/%d/%Y")))


# Creating a new tibble out of daily_sleep to work with exclusively for one plot

daily_sleep_avg <- daily_sleep %>%
  group_by(day_of_week) %>%
  summarize(avg_minutes_asleep = mean(TotalMinutesAsleep))


# Ordering day_of_week in the desired order, + the plot

daily_sleep_avg$day_of_week <- factor(daily_sleep_avg$day_of_week, levels = c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(data = daily_sleep_avg, aes(x = day_of_week, y = avg_minutes_asleep)) +
  geom_col(fill = "#A18CD1") +
  geom_hline(aes(yintercept = mean(daily_sleep$TotalMinutesAsleep), color = "7.05"), linetype = "dashed") +
  geom_text(aes(label = sprintf("%.2f", avg_minutes_asleep/60)), vjust = -0.5, color = "black") +
  labs(title = "Average hours of sleep per day", x = "", y = "Total Hours Asleep") +
  scale_y_continuous(breaks = seq(0, ceiling(max(daily_sleep_avg$avg_minutes_asleep)), 120),
                     labels = function(x) paste0(x / 60, " ")) +
  guides(color = guide_legend(title = "Weekly Average:"))

head(select(arrange(mutate(daily_sleep_avg, 
                           avg_hours_asleep = round(avg_minutes_asleep / 60, 2)), 
                    desc(avg_hours_asleep)), day_of_week, avg_hours_asleep), 7)


# Daily Steps plot

# Making a day_of_week column inside daily_activity for plots

daily_activity <- daily_activity %>%
  mutate(day_of_week = weekdays(as.Date(ActivityDate, format = "%m/%d/%Y")))

# Reordering day_of_week in the desired order

daily_activity$day_of_week <- factor(daily_activity$day_of_week, levels = c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Creating a new tibble out of daily_activity to work for one plot

daily_activity_avg <- daily_activity %>%
  group_by(day_of_week) %>%
  summarize(avg_steps = mean(TotalSteps))

# Daily activity avg plot with legend

ggplot(data = daily_activity_avg, aes(x = day_of_week, y = avg_steps)) +
  geom_col(fill = "#065535") +
  geom_hline(aes(yintercept = mean(daily_activity$TotalSteps), color = "7608"), linetype = "dashed", show.legend = TRUE) +
  geom_text(aes(label = round(avg_steps)), vjust = -0.5, color = "black") +
  labs(title = "Average amount of steps per day", x = "", y = "Steps") +
  scale_color_manual(values = "orange") +
  guides(color = guide_legend(title = "Weekly Average:"))

# Showing the average daily steps in DESC order by day

head(arrange(daily_activity_avg, desc(avg_steps)), 7)


# Daily Calories plot

# Weekday vs average calories

daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate)
daily_activity$Weekday <- weekdays(daily_activity$ActivityDate)

# Aggregating calories

weekday_calories <- aggregate(Calories ~ Weekday, data = daily_activity, FUN = function(x) mean(x, na.rm = TRUE))

# Proper mean

average_calories <- mean(daily_activity$Calories, na.rm = TRUE)

#Reordering day_of_week in the desired order

weekday_calories$Weekday <- factor(weekday_calories$Weekday, levels = c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Removing decimals

weekday_calories$Calories <- floor(weekday_calories$Calories)

# Plot days vs average calories with a legend

ggplot(weekday_calories, aes(x = Weekday, y = Calories)) +
  geom_bar(stat = "identity", fill = "#FF6347", width = 0.5) +
  geom_text(aes(label = Calories), vjust = -0.5, color = "black") +
  geom_hline(aes(yintercept = average_calories, color = "2304"), linetype = "dashed") +
  labs(title = "Weekday vs average calories", x = "", y = "Calories") +
  scale_y_continuous(breaks = seq(0, max(weekday_calories$Calories), by = 200)) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 18),
        axis.text.y = element_text(size = 9)) +
  guides(color = guide_legend(title = "Weekly Average:", ncol = 1))

# Showing the average calories in DESC order

head(arrange(weekday_calories, desc(Calories)), 7)


# Total_Steps vs Calories with a legend

ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(aes(color = ifelse(TotalSteps <= 6000, "Lightly Active",
                                ifelse(TotalSteps <= 12000, "Fairly Active", "Very Active"))), size = 3) +
  geom_smooth(aes(color = "Total Steps"), method = "loess", se = FALSE, fill = "purple", alpha = 0.2, span = 0.5) +
  labs(title = "Total Steps and Activity Level vs. Calories", x = "Total Steps", y = "Calories", color = "Activity Level") +
  scale_color_manual(values = c("Lightly Active" = "#CD7F32", "Fairly Active" = "#C4C4C4", "Very Active" = "#FFD700", "Total Steps" = "purple")) +
  theme_minimal()


# TotalMinutesAlseep vs TotalTimeInBed
ggplot(data=daily_sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point(color="#6B3FA0")+ geom_smooth() + 
  labs(title="Total Minutes Asleep vs. Total Time in Bed")

# Rename SleepDay column in daily_sleep to date
daily_sleep <- daily_sleep %>%
  rename(date = SleepDay)

# Rename ActivityDate column in daily_activity to date
daily_activity <- daily_activity %>%
  rename(date = ActivityDate)

# Convert date columns to the desired format MM/DD/YYYY
daily_sleep$date <- format(as.Date(daily_sleep$date), "%m/%d/%Y")
daily_activity$date <- format(as.Date(daily_activity$date), "%m/%d/%Y")

# Merge data based on Id and date columns
merged_data <- merge(daily_activity[, c("Id", "TotalSteps", "date")], 
                     daily_sleep[, c("Id", "TotalMinutesAsleep", "date")], 
                     by = c("Id", "date"), all = TRUE)

# Remove rows with missing values
merged_data <- na.omit(merged_data)


# Plot TotalSteps vs TotalMinutesAsleep
ggplot(merged_data, aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Total Steps vs. Total Minutes Asleep", x = "Total Steps", y = "Total Minutes Asleep") +
  theme_minimal() +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

# Group the data by 'Id'
id_grp <- daily_activity %>% group_by(Id)

# Calculate the average amount of steps and sort in descending order
id_avg_step <- id_grp %>% summarize(avg_steps = mean(TotalSteps)) %>% arrange(desc(avg_steps))

# Convert the result to a dataframe
id_avg_step <- as.data.frame(id_avg_step)

# Create a column 'activity_level' based on average step categories
id_avg_step$activity_level <- cut(id_avg_step$avg_steps,
                                  breaks = c(-Inf, 4000, 8000, 12000, Inf),
                                  labels = c("sedentary", "lightly_active", "fairly_active", "very_active"),
                                  right = FALSE)

# Create a vector with activity levels for each 'Id' in the original dataframe
id_activity_level <- id_avg_step$activity_level[match(daily_activity$Id, id_avg_step$Id)]

# Add 'activity_level' column to the original dataframe
daily_activity$activity_level <- id_activity_level

# Correlation between calories steps and calories
ggplot(daily_activity, aes(x = TotalSteps, y = Calories, color = activity_level)) +
  geom_point(shape = 16, size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "purple", fill = "purple", alpha = 0.2, span = 0.5) +
  labs(title = "Correlation of Total Steps and Calories", x = "Total Steps", y = "Calories", color = "Activity Level") +
  theme_minimal() +
  scale_color_manual(values = c("sedentary" = "#5A5A5A", "lightly_active" = "#CD7F32", "fairly_active" = "#C4C4C4", "very_active" = "#FFD700")) +
  theme(legend.position = "bottom")

# Percent of activity in minutes
very_active_mins <- sum(daily_activity$VeryActiveMinutes)
fairly_active_mins <- sum(daily_activity$FairlyActiveMinutes)
lightly_active_mins <- sum(daily_activity$LightlyActiveMinutes)
sedentary_mins <- sum(daily_activity$SedentaryMinutes)

slices <- c(very_active_mins, fairly_active_mins, lightly_active_mins, sedentary_mins)
labels <- c('Very active minutes: 1.74%', 'Fairly active minutes: 1.11%', 'Lightly active minutes: 15.82%', 'Sedentary minutes: 81.33%')

colors <- c("#FFD700", "#C4C4C4", "#CD7F32", "#5A5A5A")

pie(slices, labels = labels,
    main = 'Percentage of activity in minutes',
    col = colors,
    border = "white", font.main = 2,cex.main = 1.5)

## Act - Step 6: Conclusion and Next Steps

# Total steps per day (7,608 out of 10,000) is lower than recommended

# Calories remain consistent outside of Tuesdays, which is surprising; Tuesdays also include the second highest amount of steps and the least amount of sleep

# The more active someone is, the more calories they consume

# Total steps and sleep have no correlation

# Total steps and calories correlate positively. More steps = more calories

# The vast majority of time is spent sedentary


# Overall the data is inconclusive; two months is not enough time to conclude results. 









