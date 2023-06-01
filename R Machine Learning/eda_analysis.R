install.packages("vip")

require(tree)
# load required libraries
library(caret)
library(MASS)
library(randomForest)
library(party)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(vip)

# get working directory and change if needed
getwd()
setwd('C:/Users/oluwanifemi.aweda/Documents/Iremide-R')

# import your dataset
df <- read.csv(file ='train.csv')

# perform basic checks on the dataset
# check all columnames in the dataframe
colnames(df)

# see an overview of the data (glimpse is an alternative to str)
str(df)
glimpse(df)

# view the df in tabular form
view(df)

# check the dimension of the data
dim(df)

# see a summary statistic of the data
summary(df)

# check for null values in the data
colSums(is.na(df))

# see the number of unique values in each column of the data
sapply(categorical_cols, function(x) if(is.factor(x)) nlevels(x) else length(unique(x)))


# create a new instance of the data
df_clean = df

# convert columns to categorical according to the docs
# job, marital, education, default, housing, loan, contact, day, month, poutcome
df_clean <- df_clean %>%
  mutate_at(vars(job, marital, education, default, housing, loan, contact, day, month, poutcome), as.factor)

# check if the operation was successful
glimpse(df_clean)

# select categorical columns
categorical_cols <- df_clean %>%
  select_if(is.factor)

# select numerical columns
numerical_cols <- df_clean %>%
  select_if(is.numeric)


# Age analysis

# get the minimun value for age
min_age <- min(df_clean$age)
# get the maximum value for age
max_age <- max(df_clean$age)

# plot distribution
ggplot(df_clean, aes(x = age)) +
  geom_histogram(bins = 10, fill = "blue", alpha = 0.8) +
  labs(x = "Age Distribution", y = "Count") +
  facet_wrap(~ y, nrow = 1) +
  ggtitle("Distribution of Age by Outcome")

ggplot(df_clean, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(x = "Age Distribution", y = "Count") +
  ggtitle("Distribution of Age")


# create age ranges
numerical_cols$age_range <- cut(numerical_cols$age, breaks = c(0, 18, 30, 40, 50, 60, Inf), 
                        labels = c("0-18yrs", "19-30yrs", "31-40yrs", "41-50yrs", "51-60yrs", "60+yrs"))

numerical_cols <- numerical_cols %>%
  mutate_at(vars(age_range), as.factor)

glimpse(numerical_cols)

# Get frequency table of categories
freq_table <- table(numerical_cols$age_range)

# Create bar plot
barplot(freq_table, main = "Distribution of Categories", 
        xlab = "Category", ylab = "Frequency")


# create call duration ranges
numerical_cols$call_duration <- cut(numerical_cols$duration, breaks = c(0, 10, 20, 30, 40, 50, 60, Inf), 
                                labels = c("0-10mins", "11-20mins", "21-30mins", "31-40mins", "41-50mins", "51-60mins", "1hr+"))

numerical_cols <- numerical_cols %>%
  mutate_at(vars(call_duration), as.factor)

# Get frequency table of categories
freq_table1 <- table(numerical_cols$call_duration)

# Create bar plot
barplot(freq_table1, main = "Distribution of Categories", 
        xlab = "Category", ylab = "Frequency")


categorical_plot <- names(categorical_cols)[sapply(categorical_cols, is.factor)]

# Create bar plots for each categorical column
plot_list <- lapply(categorical_plot, function(col) {
  plot_data <- as.data.frame(table(categorical_cols[, col]))
  plot <- ggplot(plot_data, aes_string(x = names(plot_data)[1], y = names(plot_data)[2])) +
    geom_bar(stat = "identity") +
    ggtitle(col) +
    xlab(col) +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot
})

grid.arrange(grobs = plot_list, ncol = 2)


# Filter numerical columns
numerical_plot <- names(numerical_cols)[sapply(numerical_cols, is.numeric)]

# Create list of histogram plots for each numerical column
plot_list <- lapply(numerical_plot, function(col) {
  plot <- ggplot(numerical_cols, aes_string(x = col)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "white") +
    ggtitle(col) +
    xlab(col) +
    ylab("Count")
  plot
})

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

sapply(numerical_cols, function(x) if(is.numeric(x)) nlevels(x) else length(unique(x)))


# Get frequency table of month range
month_table <- table(categorical_cols$month)
month_order <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
categorical_cols$month <- factor(categorical_cols$month, levels = month_order)
month_table <- table(categorical_cols$month)
# Create bar plot
barplot(month_table, main = "Distribution of Categories", 
        xlab = "Month", ylab = "Frequency", 
        las = 2)


# create correlation for numeric variables
numerical_cols1 <- numerical_cols %>%
  select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(numerical_cols1)

# Create correlation plot
corrplot(cor_matrix, method = "color")

my_df <- my_df[, c("x", "z")]
