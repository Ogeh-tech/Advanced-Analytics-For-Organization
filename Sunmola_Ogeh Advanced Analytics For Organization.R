# Check current working directory
getwd()
# List all files in the folder
list.files()
# Install required packages (run once only)
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
# Load libraries for data manipulation and visualization
library(tidyverse)  # includes dplyr, ggplot2, etc.
library(ggplot2)    # plotting
library(dplyr)      # data manipulation
library(readr)      # reading csv files

# Load the dataset into R
df <- read.csv("turtle_reviews.csv")

# View first 6 rows
head(df)

# Check structure and data types
str(df)

# Summary statistics
summary(df)

# Remove unnecessary columns
df <- df %>% select(-language, -platform)

# Rename columns for clarity
df <- df %>% rename(renumeration = `remuneration..k..`,spend = `spending_score..1.100.`)summary(df)  

# Check structure and data types
str(df)

# Rename columns for clarity
df <- df %>%rename(income = "remuneration..k..",spend = "spending_score..1.100.")

# Remove rows with missing values
df <- na.omit(df)

# Check again
summary(df)
str(df)

# Histogram for loyalty points
ggplot(df, aes(x = loyalty_points)) + geom_histogram(fill = "steelblue", bins = 30) +labs(title = "Distribution of Loyalty Points",x = "Loyalty Points",y = "Frequency")

# Boxplot for loyalty points
ggplot(df, aes(y = loyalty_points)) + geom_boxplot(fill = "orange") + labs(title = "Boxplot of Loyalty Points",y = "Loyalty Points")

# Scatterplot: Spending Score vs Loyalty Points
ggplot(df, aes(x = spend, y = loyalty_points)) + geom_point(color = "darkgreen") + labs(title = "Spending Score vs Loyalty Points", x = "Spending Score", y = "Loyalty Points")

# Scatterplot: Income vs Loyalty Points
ggplot(df, aes(x = income, y = loyalty_points)) + geom_point(color = "purple") + labs(title = "Income vs Loyalty Points", x = "Income", y = "Loyalty Points")

# Boxplot by gender
ggplot(df, aes(x = gender, y = loyalty_points)) + geom_boxplot(fill = "lightblue") + labs(title = "Loyalty Points by Gender")

# Boxplot by education level
ggplot(df, aes(x = education, y = loyalty_points)) + geom_boxplot(fill = "lightgreen") + labs(title = "Loyalty Points by Education")

# Average loyalty points by education
df %>% group_by(education) %>% summarise(avg_loyalty = mean(loyalty_points))


##--------week 6

# Summary statistics of key variables
summary(df$loyalty_points)
summary(df$income)
summary(df$spend)
summary(df$age)

install.packages("moments")
library(moments)

# Check skewness
skewness(df$loyalty_points)

# Check kurtosis
kurtosis(df$loyalty_points)


# Build Multiple Linear Regression Model
model <- lm(loyalty_points ~ age + income + spend, data = df)

summary(model)

# Plot model diagnostics
par(mfrow = c(2,2))
plot(model)

# Create new customer scenario
new_customer <- data.frame(age = 35,income = 60,spend = 80)


# Predict loyalty points
predict(model, new_customer)

predict(model, new_customer)










