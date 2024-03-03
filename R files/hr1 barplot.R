# Libraries
library(tidyverse)
library(plotly)

# Data
temp0 <- mtcars
#Tidyverse already has readr so no need to get the readr library

hr1 <- read_csv("C:/Users/Nahid/Desktop/HR_comma_sep.csv")
View(hr1)

#histogram has 1 variables and then counts occurences by bins. 

# 1. Create a histogram for employee satisfaction and last evaluation
summary(hr1$satisfaction_level)
ggplot(hr1,aes(x=satisfaction_level)) + geom_histogram(binwidth = 0.10)

summary(hr1$last_evaluation)
ggplot(hr1,aes(x=last_evaluation)) + geom_histogram(binwidth =0.05)
# choose a binwidth that's lower than maximum which we find from summary of that variable like satisfaction level. 


#COLORED VERSION

# Histogram for satisfaction_level with color
ggplot(hr1, aes(x = satisfaction_level,)) +
  geom_histogram(binwidth = 0.10, color = "#a0db8e",fill = '#a0db8e') +
  labs(title = "Employee Satisfaction Histogram",
       x = "Satisfaction Level",
       y = "Count")

# Summary of last_evaluation
summary(hr1$last_evaluation)

# Histogram for last_evaluation with color
ggplot(hr1, aes(x = last_evaluation, fill = "red")) +
  geom_histogram(binwidth = 0.10, color = "red") +
  labs(title = "Last Evaluation Histogram",
       x = "Last Evaluation",
       y = "Count")




#2. Create a bar plot for the average satisfaction of employees by the variable that indicates whether they left of stayed at the company.

summary(hr1$left)

hr2 <- hr1 %>% 
  mutate(left =ifelse(left==0, 'Stayed', 'Left'))%>% 
  group_by(left) %>% 
  summarise(satisfaction_level = mean(satisfaction_level))


plot_ly(
  data = hr2 ,
  x = ~as.factor(left),
  y = ~satisfaction_level,
  color = ~as.factor(left) ,
  colors = c('blue' , '#03b6fc') , #two colors for two categories
  type = "bar"
) %>% hide_legend()%>% 
  layout(title = 'Average Satisfaction by Employee Status', 
         xaxis = list(title = ''), 
         yaxis = list(title = 'Satisfaction Level'))

library(plotly)
library(dplyr)

# Summary of the 'left' variable
summary_hr1 <- summary(hr1$left)

# Calculate the average last evaluation by employee status
hr2 <- hr1 %>%
  mutate(left = ifelse(left == 0, 'Stayed', 'Left')) %>%
  group_by(left) %>%
  summarise(last_evaluation = mean(last_evaluation))

# Create the bar plot
plot_ly(
  data = hr2,
  x = ~as.factor(left),
  y = ~last_evaluation,
  color = ~as.factor(left),
  colors = c('blue', '#03b6fc'), # Two colors for two categories
  type = "bar"
) %>%
  hide_legend() %>%
  layout(
    title = 'Average Last Evaluation by Employee Status',
    xaxis = list(title = ''),
    yaxis = list(title = 'Last Evaluation')
  )
