# Libraries
library(tidyverse)
library(plotly)

# Data
#temp0 <- mtcars
#Tidyverse already has readr so no need to get the readr library

hr1 <- read_csv("C:/Users/Nahid/Desktop/HR_comma_sep.csv")
View(hr1)


summary(hr1$satisfaction_level)

summary(hr1$last_evaluation)




#COLORED VERSION

# Histogram for satisfaction_level with color
ggplot(hr1, aes(x = satisfaction_level,)) +
  geom_histogram(binwidth = 0.10, color = "#a0db8e",fill = '#a0db8e') +
  labs(title = "Employee Satisfaction Histogram",
       x = "Satisfaction Level",
       y = "Count")
# box plots and correlogram

library(ggplot2) 
library(plotly)
library(tidyverse)
library(ggcorrplot)


ggplot(hr1, aes(x = "Satisfaction Level",
                y = "Count")) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("Value") + 
  ggtitle("Box Plot")


ggplot(hr1, aes(x = satisfaction_level,)) +
  geom_histogram(binwidth = 0.10, color = "#a0db8e",fill = '#a0db8e') +
  labs(title = "Employee Satisfaction Histogram",
       x = "Satisfaction Level",
       y = "Count")


ggplot(hr1, aes(x = "Satisfaction Level",
                y = "Count" , 
                   fill = as.factor(left))) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("Value") + 
  ggtitle("Boxplot")


# Perform the t-test
t_test_result <- t.test(hr1$satisfaction_level ~ hr1$left)


# Create the Box Plots
ggplot(hr1, aes(x = factor(left), y = satisfaction_level, fill = factor(left))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  xlab("") +
  ylab("Satisfaction Level") +
  ggtitle("Employee Satisfaction by Employee Status") +
  scale_x_discrete(labels = c("Left = 0", "Left = 1")) +
  labs(fill = "Attrition") +
  theme_minimal()

ggplot(hr1, aes(x = factor(left), y = last_evaluation, fill = factor(left))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  xlab("") +
  ylab("Last Evaluation") +
  ggtitle("Last Evaluation by Employee Status") +
  scale_x_discrete(labels = c("Left = 0", "Left = 1")) +
  labs(fill = "Attrition") +
  theme_minimal()


#Correlogram: 


cor(hr1$satisfaction_level , hr1$last_evaluation)

cor(hr1)

hr1_cont <- hr1 %>%
  select(satisfaction_level, last_evaluation, average_montly_hours, time_spend_company)

view(hr1_cont)

cor_hr1 <- cor(hr1_cont)
str(cor_hr1)


ggcorrplot(cor_hr1,type ='upper')

ggcorrplot(cor_hr1, 
           method = "square" ,
           type = 'lower' ,
           lab = T,
           colors = c("red", "pink", "blue"))
cor(cor_hr1)

### Correlogram now with plotly

palette <- colorRampPalette(c("orange", "green" , "darkblue"))

plot_ly (z = cor_hr1, type = "heatmap", colors = palette(50) , 
         x=colnames(cor_hr1), y=rownames(cor_hr1))


plot_ly(z = cor_hr1, type = "heatmap" ,
        colors = palette(50) ,
        x=colnames(cor_hr1), y=rownames(cor_hr1))

#Last Evaluation vs. Average Monthly Hours: T
#here is a positive correlation of approximately 21.95%. This means that as last evaluation scores increase, 
#average monthly hours also tend to increase, but the relationship is not very strong.

#Satisfaction Level vs. Last Evaluation: There is a 
#negative correlation of approximately -31.00%. 
#This indicates that as satisfaction level increases, last evaluation tends to decrease.
