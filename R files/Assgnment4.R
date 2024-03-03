library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

Mcdonalds <- read_excel("C:/Users/Nahid/Desktop/R/R files/Mcdonalds.xlsx")
View(Mcdonalds)

mcd <- Mcdonalds
summary(mcd)
str(mcd)

temp0 <- mcd %>% 
  mutate(d1= row.names(mcd))

######### Scatter plot using ggplot2 ###############

ggplot(data = mcd, aes(x = Income, y = `Average Frequency`)) +
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")


# This is just an example of overlay of different plots
ggplot(data = mcd ) +
  geom_point(aes(x = Income, y = `Average Frequency`) , col = 'blue') 

######### Scatter plot using plotly ###############

plot_ly(data = temp0 , 
        x = ~ Income ,
        y = ~ `Average Frequency` ,
        type = 'scatter' ,
        mode = 'markers' ,
        text = ~ d1)



#-------------------------------------------------------------------------------

dow1 <- read_excel("C:/Users/Nahid/Desktop/R/R files/Dow-1.xlsx")
View(dow1)



