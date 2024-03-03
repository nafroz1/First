# Bar plots and correlogram

library(ggplot2) 
library(plotly)
library(tidyverse)
library(ggcorrplot)

# data sample 
data <- data.frame(value = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)) 

ggplot(data, aes(x = "", y = value)) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("Value") + 
  ggtitle("Box Plot")

ggplot(mtcars, aes(x = "", y = mpg)) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("MPG") + 
  ggtitle("Box Plot")

# Box plot comparing the distribution between
# 2 levels of the same variable

ggplot(mtcars, aes(x = "", y = mpg , 
                   fill = as.factor(am))) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("MPG") + 
  ggtitle("Manual cars are more fuel efficient")

##################################
#### Box Plot Now with plotly ####
##################################
# data sample 
data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)

plot_ly(y = data, type = "box") 

plot_ly(data = mtcars ,
        y = ~mpg, 
        type = "box") 

# Box plot comparing the distribution between
# 2 levels of the same variable

t.test(mtcars$mpg - mtcars$am)


plot_ly(data = mtcars ,
        y = ~mpg, 
        color = ~as.factor(am) ,
        type = "box") 

##################################
#### Correlogram              ####
##################################

cor(mtcars$mpg , mtcars$wt)

cor(mtcars)

mtcars_cont <- mtcars %>% 
  select(-cyl , -vs , -am)

cor_mtcars <- cor(mtcars_cont)
str(cor_mtcars)

ggcorrplot(cor_mtcars,type ='upper')

ggcorrplot(cor_mtcars,type ='lower')

meanggcorrplot(cor_mtcars, 
           method = "circle" ,
           type = 'lower' ,
           lab = T,
           colors = c("blue", "white", "orange"))

### Correlogram now with plotly

palette <- colorRampPalette(c("orange", "white" , "darkblue"))

plot_ly (z = cor_mtcars, type = "heatmap", colors = palette(50) , x=colnames(cor_mtcars), y=rownames(cor_mtcars))
        

plot_ly(z = cor_mtcars, type = "heatmap" ,
        colors = palette(50) ,
        x=colnames(cor_mtcars), y=rownames(cor_mtcars))
