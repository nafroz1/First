#################################
#### Scatter and Line Plots #####
#################################

library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

######## Data ############
temp0 <- mtcars %>% 
  mutate(car_name = row.names(mtcars))

######### Scatter plot using ggplot2 ###############

ggplot(data = mtcars , aes(x = mpg , y = wt)) +
  geom_point( ) +
  geom_smooth(method = 'lm' )

# This is just an example of overlay of different plots
ggplot(data = mtcars ) +
  geom_point(aes(x = mpg , y = wt) , col = 'blue') +
  geom_point(aes(x = mpg , y = cyl) , col = 'green') 

######### Scatter plot using plotly ###############

plot_ly(data = temp0 , 
        x = ~mpg ,
        y = ~wt ,
        type = 'scatter' ,
        mode = 'markers' ,
        text = ~car_name)

################ Line Plots ###################

str(ldeaths)
class(ldeaths)

temp1 <- data.frame(deaths=as.matrix(ldeaths), temp_date= as.numeric(time(ldeaths)) ) %>% 
  mutate(month = floor(12*(temp_date %% 1)) + 1 ,
         year = floor(temp_date) ,
         date_text = paste(month , 1 , year , sep = '/') ,
         date = mdy(date_text)) #mdy is a function of lubridate

str(temp1)  


# line plot with ggplot2
ggplot(data = temp1 , aes(x = date , y = deaths)) +
  geom_line( ) 


# line plot with plotly
plot_ly(data = temp1 , 
        x = ~date ,
        y = ~deaths ,
        type = 'scatter' ,
        mode = 'line')

# Average per month

temp2 <- temp1 %>% 
  mutate(month2 = month(date , label = T)) %>% 
  group_by(month2) %>% 
  summarise(deaths = mean(deaths)) %>% 
  ungroup()

str(temp2)

plot_ly(data = temp2 , 
        x = ~month2 ,
        y = ~deaths ,
        type = 'scatter' ,
        mode = 'line')

