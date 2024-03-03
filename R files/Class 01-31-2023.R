# Libraries
library(tidyverse)
library(plotly)

# Data
temp0 <- mtcars

# Histogram using ggplot2
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

ggplot(temp0, aes(x=mpg)) + 
  geom_histogram(binwidth=3)

ggplot(temp0, aes(x=hp)) + 
  geom_histogram(binwidth=35)

# Histogram using plotly

plot_ly(x = ~temp0$mpg, type = "histogram" ,
        nbinsx = 5)

plot_ly(x = ~temp0$hp, type = "histogram" ,
        nbinsx = 15) %>%
  layout(title = 'Histogram for HP', plot_bgcolor = "#e5ecf6", 
         xaxis = list(title = 'HP'), 
         yaxis = list(title = 'Count'))

# Bar plot using ggplot2

str(temp0$cyl)

temp1 <- temp0 %>% 
  group_by(cyl) %>% 
  summarise(mpg = mean(mpg))

str(temp1)

ggplot(data = temp1) +
  geom_col(aes(x = as.factor(cyl) , 
               y = mpg ,
               fill = cyl) ,
           show.legend = FALSE) +
  ggtitle('MPG by number of cylinders') + 
  xlab('Cylinders') +
  ylab('Average MPG')

# Bar plot using plotly

plot_ly(
  data = temp1 ,
  x = ~as.factor(cyl),
  y = ~mpg,
  color = ~as.factor(cyl) ,
  colors = c('red' , 'green' , 'grey') ,
  type = "bar"
) %>% hide_legend()



