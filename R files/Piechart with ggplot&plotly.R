# Load ggplot2
library(ggplot2)
library(dplyr)
library(plotly)

#Pie Chart with ggplot 2
library(readr)
HR_comma_sep_2 <- read_csv("C:/Users/Nahid/Desktop/R/HR_comma_sep-2.csv")

hr1 <- HR_comma_sep_2


table (hr1$sales)
table(hr1$salary)

data <- data.frame(
  group = c("accounting", "hr", "IT", "management", "marketing", "product_mng", "RandD", "sales", "support", "technical"),
  value = c(767, 739, 1227, 630, 858, 902, 787, 4140, 2229, 2720)
)


# Basic piechart

ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# By sales groups
hr1_pie_data <- hr1 %>% 
  group_by(sales) %>% 
  summarise(count = n())

hr1_pie_data <- hr1 %>% 
  group_by(sales) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(sales)) %>%
  mutate(prop = n / sum(n) *100) %>% 
  mutate(ypos = cumsum(prop)- 0.5*prop ,
         label = paste0(sales , ' ', '\n' ,round(prop , digits = 1) , '%') ,
         label_plotly = paste0('Departments: ' , sales))

ggplot(hr1_pie_data, aes(x="", y=prop, fill=as.factor(sales))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() + 
  theme(legend.position="right") + 
  geom_text(aes(y = ypos, label = as.factor(label)), 
            color = "white", 
            size=3) + 
  scale_fill_brewer(palette="Paired") + labs(fill = "Departments")

#With Plotly


plot_ly(data = hr1_pie_data ,
        labels = ~label_plotly, 
        values = ~n,
        type = "pie")


