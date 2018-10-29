library(tidyverse)
library(patchwork)
#data <- read_csv("Data.csv")

#use the full dataset shared by @statsninja which breaks out Not applicable
data <- readxl::read_xlsx("YouGov washing hands.xlsx")

#nb - these don't sum to 100 because Never includes NA
data %>% group_by(Location) %>% summarise_at(c("Male","Female"),sum)

#first, calculate NA
data %>% group_by(Location) %>% 
  filter(Response=="Not applicable - I do not have a bowel movement in this place") %>%
  summarise_at(c("Male","Female"),sum) -> NAs

colnames(NAs) <- c("Location","MaleNA","FemaleNA")

#join on NAs and take away from Never

data %>% 
  left_join(NAs) %>% 
  mutate(Male=if_else(Response=="Never",Male-MaleNA,Male),
         Female=if_else(Response=="Never",Female-FemaleNA,Female)) -> data

#Show always and never by gender

data %>% filter(Response == "Always") -> always

data %>% filter(Response == "Never") -> never

always %>%
  mutate(Male=1-Male,Female=1-Female) %>% #show % who don't always wash their hands
  ggplot() +
  geom_segment(aes(x=Location,xend=Location,y=Male,yend=Female),colour="grey50",size=1.3)+
  geom_point(aes(x=Location,y=Male),colour="purple",size=3) +
  geom_point(aes(x=Location,y=Female),colour="orange",size=3) +
  geom_text(aes(x=Location,y=Female,label=scales::percent_format(accuracy=1)(Female)),colour="orange",nudge_y = -0.01)+
  geom_text(aes(x=Location,y=Male,label=scales::percent_format(accuracy=1)(Male)),colour="purple",nudge_y = +0.01)+
  annotate("text",x=1,y=0.3,label="% who don't always wash their hands",colour="grey50",hjust="left")+
  annotate("text",x=1.25,y=0.24,label="Men",colour="purple",hjust="left")+
  annotate("text",x=1.25,y=0.18,label="Women",colour="orange",hjust="left")+
  scale_y_continuous("",labels=NULL,limits=c(-0.05,0.3))+
  scale_x_discrete("")+
  theme_minimal() +
  theme(panel.grid=element_blank(),
        text=element_text(colour="grey50"),
        axis.title = element_text(colour="grey50"),
        axis.text = element_text(colour="grey50")) -> left_plot

never %>%
  ggplot() +
  geom_segment(aes(x=Location,xend=Location,y=Male,yend=Female),colour="grey50",size=1.3)+
  geom_point(aes(x=Location,y=Male),colour="purple",size=3) +
  geom_point(aes(x=Location,y=Female),colour="orange",size=3) +
  geom_text(aes(x=Location,y=Female,label=scales::percent_format(accuracy=1)(Female)),colour="orange",nudge_y = -0.01)+
  geom_text(aes(x=Location,y=Male,label=scales::percent_format(accuracy=1)(Male)),colour="purple",nudge_y = +0.01)+
  annotate("text",x=1,y=0.3,label="% who never wash their hands",colour="grey50",hjust="left")+
  scale_y_continuous("",labels=NULL,limits=c(-0.05,0.3))+
  scale_x_discrete("")+
  theme_minimal() +
  theme(panel.grid=element_blank(),
        text=element_text(colour="grey50"),
        axis.title = element_text(colour="grey50"),
        axis.text = element_text(colour="grey50")) -> right_plot

left_plot + right_plot +
  plot_annotation(title = "Now wash your hands please!",
                  subtitle="A YouGov survey of adults in Great Britain found that around 1 in 4 men and 1 in 5 women don't wash their hands after doing a poo",
                                       caption="#MakeoverMonday - Design by @stevejburr - Data source: YouGov",
                                       theme=theme(text=element_text(colour="grey50")))


ggsave("plot.png",height=8,width=10)
