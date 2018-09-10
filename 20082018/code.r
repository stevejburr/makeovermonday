setwd("C:\\Data\\Personal\\Make Over Monday\\20082018")

library(tidyverse)
library(lubridate)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(ggrepel)

data <- read.csv("ACLED.csv")

data %>% select(region,country,event_date,event_type,fatalities,year) %>%
  mutate(event_date=as.Date(event_date,format="%d-%b-%y")) %>%
  mutate(event_type=as.character(event_type)) %>% 
  mutate(event_type=if_else(grepl("Battle",event_type)==1,"Battle",event_type)) %>%
  mutate(event_type=as.factor(event_type),
         region = as.factor(region),
         country = as.factor(country))-> chart_data


#counts vs sum(fatalities)

#top countries 
data %>% filter(year==2018) %>% group_by(country) %>% summarise(fatalities=sum(fatalities)) %>% arrange(-fatalities) %>% mutate(obs=1:n()) %>% select(-fatalities)-> countries

chart_data %>% left_join(countries) %>% filter(year==2018 & obs <=10) %>% group_by(country, event_date) %>%
  summarise(count=n(),deaths=sum(fatalities)) -> chart_data

#need a cartesian product
cj_dates <- chart_data %>% ungroup() %>% select(event_date) %>% distinct()
cj_countries <-  chart_data %>% ungroup() %>% select(country) %>% distinct()
test <- crossing(cj_dates,cj_countries)
chart_data %>% right_join(test) %>% mutate(count=if_else(is.na(count),as.integer(0),count),deaths=if_else(is.na(deaths),as.integer(0),deaths)) %>% arrange(country, event_date) %>% 
  group_by(country) %>%
  mutate(cumEvents=cumsum(count),cumDeaths=cumsum(deaths))-> chart_data

chart_data %>% filter(country %in% c("Afghanistan","Yemen","Syria")) %>% ungroup() %>% summarise(deaths=sum(deaths))
  
chart_data %>% mutate(country_repel=if_else(n()==1:n(),as.character(country),""),
                      country_label=if_else(n()==1:n(),"",as.character(country))) %>%
            mutate(country_repel=if_else(country_repel=="Democratic Republic of Congo","DRC",country_repel),
                   country_label=if_else(country_label=="Democractic Republic of Congo","DRC",country_label)) %>%
            mutate(country_label=if_else(country_label %in% c("Afghanistan","Yemen","Syria"),country_label,""))-> chart_data


chart_data %>% 
  ggplot() + 
  geom_point(aes(x=cumEvents,y=cumDeaths, col=country, size=cumDeaths)) +
  geom_text(aes(x=cumEvents,y=cumDeaths,label=country_label,col=country),nudge_y = 1000,show.legend = FALSE)+
  geom_text_repel(aes(x=cumEvents,y=cumDeaths,label=country_repel,col=country),point.padding = 0.5,show.legend = FALSE) +
  #geom_line(aes(x=cumEvents,y=cumDeaths, col=country,group=country)) +
  scale_y_continuous("Deaths")+
  scale_x_continuous("Violent Events") +
  scale_size_continuous(guide=FALSE)+
  scale_color_manual("Country",values=c("#5d7abe",
    "#323f00",
    "#32215b",
    "#8d985a",
    "#38230e",
    "#4aa280",
    "#673b07",
    "#539cad",
    "#23541b",
    "#00483f")) +
  guides(color=guide_legend(override.aes=list(size=5)))+
  theme_minimal() +
  theme(text=element_text(colour="grey50"),
        legend.text=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        panel.grid = element_blank(),
        plot.title = element_text(face="bold",size=rel(1.25))) +
  #animation 
  labs(title="Over 59,000 people have died in violent conflict in just 3 countries so far in 2018.",
       subtitle = 'Data from 2018-01-01 to {frame_time}',caption="Design by @stevejburr - Data Source: ACLED") +
  transition_time(event_date) -> test
  #+
  #shadow_trail(max_frames=2,alpha=alpha*0.05) -> test

chart_data %>% ungroup() %>% select(event_date) %>% distinct %>% pull() %>% length() -> days

myGifRenderer <- gifski_renderer(loop=FALSE)
myAnimation <- animate(test,fps=10, nframes=days, renderer=myGifRenderer, device="png", height=500,width=520)
myAnimation
anim_save("animation.gif")

#connected scatter by country of number of events / sum of fatalties
# get most violent 10 countries in 2018
# do a plot per country 
# do jitter date + fatality