library(tidyverse)
library(lubridate)

data <- read_csv("avocado.csv")


#do weighted average of prices across whole data cut by convetional vs not

data %>% group_by(Year,Date,Type) %>% 
  summarise(Weight=sum(`Total Volume`),WeightedPrice=sum(AveragePrice*`Total Volume`)) %>%
  mutate(`Average Price`=WeightedPrice/Weight) %>%
  ungroup() %>%
  mutate(Date=dmy(Date)) %>% 
  group_by(Year,Type) %>%
  mutate(`Annual Average`=mean(`Average Price`),
         IndexVsAverage = `Average Price`/`Annual Average`,
         MaxAnnualRise = max(IndexVsAverage)-1,
         ShowMax = if_else(max(IndexVsAverage)==IndexVsAverage & Year != 2018,paste0("+",scales::percent(MaxAnnualRise)),"")) %>%
  ggplot()+
  geom_line(aes(x=Date,y=`Annual Average`,colour=Type),alpha=0.5,linetype=2)+
  geom_line(aes(x=Date,y=`Average Price`,colour=Type),size=1) +
  geom_text(aes(x=Date,y=`Average Price`,colour=Type,label=ShowMax),nudge_y = 0.03,show.legend = FALSE)+
  scale_y_continuous("Average Price per Fruit",labels=scales::dollar) +
  scale_x_date("") +
  scale_colour_manual("Type of Avocado",values=c("#2D5B00","#8C4314"),labels=c("Conventional","Organic"))+
  theme_minimal()+
  theme(text=element_text(colour = "grey50"),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x=element_blank()) +
  labs(title="Be careful when you buy Avocados!",
       subtitle="At peak prices, you can more than 1/3 more than the yearly average, the start of the year tends to be the best time to buy.",
       caption="#MakeoverMonday - Design by @stevejburr - Data source= Hass Avocado Board")


ggsave("plot.png",dpi="retina",height=9,width=9)
#get yearly averages, and calculate the max % difference vs yearly average price