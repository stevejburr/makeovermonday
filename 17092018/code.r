library(tidyverse)
library(readxl)

data <- read_xlsx("Ticket prices for planes and trains on different routes.xlsx")


ggplot(data) +
  geom_line(aes(x=WeeksAhead,y=`Ticket price`,group=Mode,color=Mode)) +
  facet_wrap(~Route)
colnames(data)


ggplot(data) +
  geom_point(aes(x=`Raw travel time`,
                 y=`Ticket price`,
                 color=Route,
                 alpha=WeeksAhead))



#look at differences between planes vs trains on x, plus also time and or eco

data %>% group_by(Route,WeeksAhead) %>%
  arrange(Route,WeeksAhead) %>% 
  select(Route,WeeksAhead,Mode,`Ticket price`) %>%
  spread(key=Mode,value=`Ticket price`) %>%
  transmute("Price Gap" = Plane-Train) -> priceGap

data %>% group_by(Route,WeeksAhead) %>%
  arrange(Route,WeeksAhead) %>% 
  select(Route,WeeksAhead,Mode,`EcoPassengerCO2`) %>%
  spread(key=Mode,value=`EcoPassengerCO2`) %>%
  transmute("Eco Gap" = Plane-Train) ->  ecoGap

ecoGap %>% group_by(Route) %>% summarise(`Eco Gap` = max(`Eco Gap`)) %>% arrange(-`Eco Gap`) %>% pull(Route) -> levels

defaultColours <- c("#F564E3","#00BFC4","#F8766D","#00BA38","#B79F00","#619CFF")

priceGap %>% left_join(ecoGap) %>% ungroup() %>%
  mutate(Route=factor(Route,levels=levels)) %>%
  ggplot()+
  geom_point(aes(x=`Price Gap`,y=`Eco Gap`,alpha=WeeksAhead, colour=Route),size=4) +
  geom_vline(aes(xintercept=0),colour="grey70") +
  scale_alpha_continuous("",range=c(0.2,1),trans="reverse",guide=FALSE) +
  scale_x_continuous("Price Gap / €s")+
  scale_y_continuous("Eco Gap / kgs of CO2",breaks=c(100,150,200,250)) +
  annotate("text",x=-100,y=320,label="<- Train is more expensive",colour="grey50",size=3) +
  annotate("text",x=100,y=320,label="Train is less expensive ->",colour="grey50",size=3) +
  annotate("text",x=95,y=160,label="If you are booking to go between Munich and Budapest last minute,\nget the train!",hjust=0,size=3,colour=defaultColours[4])+
  annotate("text",x=-10,y=120,label="The train gets is an expensive between\nLondon and Amsterdam last minute choice",hjust=1,size=3,,colour=defaultColours[2])+
  annotate("text",x=155,y=101,label="Relatively it's not that bad to travel\nfrom Berlin to Warsaw by plane",hjust=0,size=3,,colour=defaultColours[1])+
  annotate("text",x=-175,y=290,label="It's much better for the planet to\ntravel from London to Marseille by train.",hjust=0,size=3,colour=defaultColours[3])+
  theme_minimal() +
  theme(text=element_text(colour="grey50"),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey50"),
        axis.ticks = element_blank()) + 
  labs(title="It's always better for the environment to get the train - and cheaper on some routes.\nThough costs change as the journey approaches, it never changes which mode is cheapest.",
       subtitle="Eco gap is based on calculations by 'EcoPassenger' and account for factors such as Average Load and electricity mix.\nFor each route, we have six values for each week prior to the journey, the darkest dot represents the cost 1 week before,\nand the lightest 6 weeks before.",
       caption="Data from DW Data - Design by @stevejburr")

ggsave("plot.png",width=10,height=8,dpi="retina")

#put routes onto the plot or order factor at min
#label lhs/rhs
#nb darker = more recent

