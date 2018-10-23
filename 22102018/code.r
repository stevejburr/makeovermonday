library(tidyverse)
library(readxl)
library(ggrepel)
library(patchwork)

data <- read_xlsx("MLB Beer Prices.xlsx")

#bar chart for latest year side by side

data %>% filter(Year==2018) %>%
  select(Year, City, Team, Price, `Price per Ounce`,Size) %>%
  mutate(Team=as.factor(Team),
         Team=fct_reorder(Team,Price),
         Average=if_else(Team=="MLB Average","Average","Not Average")) -> latestYearChart

ggplot(latestYearChart,aes(x=Team,y=Price)) +
  geom_col(aes(fill=Average),width=0.75,show.legend = FALSE) +
  #geom_text(aes(label=paste0(Size," oz")),hjust="right",nudge_y=-0.5,colour="grey40")+
  scale_y_continuous("Price of a Beer",labels=scales::dollar,
                     limits=c(12,0),breaks=rev(c(0,2.5,5,7.5,10)),
                     trans="reverse") +
  scale_x_discrete("",position="top")+
  scale_fill_manual("",values=c("grey75","#99abc6"))+
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        #panel.grid.major.x=element_blank(),
        axis.title=element_text(colour="grey50"),
        text=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        axis.text.y=element_blank(),
        ) -> left_chart

#insert a small slope chart to the right with a reranking based on price per unit
latestYearChart %>% mutate(PriceRank=as.numeric(Team),PricePerOunceRank=row_number(`Price per Ounce`)) %>% 
  select(Team,PriceRank,PricePerOunceRank,Average,Size) -> Rankings
  
ggplot(Rankings) +
  geom_segment(aes(y=PriceRank,yend=PricePerOunceRank,x=1,xend=2,colour=Average),size=1.5,show.legend = FALSE) +
  geom_text(aes(y=PriceRank,x=0.75,label=paste0(Team," (",Size,"oz)")),colour="grey50",hjust="right",size=3.5)+
  geom_text(aes(y=PricePerOunceRank,x=2.25,label=paste0(Team," (",Size,"oz)")),colour="grey50",hjust="left",size=3.5)+
  scale_colour_manual("",values=c("grey75","#99abc6"))+
  scale_x_continuous("",
                     limits=c(-1,4.0),expand=c(0,0)
                     ) +
  scale_y_continuous("",expand=expand_scale(mult = 0, add = 0.5)
)+
  theme_minimal() +
  theme(panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        text=element_text(colour="grey50"),
        plot.margin=margin(t = 10, r = 0, b = 10, l = 0, unit = "pt")) -> center_chart


ggplot(latestYearChart,aes(x=fct_reorder(Team,`Price per Ounce`),y=`Price per Ounce`)) +
  geom_col(aes(fill=Average),width=0.75,show.legend = FALSE) +
  #geom_text(aes(label=paste0(Size," oz")),hjust="left",nudge_y=0.01,colour="grey40")+
  scale_y_continuous("Price per Ounce",labels=scales::dollar,breaks=c(0,0.2,0.4,0.6),limits=c(0,0.8)) +
  scale_x_discrete("")+
  scale_fill_manual("",values=c("grey75","#99abc6"))+
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        #panel.grid.major.x=element_blank(),
        axis.title=element_text(colour="grey50"),
        text=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        axis.text.y=element_blank()) -> right_chart

#combine the charts using the patchwork package
left_chart + 
center_chart + 
right_chart + 
plot_layout(widths=c(0.25,0.5,0.25))+
plot_annotation(title = "Major League Baseball - it's how much for a beer?!?!",
                subtitle="Over $10 for a beer when seeing the New York Mets may seem steep, but it's better value than the small beers when seeing the Red Socks or Giants",
                caption="#MakeoverMonday - Design by @stevejburr - Data source: Team Marketing Report",
                theme=theme(text=element_text(colour="grey50")))

ggsave("plot.png",width=12,height=10)
