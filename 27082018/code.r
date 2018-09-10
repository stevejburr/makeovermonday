setwd("C:\\Data\\Personal\\Make Over Monday\\27082018")
library(tidyverse)

data <- read_csv("Wearables-DFE.csv")
View(data)
colnames(data)

#select the variables to chart, group up small categories into all others to reduce noise in chart
data %>% select(Body.Location,Category,Price) %>%
  mutate(Category=if_else(is.na(Category),"Other",Category)) %>%
  mutate(Category=if_else(Category %in% c("Fitness","Lifestyle","Entertainment","Medical"),Category,"Others")) %>%
  mutate(Body.Location=if_else(Body.Location %in% c("Wrist","Head","Waist"),Body.Location,"Others"))%>%
  group_by(Body.Location, Category) %>% summarise(count=n()) -> chartdata


#get ordering for a category factor
chartdata %>% ungroup() %>% group_by(Category) %>% summarise(total=sum(count)) %>% arrange(total) %>% pull(Category) -> Categories
chartdata$Category <- factor(chartdata$Category,levels=Categories)


View(chartdata)
chartdata %>% distinct(Body.Location)
chartdata %>% ungroup() %>% select(Category) %>% distinct(Category)

#get %ages of each body location
#this is the width of each bar
chartdata %>% ungroup() %>% summarise(sum(count)) %>% pull() -> totaldevices
chartdata %>% ungroup() %>% group_by(Body.Location) %>% 
  summarise(count=sum(count)) %>% 
  mutate(width=100*count/totaldevices) %>% 
  arrange(width) %>% mutate(xmax=cumsum(width),xmin=if_else(is.na(lag(xmax)),0,lag(xmax))) -> widths

#get %age of each Category within each bodylocation
chartdata %>% ungroup() %>% group_by(Body.Location) %>% summarise(total=sum(count)) -> location.totals
chartdata %>% ungroup() %>% group_by(Body.Location,Category) %>%
  summarise(sum=sum(count)) %>% left_join(location.totals) %>%
  mutate(perc=100*sum/total) %>% arrange(Body.Location,Category) %>%
  mutate(ymax=cumsum(perc),ymin=ymax-perc) %>%
  select(Body.Location, Category, ymax, ymin) -> location_perc

chartdata %>% group_by(Body.Location,Category) %>% summarise()

location_perc  %>% left_join(widths) %>% select(-count) %>% left_join(chartdata) %>% mutate(count=if_else(count>80,as.character(paste0(count,"\nproducts")),"")) -> final.chart.data

final.chart.data %>% ungroup() %>% group_by(Body.Location) %>% summarise(xmax=max(xmax),xmin=max(xmin)) %>% mutate(pos=(0.5*(xmax+xmin))) -> xlabs


colours <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')

ggplot(final.chart.data) + geom_rect(aes(ymin=ymin,ymax=ymax,xmax=xmax,xmin=xmin,fill=as.factor(Category)),col="grey50") +
  geom_text(aes(y=(0.5*(ymin+ymax)),x=(0.5*(xmax+xmin)),label=count),col="grey20") +
  geom_text(data=xlabs,aes(y=105,x=pos,label=Body.Location),col="grey50") +
  scale_fill_manual("Usage",values=colours)+
  scale_x_continuous("")+
  scale_y_continuous("")+
  theme_minimal() +
  theme(axis.text=element_blank(),
        panel.grid = element_blank(),
        text = element_text(colour="grey50")
        ) +
  labs(title="Wearables are dominated by fitness trackers, smart watches and cameras",
       subtitle="All other uses remain fragmented and niche",
       caption="Design by @stevejburr - Data from Figure Eight")

ggsave("plot1.png",dpi="retina",width=7,height=7)

#is a stacked bar just better?
chartdata %>% mutate(label=if_else(count>80,as.character(count),"")) %>%
ggplot() + geom_col(aes(y=count,x=factor(Body.Location,levels=c("Wrist","Head","Others","Waist")),fill=as.factor(Category))) +
  coord_flip() + scale_y_continuous("Number of Products") + scale_x_discrete("") + scale_fill_manual("Usage",values=colours)+
  geom_text(aes(label=label,y=count,x=factor(Body.Location,levels=c("Wrist","Head","Others","Waist"))),position = position_stack(vjust = 0.5),col="grey20")+
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(colour="grey50"))+
  labs(title="Wearables are dominated by fitness trackers, smart watches and cameras",
       subtitle="All other uses remain fragmented and niche",
       caption="Design by @stevejburr - Data from Figure Eight")

ggsave("plot2.png",dpi="retina",width=7,height=7)

#group up locations into wrist,head,waist,all others
#group up categories into Fitness,Lifestyle,Entertainment,Medical, all others