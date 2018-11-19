library(tidyverse)
library(readxl)

#read in the raw data
data <- read_xlsx("Hours to Pay Mortgage.xlsx")


#check what's in the data
colnames(data)
data %>% distinct(City) %>% pull() %>% length() -> cities

data %>% 
  arrange(`Hours per Month to Afford a Home`) %>%
  mutate(pos=1:n(),
         fill=case_when(`Hours per Month to Afford a Home` <20 ~ "<20hrs",
                        `Hours per Month to Afford a Home` <40 ~ "20-40hrs",
                        `Hours per Month to Afford a Home` <60 ~ "40-60hrs",
                        `Hours per Month to Afford a Home` <80 ~ "60-80hrs",
                        `Hours per Month to Afford a Home` <100 ~ "80-100hrs",
                        TRUE ~ "100hrs+"),
         line=if_else(`Hours per Month to Afford a Home`<20,"grey50","white")) %>%
  mutate(fill=fct_reorder(fill,`Hours per Month to Afford a Home`)) %>%
  #filter(pos<=10 | pos >cities-10) %>%
  mutate(label=paste0(City,", ",State)) %>%
  mutate(label=fct_reorder(label,`Hours per Month to Afford a Home`)) %>%
  ggplot()+
  geom_segment(aes(y=0, yend=`Hours per Month to Afford a Home`,x=label, xend=label),colour="grey50")+
  geom_point(aes(y=`Hours per Month to Afford a Home`,x=label,fill=fill,colour=line),cex=2,shape=21) +
  annotate("text",y=78,x=88,label="California is a very expensive state to live in",hjust="left",size=2.7,colour="grey50")+
  coord_flip() +
  scale_y_continuous(breaks=c(0,20,40,60,80,100),limits=c(0,120),expand=c(0,0)) +
  scale_fill_brewer("",palette = "YlOrRd")+
  scale_colour_identity()+
  labs(x="",
       y="Working hours per month required to pay typical mortgage",
       title="There are four US cities where on average over 100hrs of work a month goes directly towards paying a typical mortgage",
       subtitle="Calculations based on 2,087 working hours a year and median household incomes for each city",
       caption="#MakeoverMonday - Design by @stevejburr - Data Source: howmuch.net") +
  theme_minimal() +
  theme(text=element_text(colour="grey50"),
        panel.grid=element_blank(),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50",size=8),
        plot.subtitle = element_text(size=8),
        plot.title=element_text(size=8,face="bold")) -> p


ggsave("plot.png",p,height=10.8,width=9.09)
