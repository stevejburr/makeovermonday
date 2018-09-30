library(tidyverse)
library(readxl)
library(grid)
library(gridExtra)

data <- read_xlsx("EM2030 Advocates Survey extract - Makeover Monday.xlsx")

data %>% filter(Qtype=="Likert") %>% select(RespID,Q0_Location,`Question Grouping`,Wording,`Question ID`,Value,Labels) ->likert_data

data %>% filter(Qtype!="Likert") %>% select(RespID,Q0_Location,`Question Grouping`,Wording,`Question ID`,Value,Labels) -> other_data


data %>% distinct(RespID) %>% pull() %>% length() -> respondents


colours <- c("#55BAB2","#07509B","#700370","#FF7477")
textCol <- "grey50"

other_data %>% filter(Value==1) %>% 
  group_by(Labels) %>%
  summarise(Total=sum(Value)) %>%
  mutate(Percentage=Total/respondents) %>%
  arrange(-Percentage) %>%
  top_n(5,Percentage) %>%
  ggplot() +
  geom_col(aes(y=Percentage,x=reorder(Labels,Percentage)),fill=colours[1]) +
  scale_y_continuous("% Respondents ranking issue in their top 3",labels=scales::percent) +
  scale_x_discrete("")+
  theme_minimal() +
  theme(text=element_text(colour=textCol),
        axis.title=element_text(colour=textCol),
        axis.text=element_text(colour=textCol),
        panel.grid=element_blank())+
  coord_flip() + 
  labs(title="Internationally, reducing gender-based violence is the key priority of advocates",
       caption="n=613 responses") -> plot1

other_data %>% group_by(Q0_Location) %>% summarise(Count=n_distinct(RespID)) -> responsesByRegion

#from https://raw.githubusercontent.com/dgrtwo/drlib/master/R/reorder_within.R
source("reorder_within.R")

other_data %>% filter(Value==1) %>% 
  group_by(Q0_Location,Labels) %>%
  summarise(Total=sum(Value)) %>%
  left_join(responsesByRegion) %>%
  mutate(Percentage=Total/Count) %>%
  arrange(Q0_Location,Percentage) %>%
  top_n(5,Percentage) %>%
  filter(Q0_Location != "No answer (International)" & Q0_Location!="Other") %>%
  mutate(order=1:n()) %>%
  ungroup() %>%
  mutate(fudge=1:n(),fudge=factor(fudge,levels=fudge,labels=Labels))-> facet_ranks
  
ggplot(facet_ranks) +
  geom_col(aes(y=Percentage,x=reorder_within(Labels,Percentage,Q0_Location)),fill=colours[1]) +
  scale_y_continuous("% Respondents Ranking issue in their top 3",labels=scales::percent) +
  scale_x_reordered("")+
  facet_wrap(~Q0_Location,scale="free_y") +
  geom_text(data=(facet_ranks %>% filter(order==1)),aes(x=0.75,y=0.75,label=paste0("n=",Count)),size=3,colour=textCol)+
  theme_minimal() +
  theme(text=element_text(colour=textCol),
        axis.title=element_text(colour=textCol),
        axis.text=element_text(colour=textCol),
        strip.text=element_text(colour=textCol),
        panel.grid=element_blank())+
  coord_flip() +
  labs(title="Gender violence is the biggest issue everywhere, but the second biggest issue varies by region") -> plot2



likert_data %>% filter(Labels!="No answer") %>%
  group_by(Wording,Labels) %>% summarise(count=n()) %>%
  group_by(Wording) %>% mutate(responses=sum(count),perc=count/responses) -> likert_data

likert_data$Labels <- factor(likert_data$Labels,levels=c("Not at all relevant","Not very relevant","Don't know","Fairly relevant","Very relevant"))

likert_data %>% filter(Labels=="Very relevant") %>%
  arrange(-perc) %>%
  ungroup() %>% 
  mutate(ranking=1:n()) %>% 
  select(Wording,ranking) -> questionOrder

likert_data %>% left_join(questionOrder) %>% arrange(ranking) %>%
ggplot() +
  geom_col(aes(x=reorder(Wording,-ranking),y=perc,fill=Labels),position=position_stack(reverse=TRUE)) +
  scale_fill_manual("Answer",values=c("#07509B","#55BAB2","grey95","#FF7477","#700370")) +
  scale_y_continuous("% Respondents",labels = scales::percent,sec.axis = sec_axis(~rev(.),labels=scales::percent,name=derive())) +
  scale_x_discrete("")+
  theme_minimal() +
  theme(text=element_text(colour=textCol),
        axis.title=element_text(colour=textCol),
        axis.text=element_text(colour=textCol),
        strip.text=element_text(colour=textCol),
        axis.text.x.top = element_text(colour="#700370"),
        axis.title.x.top=element_text(colour="#700370"),
        axis.text.x = element_text(colour="#07509B"),
        axis.title.x=element_text(colour="#07509B"),
        panel.grid=element_blank())+
  coord_flip() +
    labs(title="Gender advocates were also asked which factors mostly explain gaps in government equality data.",
subtitle="Most advocates think that collecting data on women is not prioritised, and that funding for collection is not made available",
         caption="n=573 responses") -> plot3

#left align all titles
g1 <- ggplotGrob(plot1)
g1$layout
g1$layout$l[g1$layout$name == "title"] <- 2
g1$layout$l[g1$layout$name == "subtitle"] <- 2

g2 <- ggplotGrob(plot2)
g2$layout
g2$layout$l[g2$layout$name == "title"] <- 2
g2$layout$l[g2$layout$name == "subtitle"] <- 2

g3 <- ggplotGrob(plot3)
g3$layout
g3$layout$l[g3$layout$name == "title"] <- 2
g3$layout$l[g3$layout$name == "subtitle"] <- 2

top <- textGrob(
"The EM2030 Global Advocates Survey 2018 asked gender advocates from around the world their views on progress towards gender equality,
how they feel about current data sources and the issues they think should be prioritized in the push for better and more accessible data
to the UN sustainable development goals for girls and women.",
                 gp=gpar(fontsize=14, col="grey50"),hjust=0,x=0.01)

png('Plot.png', width=1200, height=1200, res=100,type="cairo-png")
grid.arrange(
  top=top,
  g1,g2,g3,ncol=1,
  bottom = textGrob(
    "By @stevejburr - Data Source= Equal Measures 2030",
    gp = gpar(fontface = 3, fontsize = 9, col="grey50"),
    hjust = 1,
    x = 1
  ),padding = unit(1.5, "line"))
dev.off()
