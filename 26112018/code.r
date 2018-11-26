library(tidyverse)
library(readxl)

data <- read_xlsx("Cost of a night out around the world.xlsx")

data %>% filter(Category=="Party night") %>%
  mutate(label=paste0(City," - ", Country)) %>%
  select(label,Item, Cost) %>%
  group_by(label) %>%
  mutate(total=sum(Cost)) %>%
  ungroup() %>%
  mutate(label=as.factor(label),
         label=fct_reorder(label,total),
         Item = if_else(Item=="2 Longdrinks","Two long drinks",Item),
         Item=factor(Item,levels=c("Two long drinks","Club entry","Taxi","Big Mac")),
         total_label=if_else(Item=="Big Mac",paste0("Total: ",scales::dollar(total)),"")) -> chart_data

chart_data %>%
  ggplot(aes(x=label,y=Cost)) +
  geom_col(aes(fill=Item),show.legend = F) +
  geom_text(aes(label=total_label),hjust="left",colour="grey50",nudge_y=20)+
  scale_y_continuous("", labels=scales::dollar)+
  scale_x_discrete("")+
  scale_fill_manual("",values=c("#C33149","#0C0A3E","#7B1E7A","#F9564F")) +
  coord_flip() +
  facet_grid(~Item) +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        strip.text = element_text(colour="grey50"),
        text=element_text(colour="grey50"),
        axis.title = element_text(colour="grey50"),
        axis.text= element_text(colour="grey50")) +
  labs(title="Zurich is a very expensive city for a night out - Mexico city is much cheaper.",
       subtitle="Cities aren't always consistently cheap or expensive across all the parts of a night out.\nFor example clubs are expensive in Miami while Taxi's are cheaper.",
       caption="#MakeoverMonday - Design by @stevejburr - Data source: UBS/Statistica")

ggsave("plot.png",width=10,height=10)
