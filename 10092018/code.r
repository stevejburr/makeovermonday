library(tidyverse)
library(readxl)
library(lubridate)

data <- read_xlsx("Paying the President.xlsx")

#View(data)

as.Date(data[1,]$date,format="%b %d, %Y")

unique(data$type)

#replicate the same three core groupings as the original plot


data %>% mutate("Type" = if_else(source=="Donald J. Trump for President, Inc.","Donald J. Trump \nfor President, Inc.",
                                 if_else(type=="FEC","Other Campaigns","Taxpayer Dollars"))) %>%
  mutate(id=1:n()) %>%
  mutate(date=as.Date(date,format="%b %d, %Y")) %>%
  mutate(purpose_scrubbed=as.factor(purpose_scrubbed)) %>%
  group_by(Type, purpose_scrubbed,date) %>%
  arrange(Type,purpose_scrubbed, date) %>% 
  summarise(spend=sum(amount)) -> summary_data
  
min <- min(summary_data$date,na.rm=TRUE)
max <- max(summary_data$date,na.rm=TRUE)

dates <- seq(min,max,by="1 day") %>% as.data.frame()
colnames(dates) <- c("date")

crossing(dates$date,summary_data$Type,summary_data$purpose_scrubbed) %>% 
  set_names(c("date","Type","purpose_scrubbed")) %>%
  left_join(summary_data) %>%
  mutate(spend=if_else(is.na(spend),0,spend)) %>%
  group_by(Type,purpose_scrubbed) %>%
  arrange(Type,purpose_scrubbed,date) %>%
  mutate(cum_spend=cumsum(spend)) %>% 
  ungroup() %>%
  ggplot() + 
  facet_grid(Type ~ .) +
  geom_line(aes(y=cum_spend,x=date,col=purpose_scrubbed,group=purpose_scrubbed))

#really it's all about his campaign

summary_data %>% group_by(Type) %>% summarise(total_spend=sum(spend)) -> topline_data

ggplot(summary_data) + 
  geom_col(aes(x=Type,y=spend,fill=purpose_scrubbed,group=purpose_scrubbed)) +
  geom_text(data=topline_data,aes(x=Type,y=total_spend,label=scales::dollar(total_spend)),hjust=-0.5,col="grey50",size=3) +
  annotate("text", x="Taxpayer Dollars",y=386476,label="Likely under-reported due to incomplete data",hjust=-0.5,col="grey50",size=3)+
  coord_flip()+
  theme_minimal()+
  scale_x_discrete("")+
  scale_y_continuous("",labels=scales::dollar,limits=c(0,17500000),breaks=c(0,5000000,10000000,15000000)) +
  scale_fill_brewer("",type="qual",palette="Set3") +
  theme(text=element_text(colour = "grey50"),
        panel.grid = element_blank(),
        axis.ticks.x=element_line(colour="grey50"),
        plot.title=element_text(size=12),
        plot.subtitle=element_text(size=10)) +
  labs(title="Since 2015 over $16m has been spent by campaign staff and government officials\nat locations owned by Donald Trump",
       subtitle="The vast majority of this came directly from his campaign fund, though the true extent that has come from \ntaxpayers is currently unclear",
       caption="Data from ProPublica - Design by @stevejburr")

ggsave("plot.png",width=8,height=8,dpi="retina")


