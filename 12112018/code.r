library(tidyverse)

data <- read_tsv("Employee Diversity in Tech.csv")


head(data)

data %>% distinct(Company) %>% View()

#get most recent dates per company

data %>% distinct(Date) 

data %>% mutate(Date= factor(Date,levels=c("2014","Jul 2015","Aug 2016","Jan 2018","May 2018"))) %>%
  group_by(Company) %>%
  arrange(Company,Date) %>%
  filter(1:n()==n()) -> data


data %>% 
  mutate(`% Other` = if_else(`% Other`=="-","0",`% Other`),
         `% Other` = as.numeric(`% Other`)) -> data
  
ncols <- length(colnames(data))



data %>% ungroup() %>% select(colnames(data)[4:ncols]) %>% replace(.,is.na(.),0) -> temp

data[1:3] %>% bind_cols(temp) -> data


data %>% mutate(Type=factor(Type,levels=c("Country","Government","Entity","Tech","Social Media"))) -> data


data %>% 
  ungroup() %>%
  mutate(label=as.character(Company)) %>%
  arrange(Type,-`Female %`) %>%
  mutate(order=1:n(),
          label=fct_reorder(label,order)) -> data

#facet by entity vertically

data %>% 
  ggplot() +
  geom_col(aes(x=label,y=`Female %`,fill=Type)) +
  coord_flip()+
  theme_minimal()


data %>% 
  group_by(Type,label) %>% 
  select(-order) %>% 
  select_if(is.numeric) %>%
  gather(key="key",value="value",-c(Type,label)) -> trans_data

trans_data %>% 
  filter(label=="U.S. Population") %>%
  rename(Benchmark="value") %>%
  ungroup() %>%
  select(-c(label,Type)) -> bench_data

trans_data %>% left_join(bench_data) -> trans_data

trans_data %>% mutate(key=factor(key,
                                 levels=c("Female %","Male %","% White",
                                          "% Asian","% Black","% Latino",
                                          "% Multi","% Other","% Undeclared"),
                                 labels=c("% Female","Male %","% White",
                                                 "% Asian","% Black","% Latino",
                                                 "% Multi","% Other","% Undeclared"))) %>%
  filter(key %in% c("% Female","% White","% Asian","% Black","% Latino")) %>%
  filter(label != "Kaiser Permanente" & label != "DiversityInc top 50") %>%
  ungroup() %>%
  mutate(label=fct_rev(label),
         bar_end=if_else(value>Benchmark,value,Benchmark)) %>%
  ggplot() +
  facet_grid(~key) +
  geom_col(aes(x=label,y=Benchmark),colour="grey50",fill="white",alpha=0,width=0.75,show.legend = F) +
  geom_col(aes(x=label,y=value,fill=Type),width=0.5,show.legend = F) +
  geom_text(aes(x=label,y=bar_end,label=scales::number(value,accuracy=1,suffix="%")),colour="grey50",nudge_y=1,hjust="left",size=3,show.legend = F)+
  scale_fill_brewer(palette="Pastel1")+
  scale_y_continuous(limits=c(0,100))+
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text= element_text(colour="grey50"),
        strip.text=element_text(colour="grey50"),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_text(colour="grey50"),
        axis.text.y=element_text(colour="grey50")) +
  labs(y="% of employees",
       caption="#MakeoverMonday - Design by @stevejburr - Data Source: David McCandless",
       title="Tech companies are not representative of the US population in gender or ethnicity terms",
       subtitle="However, most do better than the low bar set by Congress, Venture Capilalists and CEOs")

ggsave("plot.png",height=10,width=10)
