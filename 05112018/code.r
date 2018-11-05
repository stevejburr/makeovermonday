library(tidyverse)

data <- read_csv("US Population Estimates.csv")

colnames(data)

#transpose to long format

data %>% 
  group_by(Sex,Origin,Race,Year,`Total Population`) %>%
  gather(key="Age",value="Population",starts_with("Age")) -> data_long

#get numeric age
data_long$test <- as.numeric(gsub("Age ","",data_long$Age))

#group numeric age into < 18 and 65<= 

data_long %>% mutate(age_group=case_when(
  test < 18 ~ "Children (under 18)",
  test >=65 ~ "Older Adults (65+)",
  TRUE ~ "Other")) -> data_long


#delete other and then summarise and calculate proportions

#Sex has values "Both / Female / Male" 
#Origin has values "Hispanic" / "Not Hispanic", "Total"

#use these for subsetting, don't are about race

#total population is total population of USA so can't use this to get to proportions

data_long %>%
  group_by(Sex,Origin,Year,age_group) %>%
  summarise(Population = sum(as.numeric(Population))) %>%
  group_by(Sex,Origin,Year) %>%
  mutate(Total = sum(Population),
         Prop = Population / Total) %>%
  ungroup() %>%
  mutate(Sex=if_else(Sex=="Both","Total",Sex)) %>% #recode variable for consistency
  filter(age_group != "Other") -> data_fin

data_fin$Sex <- as.factor(data_fin$Sex)
data_fin$Sex <- fct_rev(data_fin$Sex)
?fct_rev
#try and do a nice animation of combinations of a panek of Sex/Race showing proportions of Kids vs Adults

data_fin %>% filter(Year==2018) %>%
  ggplot() +
  facet_grid(Sex ~ Origin) +
  geom_col(aes(x=age_group,y=Population,fill=age_group)) +
  geom_text(aes(x=age_group,y=Population,
                label=scales::comma_format(accuracy=0.1,scale=0.000001,suffix="m")(Population)),
            nudge_y = 10000000,
            colour="grey50")+
  scale_y_continuous("",labels=NULL) +
  scale_fill_discrete("") +
  scale_x_discrete("",labels=NULL)+
  theme_minimal()+
  theme(text=element_text(colour="grey50"),
        axis.title = element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        panel.grid=element_blank(),
        strip.text=element_text(colour="grey50"))


library(gganimate)

data_fin %>%
  ggplot() +
  facet_grid(Sex ~ Origin) +
  geom_col(aes(x=age_group,y=Population,fill=age_group)) +
  geom_text(aes(x=age_group,y=Population,
                label=scales::comma_format(accuracy=0.1,scale=0.000001,suffix="m")(Population)),
            nudge_y = 10000000,
            colour="grey50")+
  scale_y_continuous("",labels=NULL) +
  scale_fill_discrete("") +
  scale_x_discrete("",labels=NULL)+
  theme_minimal()+
  theme(text=element_text(colour="grey50"),
        axis.title = element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        panel.grid=element_blank(),
        strip.text=element_text(colour="grey50"))+
  labs(title="Population estimates for the USA suggest that soon there will be more adults\naged over 65 than children under 18",
         subtitle = 'The exact cross over point varies by gender and ethnicity\nYear: {frame_time}',
       caption="#MakeoverMonday - Design by @stevejburr - Data Source: US Census Bureau") +
  transition_time(Year)

anim_save("output.gif")
