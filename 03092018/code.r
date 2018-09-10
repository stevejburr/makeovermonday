setwd("C:/Data/Personal/Make Over Monday/03092018")

library(tidyverse)
library(readxl)
library(ggbeeswarm)
library(scales)
library(gridExtra)
library(grid)

data <- read_xlsx('Nike Factory Locations.xlsx')


#show "total factories" and "total workers" by country
#put on a map in someway?

data %>% group_by(Country) %>% summarise(Factories=n(),Workers=sum(`Total Workers`,na.rm=TRUE)) -> country_summary

View(country_summary)

#one bubble per factory
#each one sized by average number of workers per factory 
#overall size will show impression of number of workers

country_summary %>% mutate(Size=Workers/Factories) -> country_summary

#need a row for each factory
expanded_data <- country_summary[rep(seq(nrow(country_summary)), country_summary$Factories),]

ggplot((expanded_data%>% filter(Country=="ARGENTINA")),aes(y=1,x=1)) +  geom_quasirandom(aes(size=Size),method = "tukeyDense",groupOnX = FALSE) 

#need the countries factor to be based on sorted factories

country_summary %>% arrange(Workers) %>% select(Country) %>% pull() -> Countries

country_summary$Country <- factor(country_summary$Country,levels=Countries)

#just do bars

ggplot(country_summary) +
  geom_segment(aes(x = Country, y = 0, xend = Country, yend = Factories), color = "skyblue",size=1.25) +
  geom_point(aes(x=Country,y=Factories),col="skyblue",size=3) +
  geom_text(aes(x=Country,y=Factories,label=comma(Factories)),nudge_y=-10,colour="grey50",size=3.25)  + 
  coord_flip() + 
  scale_y_reverse("Number of Factories") +
  scale_x_discrete("",position="top")+
  expand_limits(y=c(0,160)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(colour="grey50"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(colour="grey50"),
        axis.line.y = element_blank(),
        panel.grid= element_blank(),
        plot.margin=unit(c(5.5, 0, 5.5, 5.5), "points")) -> left


ggplot(country_summary) +
  geom_segment(aes(x = Country, y = 0, xend = Country, yend = Workers), color = "skyblue", size=1.25) +
  geom_point(aes(x=Country,y=Workers),col="skyblue",size=3) +
  geom_text(aes(x=Country,y=Workers,label=comma(Workers)),nudge_y=62000,colour="grey50",size=3.25)  + 
  theme_minimal() +
  coord_flip() + 
  scale_y_continuous("Workers",labels=comma) +
  scale_x_discrete("",labels=str_to_title) +
  expand_limits(y=c(0,500000)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(colour="grey50"),
        axis.text.x =  element_blank(),
        axis.text.y = element_text(hjust=0.5 ,colour="grey50"),
        axis.line.x = element_line(colour="grey50"),
        panel.grid= element_blank(),
        plot.margin=unit(c(5.5, 5.5, 5.5, 0), "points")) -> right

png('Plot.png', width=1000, height=1000, res=145,type="cairo-png")
grid.arrange(left,right,nrow=1,ncol=2,widths=c(0.50,0.58),
            
             top = textGrob("China has the most Nike factories, but there are more factory workers in Vietnam and Indonesia",
                            gp=gpar(col="grey50",fontsize=11)),
             bottom = textGrob(
               "By @stevejburr - Data Source=Nike",
               gp = gpar(fontface = 3, fontsize = 9, col="grey50"),
               hjust = 1,
               x = 1
             )
) 
dev.off()
