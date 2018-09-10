setwd("C:\\Data\\Personal\\Make Over Monday\\13082018")

library(tidyverse)
library(grid)
library(gridExtra)
library(RColorBrewer)

data <- read_csv("Anthony Bourdain Travels.csv")

View(data)


#year #show #region #counts

data %>% group_by(Year,Show, Region) %>% summarise(count=n()) %>% ungroup() %>%
  mutate(Show=as.factor(Show),Region=as.factor(Region)) %>%
  ggplot() + geom_col(aes(x=Year,y=count,fill=Region))


# do a waffle by region

data %>% mutate(total=n()) %>% group_by(Region) %>% summarise(count=n(),total=round(100*count/max(total))) %>%
  mutate(total=if_else(Region=="North America",total+1,total)) %>% arrange(-total)-> waffle

labels <-as.character(waffle$Region)
labels

waffle %>% ungroup() %>% mutate(Region=as.character(Region)) -> waffle
waffle$Region <- factor(waffle$Region,levels=labels)
#we know it sums to 99 -> increase North America +1 as smallest % distortion of reality


#get a 10x10 grid with region by colour
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$region <- rep(waffle$Region,waffle$total)


#colour pallette function
getColours <- colorRampPalette(brewer.pal(8, "Set2")) 
numberValues <- length(as.character(waffle$Region))


ggplot(df) + geom_tile(aes(x=x,y=y,fill=region),color="white",size=1.5) +
  scale_y_continuous("",expand=c(0,0), trans="reverse") +
  scale_x_continuous("",expand=c(0,0)) +
  scale_fill_manual("Region",values=getColours(numberValues))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(colour="grey50"),
        text = element_text(colour="grey50")) +
  ggtitle("Travels off the beaten track?","Looking at the proportion of Anthony Bourdain's travels which took place in each region,\nwe see that around 1/3rd took place in North America.\n\nWhile a visit to Antarctica pushes the boundaries, it feels like Central America,\nthe Middle East and Oceania are under represented") +
  geom_text(data=data.frame("text"),x = 10,
            y = -11,
            hjust = 1,
            size = 4,label="By @stevejburr - Data Source= @christinezhang", col="grey50") +
  coord_cartesian(
                  clip = 'off' 
  ) +
  theme(plot.margin = unit(c(1,1,5,1), "lines"))


ggsave("plot.png",dpi="retina")
