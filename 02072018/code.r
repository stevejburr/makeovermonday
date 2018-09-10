setwd("C:/Data/Personal/Make Over Monday/02072018")

library(tidyverse)
library(lubridate)
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(scales)
library(gridExtra)
library(grid)
data <- readr::read_csv("rat_sightings.csv")


colnames(data)
View(data)


# get unique observations by borough -> created_date 

data %>% mutate(date=as.Date(created_date)) %>%
  group_by(borough,date) %>% summarise(count=n()) -> timeseries

head(timeseries)

timeseries %>% mutate(year=year(date),month=month(date)) %>%
group_by(borough,year, month) %>% summarise(count=sum(count)) -> timeseries
# thanks to https://stackoverflow.com/questions/21887088/generate-a-filled-geom-step
timeseries_areaStep <- bind_rows(old = timeseries, new= timeseries %>%
                                   mutate(count=lag(count)), .id="source") %>%
  arrange(borough, year, month, source) %>% 
  mutate(date=as.Date(paste0(year,"-",month,"-",1))) %>% 
  split(.$borough) %>% 
  map(function(df) ungroup(df) %>%
mutate(count=if_else(is.na(count),lag(count),count))) %>% 
  bind_rows() %>% 
  filter(borough != "Unspecified") %>% arrange(borough, date, source) -> timeseries_areaStep
  
  
  
skyscraper_plot <-ggplot(timeseries_areaStep) +
  geom_ribbon(aes(x=date,ymin=0,ymax=count,fill=borough)) +
  #scale_fill_discrete(name="Borough")+
  scale_fill_brewer(name="Borough",type="seq",palette=6)+

  theme_minimal()+
  scale_x_date(expand=c(0,0))+
  scale_y_continuous(name="Rat Sightings per Month",expand=c(0,0))+
  theme(axis.title.x=element_blank(),
        
        plot.background = element_rect(fill=NA, colour=NA),
        panel.grid = element_blank(),
        panel.background=element_rect(fill="skyblue1",colour=NA),
  )



nyc_map <- get_map("New York, NY", zoom=10, maptype = "toner-background")
plot(nyc_map)

BRONX <- geocode("Bronx, NY")
BROOKLYN <- geocode("BROOKLYN, NY")
MANHATTAN <- geocode("Manhattan, NY")
QUEENS <- geocode("Queens, NY")
`STATEN ISLAND` <- geocode("Staten Island, NY")

locations <- rbind(BRONX,BROOKLYN,MANHATTAN,QUEENS,`STATEN ISLAND`)
locations <- cbind(c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND"),locations)
colnames(locations) <- c("borough","lon","lat")


total_obs <- data %>% group_by(borough) %>% summarise(count=n()) %>% filter(borough != "Unspecified") %>% left_join(locations)

map_plot<- ggmap(nyc_map) + geom_point(data=total_obs, aes(x=lon,y=lat,size=count),colour="skyblue1") +
  geom_label(nudge_y=-0.035, data=total_obs, aes(x=lon, y=lat, label=borough, fill=borough))+
  scale_fill_brewer(name="Borough",type="seq",palette=6, guide=FALSE)+
  scale_size_continuous(name="Total Rat Sightings \n2010-2018",range=c(3,20),labels=comma) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        legend.key = element_blank())


grid.arrange(skyscraper_plot,map_plot,
             
             top = "Rat Sightings in New York City #makeovermonday",
             bottom = textGrob(
               "By @stevejburr - Data Source=NYC Open Data",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1
             )
)

ggsave("test.png",dpi="retina")
