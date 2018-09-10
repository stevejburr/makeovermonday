setwd("C:\\Data\\Personal\\Make Over Monday\\30072018")

library(tidyverse)
library(grid)
library(gridExtra)
library(scales)
data <- read_csv("Big Mac Index.csv")

#list out dates
data %>% distinct(date) %>% View()

data %>% filter(name=="Norway" & (date==as.Date("2018-01-01") | date==as.Date("2017-07-01"))) %>% View()


#use dollar price for comparisons 

data %>% group_by(name) %>% select(name, date, dollar_price, USD_raw) -> chartdata 
  

ggplot(chartdata) + geom_line(aes(x=date,y=dollar_price,col=name,group=name))

ggplot(chartdata) + geom_line(aes(x=date,y=USD_raw,col=name,group=name)) + facet_wrap(~name)

# do a column chart of dollar_price / USD_raw by complete countries

# get complete data series

data %>% group_by(name) %>%
  mutate(count=n()) %>%
  ungroup() %>% 
  mutate(max_count=max(count)) %>% 
  filter(count==max_count) %>% 
  select(name,date, dollar_price,USD_raw) %>%
  group_by(name,date) %>%
  gather(key="Measure",value="value",dollar_price:USD_raw) -> chartdata2

ggplot(chartdata2) + geom_line(aes(x=date,y=value,group=1)) + facet_grid(name ~ Measure, scales = "free_y")


# try slope chart on latest year in USD

data %>% filter((date==as.Date("2018-01-01") | date==as.Date("2017-07-01"))) %>%
  filter(name %in% c("Switzerland","Norway","Sweden","United States",
                     "Canada","Brazil","Denmark","Euro area","Australia",
                     "New Zealand","Britain","Czech Republic","Japan","China",
                     "Poland","Turkey","South Africa","Russia")) %>%
  select(name, dollar_price, date) %>% 
  spread(date,-dollar_price) -> slopedata

ggplot(slopedata) + geom_segment(aes(x=0,xend=1,y=`2017-07-01`,yend=`2018-01-01`,col=`name`))  


#try redoing the original with $ labels within coloured circles positioned based on %ages

data$text_label <- dollar_format()(data$dollar_price)

data %>% filter((date==as.Date("2018-01-01") | date==as.Date("2017-07-01"))) %>%
  filter(name %in% c("Switzerland","Norway","Sweden","United States",
                     "Canada","Brazil","Denmark","Euro area","Australia",
                     "New Zealand","Britain","Czech Republic","Japan","China",
                     "Poland","Turkey","South Africa","Russia")) %>%
  arrange(USD_raw) %>% mutate(name=factor(name, levels=rev(c("Switzerland","Norway","Sweden","United States",
                                                         "Canada","Brazil","Denmark","Euro area","Australia",
                                                         "New Zealand","Britain","Czech Republic","Japan","China",
                                                         "Poland","Turkey","South Africa","Russia")))) -> chartdata3

ggplot(chartdata3) + 
  geom_vline(aes(xintercept=0), col="grey50",alpha=0.5)+
  geom_point(aes(x=USD_raw,y=name,col=as.factor(date)),shape="|",size=4)+
  geom_text(data=(chartdata3%>%filter(date==as.Date("2018-01-01") & name!="Turkey")),
            aes(x=USD_raw,y=name,col=as.factor(date),label=text_label),hjust=0,show.legend = FALSE) +
  geom_text(data=(chartdata3%>%filter(date==as.Date("2018-01-01") & name=="Turkey")),
            aes(x=USD_raw,y=name,col=as.factor(date),label=text_label),hjust=1,nudge_x=-0.01,show.legend = FALSE) +
  geom_text(data=(chartdata3%>%filter(date!=as.Date("2018-01-01") & name!="Turkey")),
            aes(x=USD_raw,y=name,col=as.factor(date),label=text_label),hjust=1,nudge_x =-0.01,show.legend = FALSE) +
  geom_text(data=(chartdata3%>%filter(date!=as.Date("2018-01-01") & name=="Turkey")),
            aes(x=USD_raw,y=name,col=as.factor(date),label=text_label),hjust=0,show.legend = FALSE) +
 
  scale_x_continuous("Local Currency under(-) / over(+) valuation vs $", labels=percent, expand=c(0.1,0.1)) +
  scale_y_discrete("")+
  scale_color_manual("",values=c("#E09F7D","#3D0814"),labels=c("Jun 2017","Jan 2018"))+
  guides(colour = guide_legend(override.aes = list(shape = "|")))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(colour="grey50"),
        axis.text = element_text(colour="grey50"),
        legend.text = element_text(colour="grey50"),
        title= element_text(colour="grey50")) +
  ggtitle("Big Mac Index Jun 2017/Jan 2018") -> chart1


data %>% filter(name %in% c("Switzerland","Norway","Sweden","United States",
                   "Canada","Brazil","Denmark","Euro area","Australia",
                   "New Zealand","Britain","Czech Republic","Japan","China",
                   "Poland","Turkey","South Africa","Russia")) %>%
  arrange(USD_raw) %>% mutate(name=factor(name, levels=(c("Switzerland","Norway","Sweden","United States",
                                                             "Canada","Brazil","Denmark","Euro area","Australia",
                                                             "New Zealand","Britain","Czech Republic","Japan","China",
                                                             "Poland","Turkey","South Africa","Russia")))) %>%
  ggplot() + geom_line(aes(x=date,y=dollar_price),col="grey50")+
  facet_grid(name ~ . , scales = "free") +
  scale_x_discrete("")+
  scale_y_continuous("")+
  theme_minimal() +
  theme(axis.text=element_blank(),
        panel.grid=element_blank(),
        strip.text.y = element_text(colour="grey50",angle=0),
        title=element_text(colour="grey50",size=8),
        plot.margin=margin(5.5,5.5,22.0,5.5,"pt")) +
  ggtitle("Price in USD (2000->2018)") -> chart2
  
  
data %>% filter(name %in% c("Switzerland","Norway","Sweden","United States",
                            "Canada","Brazil","Denmark","Euro area","Australia",
                            "New Zealand","Britain","Czech Republic","Japan","China",
                            "Poland","Turkey","South Africa","Russia")) %>%
  arrange(USD_raw) %>% mutate(name=factor(name, levels=(c("Switzerland","Norway","Sweden","United States",
                                                          "Canada","Brazil","Denmark","Euro area","Australia",
                                                          "New Zealand","Britain","Czech Republic","Japan","China",
                                                          "Poland","Turkey","South Africa","Russia")))) %>%
  ggplot() + geom_line(aes(x=date,y=USD_raw),col="grey50")+
  facet_grid(name ~ . , scales = "free") +
  scale_x_discrete("")+
  scale_y_continuous("")+
  theme_minimal() +
  theme(axis.text=element_blank(),
        panel.grid=element_blank(),
        strip.text.y = element_text(colour="grey50",angle=0),
        title=element_text(colour="grey50",size=8),
        plot.margin=margin(5.5,5.5,22.0,5.5,"pt"),) +
  ggtitle("Price index vs USD") -> chart3

top <- grid.text("Over the last 6 months, most countries have seen price rises in USD terms",
                 gp=gpar(fontsize=20, col="grey50"))

png('Plot.png', width=1100, height=587, res=100,type="cairo-png")
grid.arrange(chart1, chart2, chart3,ncol=3,nrow=1,widths=c(4,1,1),
             top=top,
             bottom = textGrob(
               "By @stevejburr - Data Source=The Economist",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1
             )
)
dev.off()
