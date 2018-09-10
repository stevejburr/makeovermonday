setwd("C:\\Data\\Personal\\Make Over Monday\\23072018")

library(tidyverse)
library(grid)
library(gridExtra)
'%ni%' <- Negate('%in%')

data <- readxl::read_xlsx("OECD Parental Leave system.xlsx")

colnames(data) 

data %>% mutate(`Paid Net`=if_else(Country %in% c("Germany","Austria","France","Chile"),"Paid Net","Not Paid Net")) -> data

OECD <- data %>% filter(Country == "OECD average") %>% select(`Paid maternity leave in weeks`) %>% as.numeric()
EU <- data %>% filter(Country == "EU average") %>% select(`Paid maternity leave in weeks`) %>% as.numeric()
EURO <- data %>% filter(Country == "Eurozone ave.") %>% select(`Paid maternity leave in weeks`) %>% as.numeric()

#calcualte mean x to label the chart
meanX <- data %>% filter(Country %ni% c("OECD average","EU average","Eurozone ave.")) %>%
  summarise(average=mean(`Paid maternity leave avg payment rate (%)`)) %>% as.numeric()

#identify all the points above the EU average on the y variable and create filters 
data %>% mutate(label=if_else(`Paid maternity leave in weeks` > EU,Country,"")) -> data

chart <- ggplot(data %>% filter(Country %ni% c("OECD average","EU average","Eurozone ave."))) +
  geom_point(aes(y=`Paid maternity leave in weeks`,x=`Paid maternity leave avg payment rate (%)`,
                 #shape=`Paid Net`
                 ),size=4,colour="blue",alpha=0.5,shape=16) +
  geom_hline(yintercept = OECD,linetype="dashed",colour="grey50") +
  annotate("text", x=0,y=OECD+2,label="OECD Average", colour="grey50",hjust=0) +
  geom_hline(yintercept= EU, linetype="dashed",colour="grey20") +
  annotate("text", x=0,y=EU+2,label="EU Average", colour="grey20",hjust=0) +
  geom_vline(xintercept=meanX,linetype="dashed",colour="grey50") +
  annotate("text", x=meanX+1,y=0,colour="grey50",hjust=0,label="Average of sample") +
  geom_text(aes(y=`Paid maternity leave in weeks`,x=`Paid maternity leave avg payment rate (%)`,label=label),nudge_y=+2.7,colour="grey50")+
  annotate("text",x=1.5,y=0.5,label="United States",hjust=0,colour="red")+
  #scale_shape("Is maternity leave paid net?") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour="grey50",size=0.5),
        axis.ticks = element_line(colour="grey50",size=0.5),
        axis.text.x = element_text(colour="grey50"),
        axis.title = element_text(colour="grey50"),
        plot.title = element_text(colour="grey50"),
        plot.subtitle = element_text(colour="grey50"),
        legend.text= element_text(colour="grey50"),
        legend.title=element_text(colour="grey50")) +
  ggtitle("Most countries provided just less than 20 weeks of paid maternity leave",subtitle="Many countries pay up to 100% of salary during this period, though there's large variation in this figure.\nThe United States stands out for the lack of any required maternity leave.")

chart

png('Plot.png', width=1071, height=587, res=100,type="cairo-png")
grid.arrange(chart,
             bottom = textGrob(
               "By @stevejburr - Data Source=OECD",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1
             )
)
dev.off()



