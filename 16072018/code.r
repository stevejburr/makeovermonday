setwd("C:\\Data\\Personal\\Make Over Monday\\16072018")
library(tidyverse)
library(ggbeeswarm)
library(scales)
library(grid)
library(gridExtra)
data <-readxl::read_xlsx("NBA_Salary_History.xlsx")

#manually code up conferences
team1<-c("Boston Celtics","Brooklyn Nets","New York Knicks","Philadelphia 76ers","Toronto Raptors")
region1<- rep("Atlantic",length(team1))
conference1<- rep("Eastern",length(team1))

team2<-c("Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Milwaukee Bucks")
region2<- rep("Central",length(team2))
conference2<- rep("Eastern",length(team2))

team3<-c("Atlanta Hawks","Charlotte Hornets","Charlotte Bobcats","Miami Heat","Orlando Magic","Washington Wizards")
region3<- rep("Southeast",length(team3))
conference3<- rep("Eastern",length(team3))

team4<-c("Denver Nuggets","Minnesota Timberwolves","Oklahoma City Thunder","Portland Trailblazers","Utah Jazz")
region4<- rep("Northwest",length(team4))
conference4<- rep("Western",length(team4))

team5<-c("Golden State Warriors","Los Angeles Clippers","Los Angeles Lakers","Phoenix Suns","Sacramento Kings")
region5<- rep("Pacific",length(team5))
conference5<- rep("Western",length(team5))

team6<-c("Dallas Mavericks","Houston Rockets","Memphis Grizzlies","New Orleans Pelicans","San Antonio Spurs")
region6<- rep("Pacific",length(team6))
conference6<- rep("Western",length(team6))

Team <- c(team1,team2,team3,team4,team5,team6)
Region <- c(region1,region2,region3,region4,region5,region6)
Conference <- c(conference1,conference2,conference3,conference4,conference5,conference6)

info <- as.tibble(cbind(Team,Region,Conference))

data %>% mutate(Season=as.factor(Season), Team=as.factor(Team),`Total Salary`=`Total Salary`/1000000,`Salary Cap`=`Salary Cap`/1000000) ->data
#extract cap by year
data %>% group_by(Season) %>% summarise(`Salary Cap`=max(`Salary Cap`)) -> cap

#get middle 50% of data and shade
data %>% group_by(Season) %>% summarise(`Top`=quantile(`Total Salary`,0.75),`Bottom`=quantile(`Total Salary`,0.25),`Median`=quantile(`Total Salary`,0.5)) -> quantiles

data %>% left_join(info) -> data

chart <- ggplot() + geom_line(data=cap,aes(x=Season,y=`Salary Cap`,group=1,col=as.factor(1)),size=1) +
  geom_ribbon(data=quantiles,aes(x=Season,ymin=`Bottom`,ymax=`Top`,group=1,fill=as.factor(1)),alpha=0.25) +
  geom_quasirandom(data=data,aes(x=Season,y=`Total Salary`),alpha=0.5,col="black",fill="orange",shape=21) +
  scale_y_continuous("Salaries / $m",labels=dollar)+
  scale_fill_manual("",values=c("orange"),labels="Middle 50% of Teams")+
  scale_colour_manual("",values=c("black"), labels="Salary Cap")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour="grey50",size=0.5),
        axis.ticks = element_line(colour="grey50",size=0.5),
        axis.text.x = element_text(angle=90,vjust=0.5,colour="grey50"),
        axis.title = element_text(colour="grey50"),
        plot.title = element_text(colour="grey50"),
        plot.subtitle = element_text(colour="grey50")) +
  ggtitle("NBA Salary Cap #makeovermonday",subtitle="During the 2000s the majority of teams exceeded the salary cap and the spread of spend was wide. \nFrom 2008 onwards, the spread narrowed and in recent years the cap has risen substantially, though the majority of teams cluster above this.")

chart

grid.arrange(chart,
             bottom = textGrob(
               "By @stevejburr - Data Source=@CelticsHub @whatsthecap",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1
             )
)


png('Plot.png', width=1071, height=587, res=100,type="cairo-png")
grid.arrange(chart,
             bottom = textGrob(
               "By @stevejburr - Data Source=@CelticsHub @whatsthecap",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1
             )
)
dev.off()

#try small multiples plot
#merge topline stats to data
data %>% left_join(quantiles) %>% left_join(info) -> data

data %>% mutate(Above=if_else(`Total Salary`>`Salary Cap`,"Above","Below")) -> data

seasons <- levels(data$Season)
seasons[2:27] <- ""
seasons

chart2 <- ggplot(data) + geom_line(aes(y=`Total Salary`,x=`Season`,group=1,col=as.factor(1))) +
  geom_line(aes(y=`Salary Cap`,x=`Season`,group=1,col=as.factor(2))) + 
  geom_line(aes(y=`Median`, x=`Season`,group=1,col=as.factor(3)),alpha=0.25,size=1.25) +
  scale_colour_manual("",labels=c("Salary","Cap","Median Team"),values=c("blue","red","grey50"),aesthetics="colour") +
  scale_x_discrete("Season",labels=seasons) +
  facet_wrap(~Team) + theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour="grey50",size=0.5),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour="grey50"),
        axis.text.x = element_text(angle=90,vjust=0.5,colour="grey50"),
        axis.title = element_text(colour="grey50"),
        plot.title = element_text(colour="grey50"),
        strip.text = element_text(colour="grey50"),
        plot.subtitle = element_text(colour="grey50")) + ggtitle("NBA Team Salaries vs the Cap")

png('Plot2.png', width=1071, height=587, res=100,type="cairo-png")
grid.arrange(chart2,
             bottom = textGrob(
               "By @stevejburr - Data Source=@CelticsHub @whatsthecap",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1
             )
)
dev.off()
