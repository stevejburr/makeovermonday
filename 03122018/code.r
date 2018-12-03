library(tidyverse)

data <- read_csv("Women Constructors in the Shortz Era.csv")

#try converting rounded %ages into a grid of waffle charts

perc <- 0.101

makeWaffleData <- function(perc) {
  number <- round(perc*100,0)
  out <- rep(0,100)
  for (i in 1:number){
    out[i] <- 1
  }
  dfOut <- data.frame()
  for (row_numb in 1:10){
    for (col_numb in 1:10){
      temp_dfOut <- data.frame()
      row <- row_numb
      col <- col_numb
      index <- ((row-1)*10)+col
      outVect <- c("row"=row,"col"=col,"value"=out[index])
      dfOut <- rbind(dfOut,outVect)
    }
  }
  colnames(dfOut) <- c("row","col","value")
  return(dfOut)
}
makeWaffleData(perc)


makeWaffleData(perc) %>%
  ggplot() +
  geom_tile(aes(x=row,y=col,fill=as.factor(value),height=1,width=1),
            color="black",
            show.legend = FALSE) +
  scale_fill_manual("",values=c("white","grey50")) +
  scale_x_continuous("") +
  scale_y_continuous("",limits=c(0,11))


makeWaffleData_tibble <- function(data){
  return(makeWaffleData(data$perc))
}


data %>% 
  mutate(perc=Women/(Men+Women)) %>%
  mutate(Weekday=factor(Weekday,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>%
  group_by(Year,Weekday) ->data

data %>%
  select(perc) %>%
  nest() -> data_nested

data_nested %>%
  mutate(chart_data=map(data,makeWaffleData_tibble)) %>%
  select(-data) %>%
  unnest() -> chart_data



chart_data %>% 
  ggplot() +
  facet_grid(Year~Weekday) +
  geom_tile(aes(x=row,y=col,fill=as.factor(value),height=1,width=1),
            color="black",
            show.legend = FALSE) +
  geom_label(data=data,aes(x=10,y=10.5,label=scales::percent_format(accuracy=1)(perc)),vjust="top",hjust="right")+
  scale_fill_manual("",values=c("white","grey50")) +
  scale_x_continuous("") +
  scale_y_continuous("",limits=c(0,11)) +
  theme_minimal() +
  theme(axis.text=element_blank(),
        text=element_text(colour="grey50"),
        panel.grid=element_blank(),
        strip.text=element_text(colour="grey50"))+
  labs(title="The New York Times Crossword is rarely written by Women",
       subtitle = "This graph shows the proportion of puzzles where at least one woman contributed to the design.",
       caption="#MakeOverMonday - Design by @stevejburr - Data Source: XWord Info")

ggsave("plot.png",height=12,width=12)
