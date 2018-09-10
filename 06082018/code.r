setwd("C:\\Data\\Personal\\Make Over Monday\\06082018")

library(tidyverse)
library(readxl)
library(grid)
library(gridExtra)
library(scales)

data <- read_xlsx("Gross domestic expenditure on R&D.xlsx")

head(data)

#lets compare first and last years as dots 
#manually extracted list of coutnries (vs regional summary figures in original datsaet)
countries <- c("Albania","Algeria","American Samoa","Angola","Argentina","Armenia","Australia","Austria","Azerbaijan","Bahrain","Bangladesh","Belarus","Belgium","Benin","Bermuda","Bolivia (Plurinational State of)","Bosnia and Herzegovina","Botswana","Brazil","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cabo Verde","Central African Republic","Chad","Chile","China","China, Hong Kong Special Administrative Region","China, Macao Special Administrative Region","Colombia","Congo","Costa Rica","Côte d'Ivoire","Croatia","Cuba","Cyprus","Czechia","Democratic Republic of the Congo","Denmark","Ecuador","Egypt","El Salvador","Estonia","Eswatini","Ethiopia","Faeroe Islands","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Greenland","Guam","Guatemala","Guinea","Honduras","Hungary","Iceland","India","Indonesia","Iran (Islamic Republic of)","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kuwait","Kyrgyzstan","Lao People's Democratic Republic","Latvia","Lesotho","Libya","Lithuania","Luxembourg","Madagascar","Malawi","Malaysia","Mali","Malta","Mauritania","Mauritius","Mexico","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Palestine","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Puerto Rico","Qatar","Republic of Korea","Republic of Moldova","Romania","Russian Federation","Rwanda","Saint Helena","Saint Lucia","Saint Vincent and the Grenadines","Saudi Arabia","Senegal","Serbia","Seychelles","Singapore","Slovakia","Slovenia","South Africa","Spain","Sri Lanka","Sudan","Sudan (pre-secession)","Sweden","Switzerland","Tajikistan","Thailand","The former Yugoslav Republic of Macedonia","Togo","Trinidad and Tobago","Tunisia","Turkey","Uganda","Ukraine","United Arab Emirates","United Kingdom of Great Britain and Northern Ireland","United Republic of Tanzania","United States of America","United States Virgin Islands","Uruguay","Uzbekistan","Venezuela (Bolivarian Republic of)","Viet Nam","Zambia","Zimbabwe")

data %>% group_by(Country) %>% filter(Country %in% countries) %>%
filter(Year %in% c(2011,2015)) %>% filter(!is.na(`GERD in 000 current PPP$`)) %>% 
  mutate(Count=n()) %>% filter(Count==2) %>% select(-Count) %>%
mutate(Spend=as.numeric(sub(",",".",`GERD in 000 current PPP$`)))-> filtered_data


#order by 2016 and extract the years as a reversed vector and set as elvels of factor

filtered_data %>% filter(Year==2015) %>% arrange(Spend) %>% ungroup() %>% pull(Country) -> levelsTouse

filtered_data %>% ungroup() %>% mutate(Country=factor(Country,levels=levelsTouse)) -> filtered_data

levels(filtered_data$Country)
levels(filtered_data$Country)[73] <- "UK"
levels(filtered_data$Country)[50] <- "UAE"
levels(filtered_data$Country)[45] <- "Hong Kong"
levels(filtered_data$Country)[16] <- "FYR Macedonia"
levels(filtered_data$Country)[33] <- "Venezuela"
levels(filtered_data$Country)[13] <- "Macao"
levels(filtered_data$Country)[79] <- "USA"
levels(filtered_data$Country)[70] <- "Russia"
#get to spend by year
data %>% filter(Country=="World" & !is.na(`GERD in 000 current PPP$`)) %>% mutate(TotalSpend=as.numeric(`GERD in 000 current PPP$`)) %>%
  select(Year,TotalSpend) -> spend_by_year

filtered_data %>% left_join(spend_by_year) %>% mutate(`% of Global Spend`=Spend/TotalSpend) -> filtered_data

#two horizontal dot plots by year

# add on vertical bars by year to show 50% of global spend + 80% of global spend (colour)
# label value of USA + China in both years (colour these directly)

#find cut offs for %ages by year


filtered_data %>% arrange(Spend) %>% mutate(Year=factor(Year,levels=c(2015,2011))) %>%
  mutate(colour=if_else(Country=="USA","USA", if_else(Country=="China","China","All other countries")), colour=as.factor(colour)) %>%
  ggplot() + geom_point(aes(x=Country,y=`% of Global Spend`,col=colour)) + facet_grid( Year ~ .) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))

#label as %ages



#So many tiny flags - filter to top 10 in 2015 + all others

filtered_data %>% group_by(Year) %>% arrange(Year,-Spend) %>% top_n(10) %>% pull(Country) -> top10

finFactor <- rev(c(rev(levels(top10))[1:10],"All Others"))

filtered_data %>% filter(Country %in% top10) %>% select(Year, Country, `% of Global Spend`) -> top10_chart

# get to all others
filtered_data %>% group_by(Year) %>% summarise(total=sum(`% of Global Spend`)) %>% 
  mutate(Country="All Others",`% of Global Spend`=1-total) %>% select(-total) %>% rbind(top10_chart) -> fin_data

fin_data$label <- percent_format()(fin_data$`% of Global Spend`)

plot <- ggplot(data=fin_data,aes(col=as.factor(Year),y=factor(Country,levels=finFactor),x=`% of Global Spend`)) + 
  geom_point(aes(shape=as.factor(Year)),size=3) +
  geom_text(data=(fin_data%>%filter(Year==2011 & Country %in% c("China","All Others"))),aes(label=label),nudge_x=-0.02,show.legend = FALSE) +
  geom_text(data=(fin_data%>%filter(Year==2015 & Country %in% c("China","All Others"))),aes(label=label),nudge_x=0.02,show.legend = FALSE) +
  geom_text(data=(fin_data%>%filter(Year==2011 & Country %in% c("USA"))),aes(label=label),nudge_x=0.02,show.legend = FALSE) +
  geom_text(data=(fin_data%>%filter(Year==2015 & Country %in% c("USA"))),aes(label=label),nudge_x=-0.02,show.legend = FALSE) +
  scale_shape_manual("",values=c(16,21)) +
  scale_x_continuous(name="% of Global R&D Spend (in PPP$)",labels=percent)+
  scale_y_discrete(name="")+
  scale_color_manual(name="",values=c("#f9893a","grey50"))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text=element_text(colour = "grey50"),
        axis.text=element_text(colour="grey50")) + 
  ggtitle("The USA has long dominated global R&D spending, but China is closing the gap.",
          subtitle="Controlling for purchase power parity (PPP) we see that the top 10 spenders have consistently been close to 95% of global investment. \nThe remaining countries spend a similar amount to 4th place Germany") +
  geom_segment(aes(x=0.165,xend=0.205,y="China",yend="China"),
             size=1,
             arrow = arrow(length = unit(0.2, "cm")),
             col=("grey60")) 
plot  

png('Plot.png', width=1071, height=587, res=100,type="cairo-png")
grid.arrange(plot,
             bottom = textGrob(
               "By @stevejburr - Data Source= UNSECO / How Much?",
               gp = gpar(fontface = 3, fontsize = 9,col="grey50"),
               hjust = 1,
               x = 1
             )
)
dev.off()

