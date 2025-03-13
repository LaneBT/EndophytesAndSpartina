# Spartina Manuscript Code: Annotated 

#Total Length Analysis (root and shoot length combined)----

#Importing Data
## Clean the data before importing it!! To complete dataset at longest length and prevent negative growth rates?

getwd()

rawdata <- read.csv("Spartina_CleanedtoMax_24.csv")

#Create new object to manipulate raw data

rawdata1 <- rawdata

# combine root lengnth and shoot length data, remove NA values

rawdata1$tot1 <- rowSums(cbind(rawdata1$SL1,rawdata1$RL1),na.rm=TRUE)
rawdata1$tot2 <- rowSums(cbind(rawdata1$SL2,rawdata1$RL2),na.rm=TRUE)
rawdata1$tot3 <- rowSums(cbind(rawdata1$SL3,rawdata1$RL3),na.rm=TRUE)
rawdata1$tot4 <- rowSums(cbind(rawdata1$SL4,rawdata1$RL4),na.rm=TRUE)
rawdata1$tot5 <- rowSums(cbind(rawdata1$SL5,rawdata1$RL5),na.rm=TRUE)
rawdata1$tot6 <- rowSums(cbind(rawdata1$SL6,rawdata1$RL6),na.rm=TRUE)
rawdata1$tot7 <- rowSums(cbind(rawdata1$SL7,rawdata1$RL7),na.rm=TRUE)
rawdata1$tot8 <- rowSums(cbind(rawdata1$SL8,rawdata1$RL8),na.rm=TRUE)
rawdata1$tot9 <- rowSums(cbind(rawdata1$SL9,rawdata1$RL9),na.rm=TRUE)

#reformat data into new object, remove unneccesary columns, rename columns
#NEW DATA SET: Include identifying columns, create new column IDing the measrement number (first, second, third, etc), and combined length measured (total length)

library(tidyr)

data <- pivot_longer(rawdata1,c(23,24,25,26,27,28,29,30,31),names_to="totalmeasurement",values_to="totalgrowth",values_drop_na=TRUE)[c(1,2,3,4,23,24)]

#turn meaurement column into just numbers, ie remove 'tot'
library(stringr)
data$totalmeasurement <- str_remove(data$totalmeasurement,"tot")

#turn measurement ID numbers into numeric values
data$totalmeasurement <- as.numeric(data$totalmeasurement)

#revisit why I did this..... !!!
#I think it was to move data set to a new object to be manipulated for analytical tests

library(dplyr)
data3 <- data%>%
  filter(totalgrowth>0)%>%
  group_by(Endophyte,EnvironStress,Seed.ID,Group)

#fit lm (natural log) for each group
# This estiamte a value for intercept and measurment coefficientfor each replicate and saves as rateresults
# I am interested in the slope!!! For growth rate.

rateresults <- group_modify(data3,~broom::tidy(lm(log(totalgrowth)~totalmeasurement, data=.x)))
rateresults <- rateresults[rateresults$term != "(Intercept)",]
rateresults #look at this...'estimate' value is the growth rate!!

#NOTE: CHECK AT THIS POINT TO LOOK FOR NEGATIVE GROWTH RATES/WEIRDNESS


#find average coefficient estimate (growth rate) for OTU/treatment groups
#remove replicate as grouping variable
rateresults1 <- group_by(rateresults,Endophyte,EnvironStress)

#condense data into one growth rate value per treatment.
#Create new data fram with Id variables (otu and treatment) and summarized growth rate (estimate)
growthrates_avg <-  summarise_at(rateresults1, vars(estimate), list(estimate = mean))
#This is just to visualize. 

#Graphical Analysis ----

#Use growthrates_avg to create general figures

library(ggplot2)

#average growthrate barchart
ggplot(growthrates_avg,aes(x=Endophyte, y=estimate,fill=EnvironStress))+
  geom_bar(position="dodge",stat="identity")+
  ggtitle("Average Growth Rate of Spartina")+
  labs(x="Endphyte Combination",y="Average Growthrate")+
  theme(axis.text.x = element_text(angle=90))

#average growht rates, faceted by environmental stress
ggplot(growthrates_avg,aes(x=Endophyte, y=estimate,fill=Endophyte))+
  geom_bar(stat="identity")+
  labs(x="Endophyte OTU",y="Growth Rate")+
  ggtitle("Total Growth Rate of Spartina")+
  facet_wrap(~EnvironStress,scale="free")+
  theme(axis.text.x = element_text(angle=90))

#average growth rates, faceted by OTU
ggplot(growthrates_avg,aes(x=EnvironStress, y=estimate,fill=EnvironStress))+
  geom_bar(stat="identity")+
  labs(x="Environmental Stress",y="Growth Rate")+
  ggtitle("Total Growth Rate of Spartina")+
  facet_wrap(~Endophyte,scale="free")+
  theme(axis.text.x = element_text(angle=90))

#boxplot of all growthrates, faceted by environmental stressor
ggplot(rateresults1,aes(x=Endophyte,y=estimate,fill=Endophyte))+
  geom_boxplot()+
  labs(x="Endophyte Combo",y="total Spartina growthrate")+
  ggtitle("Growthrate of Spartina")+
  facet_wrap(~EnvironStress,scale="free")+
  theme(axis.text.x = element_text(angle=90))


#boxplot of all growthrates, faceted by endophyte
ggplot(rateresults1,aes(x=EnvironStress,y=estimate,fill=EnvironStress))+
  geom_boxplot()+
  facet_wrap(~Endophyte,scale="free")

#boxplot by endophyte?
ggplot(dat128,aes(x=EnvironStress,y=estimate,fill=EnvironStress))+
  geom_boxplot()

#just for fun... boxplot of growth other time? 
datafun <- pivot_longer(rawdata1,c(23,24,25,26,27,28,29,30,31),names_to="totalmeasurement",values_to="totalgrowth",values_drop_na=TRUE)[c(1,2,3,4,23,24)]
data128 <- datafun%>%
  filter(Endophyte=="128")

ggplot(data128,aes(x=totalmeasurement,y=totalgrowth,fill=EnvironStress))+
  geom_boxplot()+
  ggtitle("Growth of OTU 128 Across Environments")+
  labs(x="Measurement number",y="Growth in millimeters")
#This is cool!!!

#same thing (actual growth amounts in mm) but of all my data, colored by endp and faceted by stressor
ggplot(datafun,aes(x=totalmeasurement,y=totalgrowth,fill=Endophyte))+
  geom_boxplot()+
  facet_wrap(~EnvironStress,scale="free")+
  ggtitle("Growth of Spartina over time")+
  labs(x="Total Growth of Spartina (mm)",y="Measurement number")



#Statistical Analysis ----

#Complete Anova tests using rateresults1
