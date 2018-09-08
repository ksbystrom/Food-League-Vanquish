## Alice Roberts
## Vanquish 
## September 7, 2018 

write.csv(Traffic_20Counts_2FPERM_PEDESTRIAN_CT,"TrafficCountPedes.csv")
write.csv(TrafficCounts_Bikes,"TrafficCountbikes.csv")

############################# INJURY DATA ############################


injdata = read.csv("injuryData(2008-17).csv", header = TRUE)
#Viewing data 
injdata
#looking at the what was involved for the accidents
x <- injdata$Modes 
#changing to characters 
x <- as.character(x)


#adding a character split for single cells
x[x == "Single Cyl"  ] <- "Single Cyl- 0" 
x[x == "Single Veh"  ] <- "Single Veh- 0" 
x[x == "Single Mot"  ] <- "Single Mot- 0" 
x[x == "Single Veh"  ] <- "Single Veh- 0" 


#Spliting the list from the modes 
listHolder <- strsplit(as.character(x), "-")
listHolder<- as.character(listHolder)

#splitting the variable vector into two seperate vectors
dd  <-  as.data.frame(matrix(unlist(listHolder),2)) 



############################# COLLISIONS DATA ############################
#Reading in the collisions data 
library(readr)
Bikemapscoll <- read_csv("Bikemaps(collision).csv")

#Removing the commas after the bikemaps collions section
incident <- Bikemapscoll$incident_with
incident<-gsub(",.*","",incident)
incident



############################# HAZARDS DATA ############################
#Reading in the hazards data 
library(readr)
Bikemapshazards<- read_csv("Bikemaps(hazards).csv")

