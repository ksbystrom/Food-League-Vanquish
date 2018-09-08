## Alice Roberts
## Vanquish 
## September 7, 2018 

write.csv(Traffic_20Counts_2FPERM_PEDESTRIAN_CT,"TrafficCountPedes.csv")
write.csv(TrafficCounts_Bikes,"TrafficCountbikes.csv")

injdata = read.csv("injuryData(2008-17).csv", header = TRUE)
#Viewing data 
injdata
#looking at the what was involved for the accidents
x <- injdata$Modes 
x <- as.character(x)


#adding a character split for single cells
x[x == "Single Cyl"  ] <- "Single Cyl- 0" 
x[x == "Single Veh"  ] <- "Single Veh- 0" 
x[x == "Single Mot"  ] <- "Single Mot- 0" 
x[x == "Single Veh"  ] <- "Single Veh- 0" 



#Spliting the list from the modes 
listHolder <- strsplit(as.character(x), "-")
listHolder<- as.character(listHolder)


dd  <-  as.data.frame(matrix(unlist(listHolder), 2))

