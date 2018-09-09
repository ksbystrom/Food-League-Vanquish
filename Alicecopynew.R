## Alice Roberts
## Vanquish 
## September 7, 2018 

#write.csv(Traffic_20Counts_2FPERM_PEDESTRIAN_CT,"TrafficCountPedes.csv")
#write.csv(TrafficCounts_Bikes,"TrafficCountbikes.csv")

#install.packages("qdap")
library(readr)
library(quanteda)
library(corpus)
library(tm)
library(qdap)
library(dplyr)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)



############################# INJURY DATA ############################


injdata = read.csv("injuryData(2008-17).csv", header = TRUE)
#Viewing data 
injdata
#looking at the what was involved for the accidentss
x <- injdata$Modes 
#changing to characters 
x <- as.character(x)
injdata$Collision.Date <- as.Date(injdata$Collision.Date, format = '%d/%m/%Y')
injdata$Collision.Date

#Dropping all of the variables we do not need
injdata$Modes <- droplevels.factor(injdata$Modes, exclude = c("Mot-Ped", "Ped-Unk", "Single Mot", "Single Mot", "Single Ped", "Single Veh", "Veh-Mot", "Veh-Ped", "Veh-Veh"))
injdata <- na.omit(injdata)

#creating two variables for plot
Severeinjdata <-injdata[which(injdata$Injury.Type == "Severe"),]
Minorinjdata <-injdata[which(injdata$Injury.Type == "Minor"),]

## Tabulate
Stab <- table(cut(Severeinjdata$Collision.Date, 'month'))
Mtab <- table(cut(Minorinjdata$Collision.Date, 'month'))

## Format
Severeinjuries <-data.frame(Date=format(as.Date(names(Stab)), '%m/%Y'),
                           Frequency=as.vector(Stab))
Minorinjuries <-data.frame(Date=format(as.Date(names(Mtab)), '%m/%Y'),
           Frequency=as.vector(Mtab))

#creating time and frequency for minor and servere incidents
names(Severeinjuries)
Severetime = Severeinjuries$Date
Severefrequency = Severeinjuries$Frequency

Minortime = Minorinjuries$Date
Minorfrequency = Minorinjuries$Frequency



# plotting the lines for minor and servere incidents

plot(Minorfrequency, type='l', xaxt = 'n', main = "Time Series of Injuries from Cyclists",col="blue", lwd=3,  xlab="Time", ylab="Frequency")
lines(Severefrequency, col="green",lwd=3)
axis(1, at=c(12, 24, 36, 48, 60, 72, 84, 96, 108, 120),
     labels = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
     )
legend(x = 0.005, y = 46, legend = c("Minor Injuries", "Severe Injuries"),lty=1,col=c("blue","green"), bty="n",cex=1.2,lwd=3)





####### Section can be done if we have extra time #########
#adding a character split for single variables
x[x == "Single Cyl"  ] <- "Single Cyl- 0" 


#Spliting the list from the modes 
listHolder <- strsplit(as.character(x), "-")
listHolder<- as.character(listHolder)

#splitting the variable vector into two seperate vectors
dd  <-  as.data.frame(matrix(unlist(listHolder),2)) 

#####################################################################










##################### FUNCTIONS FOR BIKE MAP ##################################


## Creating a function to clean the data
## return dataframe with cleaned data
#involved are the types of transportation involved (vehicles, bikes, pedestrians etc.)
datacleanFun = function(date,details,age,sex,involved)
{
  ##### date #####
  #dealing with date, check ZhiYuhCode if you have time
  
  ##### details #####
  
  
  #Getting rid of all non ascii symbols
  Encoding(details) <- "latin1"
  details <- iconv(details, "latin1", "ASCII", sub="")
  
  # All lowercase, removing punctation and numbers
  details <- tolower(details)
  details <- removePunctuation(details)
  details <- removeNumbers(details)
  
  # Remove stop words from details
  details <-removeWords(details, stopwords('en'))

  
  ##### sex ##### 
  Encoding(sex) <- "latin1"
  sex<-iconv(sex, "latin1", "ASCII", sub="")
  
  
  ##### involved #####
  
  #Removing the commas after the bikemaps collions section
  involved<-gsub(",.*","",involved)
  
  #getting rid of non ascii symbols in involved 
  Encoding(involved) <- "latin1"
  involved<-iconv(involved, "latin1", "ASCII", sub="")
  
  
  ### creating new data frame ### 
  newdataframe <- data.frame(date,details,age,sex,involved)
  
  return(newdataframe)
}


### Creating a function that will plot the most fequent words in the dataframe

FrequencyPlotFUN= function(newdataframe){
  # now I wish to create my bag of words
  detailsFre <- Corpus(VectorSource(newdataframe$details))
  #removing the stop words
  stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  detailsFre = stringr::str_replace_all(details, stopwords_regex, '')
  #viewing 
  detailsFre
  
  #Finding the most frequent terms and plotting it. 
  frequent_terms <- freq_terms(details, 10)
  plot <- plot(frequent_terms,col=47)
  return(frequent_terms)
  
}


clusteringFUN= function(newdataframe)
{
  
  details<- newdataframe$details
  ## creating clustering function 
  details_source <- VectorSource(details)
  # Make a volatile corpus: details_corpus
  details_corpus <- VCorpus(details_source)
  # Print out details_corpus
  details_corpus
  # Print the content of the 15th item in details_corpus
  details_corpus[[15]]$content
  
  clean_corp <- clean_corpus(details_corpus)
  clean_corp[[20]][1]
  
  # Create the dtm from the corpus: details_dtm
  clean_corp
  details_dtm <- DocumentTermMatrix(clean_corp)
  
  # Print out 
  details_dtm
  
  ##get the top20######
  toptwenty<-findFreqTerms(details_dtm , lowfreq = 5)
  
  # Convert coffee_dtm to a matrix: coffee_m
  details_m <- as.matrix(details_dtm)
  newdata<- details_m[,toptwenty]
  lmao<-data.frame(newdata )
  

  k=5
  hc <- hclust(d = dist(lmao, method = "euclidean"), method = "complete")
  DF <- data.frame(as.matrix(lmao), stringsAsFactors=FALSE)
  label_back <-t(data.frame(DF,cutree(hc,k)))
  row.names(label_back) <- NULL
  
  cluscutree <- cutree(hc, k )
  clusnums <- data.frame(cutree(hc, k))
  table(clusnums )
  # Plot a dendrogram
  plot(hc)
  return (clusnums )
  
  
}


## Creating a function to make a word cloud of the cleaned data(see function above)
wordcloudFUN = function(newdataframe,clustercolumn,clusternumber){
  
 
  clusdf <-newdataframe[which(newdataframe$clusnumscoll == clusternumber),]
  clusdf$details
  vs<-VectorSource(clusdf$details)
  detailsC <- Corpus(vs)
  inspect(detailsC)
  
  dtm <- TermDocumentMatrix(detailsC)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  # Creating word cloud 
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  newdataframe$details <-  detailsC
  return(newdataframe)
  
}



  
  
  


############################################################################




############################# BIKEMAP DATA #################################

##### COLLISIONS DATA 

#Reading in the collisions data 
Bikemapscoll <- read_csv("Bikemaps(collision).csv")

# Using functions 
BMColCleanData<- datacleanFun(Bikemapscoll$date ,Bikemapscoll$details, Bikemapscoll$age, Bikemapscoll$sex,Bikemapscoll$incident_with)
head(BMColCleanData,4)

FrequencyPlotFUN(BMColCleanData)
clusnumscoll <-clusteringFUN(BMColCleanData)
BMColCleanData$clusnumscoll<- clusnumscoll
BMColCleanData


#######
wordcloudFUN(BMColCleanData,BMColCleanData$clusnumscoll,1)
wordcloudFUN(BMColCleanData,BMColCleanData$clusnumscoll,2)
wordcloudFUN(BMColCleanData,BMColCleanData$clusnumscoll,4)

#### Summary Statistics 
#cluster 1
summary1 <- BMColCleanData[clusnumscoll$cutree.hc..k. == "1",] 
stat1Sex <- data.frame(table(summary1$sex))
stat1Sex$proportion <- (stat1Sex$Freq/nrow(summary1))

stat1Involved <- data.frame(table(summary1$involved))
stat1Involved$proportion <- data.frame(stat1Involved$Freq/nrow(summary1))

stat1Age <- data.frame(table(summary1$age))
stat1Age$Var1 <- as.numeric(levels(stat1Age$Var1))[stat1Age$Var1]
stat1Age$proportion <- (stat1Age$Freq/nrow(summary1))
2018 - summary(stat1Age$Var1)["Mean"] #showing the mean age

#cluster 2
summary2 <- BMColCleanData[clusnumscoll$cutree.hc..k. == "2",] 
stat2Sex <- data.frame(table(summary2$sex))
stat2Sex$proportion <- (stat2Sex$Freq/nrow(summary2))

stat2Involved <- data.frame(table(summary2$involved))
stat2Involved$proportion <- data.frame(stat2Involved$Freq/nrow(summary2))

stat2Age <- data.frame(table(summary2$age))
stat2Age$Var2 <- as.numeric(levels(stat2Age$Var2))[stat2Age$Var2]
stat2Age$proportion <- (stat2Age$Freq/nrow(summary2))
2018 - summary(stat2Age$Var2)["Mean"] #showing the mean age

#cluster 4
summary4 <- BMColCleanData[clusnumscoll$cutree.hc..k. == "4",] 
stat4Sex <- data.frame(table(summary4$sex))
stat4Sex$proportion <- (stat4Sex$Freq/nrow(summary4))

stat4Involved <- data.frame(table(summary4$involved))
stat4Involved$proportion <- data.frame(stat4Involved$Freq/nrow(summary4))

stat4Age <- data.frame(table(summary4$age))
stat4Age$Var1 <- as.numeric(levels(stat4Age$Var1))[stat4Age$Var1]
stat4Age$proportion <- (stat4Age$Freq/nrow(summary4))
2018 - summary(stat4Age$Var1)["Mean"] #showing the mean age








###### NEARMISS DATA$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
BikemapsnearMiss <- read_csv("Bikemaps(nearMiss).csv")
BMnearmissCleanData<- datacleanFun(BikemapsnearMiss$date ,BikemapsnearMiss$details, BikemapsnearMiss$age, BikemapsnearMiss$sex,BikemapsnearMiss$incident_with)
FrequencyPlotFUN(BMnearmissCleanData)
wordcloudFUN(BMnearmissCleanData)
clusteringFUN(BMnearmissCleanData)\







####### HAZARDS DATA 

#Reading in the hazards data 

Bikemapshazards<- read_csv("Bikemaps(hazards).csv")

# Using functions 
BMHazCleanData<- datacleanFun(Bikemapshazards$date ,Bikemapshazards$details, Bikemapshazards$age, Bikemapshazards$sex,Bikemapshazards$i_type)
head(BMHazCleanData,4)
FrequencyPlotFUN(BMHazCleanData)
wordcloudFUN(BMHazCleanData)
clusteringFUN(BMHazCleanData)



############################################################################

 
##### Fatalitites  ####### 
 
fatalities <- read_csv("fatalities.csv")


#new dataframe with fatalitites soley from bikes 
newfatalities<- fatalities[complete.cases(fatalities$Bike), ]


