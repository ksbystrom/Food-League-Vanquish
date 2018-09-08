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

injdata$Modes <- droplevels.factor(injdata$Modes, exclude = c("Mot-Ped", "Ped-Unk", "Single Mot", "Single Mot", "Single Ped", "Single Veh", "Veh-Mot", "Veh-Ped", "Veh-Veh"))
injdata <- na.omit(injdata)


## Tabulate
tab <- table(cut(injdata$Collision.Date, 'month'))

## Format
TSinjuries <-data.frame(Date=format(as.Date(names(tab)), '%m/%Y'),
           Frequency=as.vector(tab))

library(TSA)

names(TSinjuries)
time = TSinjuries$Date

frequency = TSinjuries$Frequency

plot(frequency, type='l', xaxt = 'n', main = "Time Series of Injuries")
axis(1, at=c(12, 24, 36, 48, 60, 72, 84, 96, 108, 120), 
     labels = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
     )





### Section still needs to be fixed: 
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
  #dealing with date, check ZhiYuhCode when you have time
  
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




FrequencyPlotFUN= function(newdataframe){
  # now I wish to create my bag of words
  detailsFre <- Corpus(VectorSource(newdataframe$details))
  stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  detailsFre = stringr::str_replace_all(details, stopwords_regex, '')
  detailsFre
  
  #Finding the most frequent terms and plotting it. 
  frequent_terms <- freq_terms(details, 10)
  plot <- plot(frequent_terms)
  return(frequent_terms)
  
}
BMColCleanData<- datacleanFun(Bikemapscoll$date ,Bikemapscoll$details, Bikemapscoll$age, Bikemapscoll$sex,Bikemapscoll$incident_with)
head(BMColCleanData,4)
FrequencyPlotFUN(BMColCleanData)

## Creating a function to make a word cloud of the cleaned data(see function above)
wordcloudFUN = function(newdataframe){
 
  detailsC <- Corpus(VectorSource(newdataframe$details))
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
clusteringFUN= function(newdataframe)
{
  details<- newdataframe$details
  
  ## creating clustering function 
  details_source <- VectorSource(details)
  
  # Make a volatile corpus: details_corpus
  details_corpus <- VCorpus(details_source)
  
  # Print out details_corpus
  details_corpus
  
  # Print the content of the 15th tweet in details_corpus
  details_corpus[[15]]$content
  
  clean_corp <- clean_corpus(details_corpus)
  clean_corp[[20]][1]
  
  
  
  # Create the dtm from the corpus: details_dtm
  clean_corp
  
  details_dtm <- DocumentTermMatrix(clean_corp)
  
  # Print out coffee_dtm data
  details_dtm
  
  
  # Convert coffee_dtm to a matrix: coffee_m
  details_m <- as.matrix(details_dtm)
  
  # Print the dimensions of coffee_m
  dim(details_m)
  
  # Review a portion of the matrix
  details_m[14:16, 100:105]
  
  
  # Create a TDM from clean_corp: coffee_tdm
  details_tdm <- TermDocumentMatrix(clean_corp)
  
  
  details_tdm2 <- removeSparseTerms(details_tdm, sparse = 0.975)
  
  hc <- hclust(d = dist(details_tdm2, method = "euclidean"), method = "complete") 
  
  # Plot a dendrogram
  plot(hc)
  
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
wordcloudFUN(BMColCleanData)
clusteringFUN(BMColCleanData)




###### NEARMISS DATA
BikemapsnearMiss <- read_csv("Bikemaps(nearMiss).csv")
BMnearmissCleanData<- datacleanFun(BikemapsnearMiss$date ,BikemapsnearMiss$details, BikemapsnearMiss$age, BikemapsnearMiss$sex,BikemapsnearMiss$incident_with)
FrequencyPlotFUN(BMnearmissCleanData)
wordcloudFUN(BMnearmissCleanData)
clusteringFUN(BMnearmissCleanData)




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

 
 







