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
############################# INJURY DATA ############################


injdata = read.csv("injuryData(2008-17).csv", header = TRUE)
#Viewing data 
injdata
#looking at the what was involved for the accidents
x <- injdata$Modes 
#changing to characters 
x <- as.character(x)


#adding a character split for single variables
x[x == "Single Cyl"  ] <- "Single Cyl- 0" 
x[x == "Single Veh"  ] <- "Single Veh- 0" 
x[x == "Single Mot"  ] <- "Single Mot- 0" 
x[x == "Single Veh"  ] <- "Single Veh- 0" 


#Spliting the list from the modes 
listHolder <- strsplit(as.character(x), "-")
listHolder<- as.character(listHolder)

#splitting the variable vector into two seperate vectors
dd  <-  as.data.frame(matrix(unlist(listHolder),2)) 



#####################################################################






#########################FUNCTION####################################
## Creating a function to clean the data
## return dataframe with cleaned data
#involved are the types of transportation involved (vehicles, bikes, pedestrians etc.)
datacleanFun = function(date,details,age,sex,involved)
{
  
  ##### date #####
  
  
  ##### details #####
  
  #getting rid of non ascii symbols in the details
  Encoding(details) <- "latin1"
  iconv(details, "latin1", "ASCII", sub="")
  
  #make details lower case
  details <- tolower(details) 
  
  #getting rid of the punctation  in details 
  details <- gsub('[[:punct:]]', '', details)
  
  
  
  ##### sex ##### 
  Encoding(sex) <- "latin1"
  iconv(sex, "latin1", "ASCII", sub="")
  
  
  ##### involved #####
  
  #Removing the commas after the bikemaps collions section
  involved<-gsub(",.*","",involved)
  
  #getting rid of non ascii symbols in involved 
  Encoding(involved) <- "latin1"
  iconv(involved, "latin1", "ASCII", sub="")
  
  
  
  ### creating new data frame ### 
  newdataframe = data.frame(date,details,age,sex,involved)
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
  
}


## Creating a function to make a word cloud of the cleaned data(see function above)
wordcloudFUN = function(newdataframe){
  
  
  detailsC <- Corpus(VectorSource(newdataframe$details))
  inspect(detailsC)
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  detailsC <- tm_map(detailsC, toSpace, "/")
  detailsC <- tm_map(detailsC, toSpace, "@")
  detailsC <- tm_map(detailsC, toSpace, "\\|")
  
  
  
  # Convert the text to lower case
  detailsC <- tm_map(detailsC, content_transformer(tolower))
  # Remove numbers
  detailsC <- tm_map(detailsC, removeNumbers)
  # Remove english common stopwords
  detailsC <- tm_map(detailsC, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  detailsC <- tm_map(detailsC, removeWords, c("blabla1", "blabla2")) 
  # Remove punctuations
  detailsC <- tm_map(detailsC, removePunctuation)
  # Eliminate extra white spaces
  detailsC <- tm_map(detailsC, stripWhitespace)
  # Text stemming
  # detailsC <- tm_map(detailsC, stemDocument)
  
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

####### HAZARDS DATA

#Reading in the hazards data 

Bikemapshazards<- read_csv("Bikemaps(hazards).csv")

# Using functions 
BMColCleanData<- datacleanFun(Bikemapshazards$date ,Bikemapshazards$details, Bikemapshazards$age, Bikemapshazards$sex,Bikemapshazards$incident_with)
head(BMColCleanData,4)
FrequencyPlotFUN(BMColCleanData)
wordcloudFUN(BMColCleanData)

###### NEARMISS DATA

library(readr)
 BikemapsnearMiss <- read_csv("Bikemaps(nearMiss).csv")
 BMColCleanData<- datacleanFun(BikemapsnearMiss$date ,BikemapsnearMiss$details, BikemapsnearMiss$age, BikemapsnearMiss$sex,BikemapsnearMiss$incident_with)
 head(BMColCleanData,4)
 FrequencyPlotFUN(BMColCleanData)
 wordcloudFUN(BMColCleanData)


 ############################################################################
 
 
 
 
 
 
 







