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



############################# COLLISIONS DATA ############################
#Reading in the collisions data 

Bikemapscoll <- read_csv("Bikemaps(collision).csv")

#Removing the commas after the bikemaps collions section
incident <- Bikemapscoll$incident_with
incident<-gsub(",.*","",incident)
Bikemapscoll$incident_with <-  incident


### Starting the NLP for collisions ### 

details <- Bikemapscoll$details
#Seeing what gives the problems 
num <- grep("Within", details)
prob <- details[num]
#getting rid of non ascii symbols
Encoding(details) <- "latin1"
iconv(details, "latin1", "ASCII", sub="")


#make it lower case
details <- tolower(details) 

#getting rid of the punctation 
details <- gsub('[[:punct:]]', '', details)


# now I wish to create my bag of words
details <- Corpus(VectorSource(details))
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
details = stringr::str_replace_all(details, stopwords_regex, '')
details

#Finding the most frequent terms and plotting it. 
frequent_terms <- freq_terms(details, 10)
plot(frequent_terms)




############################# HAZARDS DATA ############################
#Reading in the hazards data 

### Starting the NLP for hazards  ### 

Bikemapshazards<- read_csv("Bikemaps(hazards).csv")

detailsHaz<-  Bikemapshazards$details
#getting rid of non ascii symbols
Encoding(detailsHaz) <- "latin1"
iconv(detailsHaz, "latin1", "ASCII", sub="")


#make it lower cases
detailsHaz <- tolower(detailsHaz) 

#getting rid of the punctation 
detailsHaz <- gsub('[[:punct:]]', '', detailsHaz)


# now I wish to create my bag of words
detailsHaz <- Corpus(VectorSource(detailsHaz))
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
detailsHaz = stringr::str_replace_all(detailsHaz, stopwords_regex, '')
detailsHaz

#Finding the most frequent terms and plotting it. 
frequent_terms <- freq_terms(detailsHaz, 10)
plot(frequent_terms)






