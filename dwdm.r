library(stringr)  # to use regx and other string functions
library(tidyverse)  # to manipulate data
library(dplyr)      # to manipulate data
library(ggplot2)    # to plot graph
library(readr)      # to read flat/tabular text files
library(lubridate)  # to manipulate as date
library(tm)         # to perform text mining operations (for wordcloud here)
library(caret)      # to spilt data and and select featured data
library(wordcloud)  # to write most reasons for crash in a cloud form
library(gridExtra)  # to arrange multiple grid based plots on a page
library(RColorBrewer)# to have nice color palettes
library(DT)

setwd("D:\\")
AirplaneCrashURL <- "air.csv"
AirplaneCrash <- read.csv(AirplaneCrashURL, stringsAsFactors = FALSE ) 
AirplaneCrash <- as_tibble(AirplaneCrash)
AirplaneCrash <- na.omit(AirplaneCrash)
AirplaneCrash <- AirplaneCrash %>% separate(Date, into = c("Month","Day","Year"))
AirplaneCrash$Location <- sapply(AirplaneCrash$Location, as.character)
AirplaneCrash$Location <- gsub(".*,", "", AirplaneCrash$Location)
#remove white space at beginning
AirplaneCrash$Location <- str_trim(AirplaneCrash$Location, side = "both")
#Convert string back to factors
AirplaneCrash$Location <- sapply(AirplaneCrash$Location, as.factor)
#datatable(AirplaneCrash ,extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis')))

#Monthly
months <- as.data.frame(table(AirplaneCrash$Month))
A2 <- ggplot(months, aes(Var1, Freq)) + 
  geom_bar(stat = "identity", fill = "Navy", width = 0.3) + 
  xlab("Month") + ylab("Crashes") +
  ggtitle("Total number of crashes per month")

#Yearly
years <- as.data.frame(table(AirplaneCrash$Year))
A1 <- ggplot(years, aes(y = Freq, x = Var1, group = 1))  + 
  geom_line(size = 1, linetype = 1, color = "Navy") + 
  geom_point(size = 3, shape = 20)+ 
  geom_smooth() +
  xlab("Years") + ylab("Crashes") + 
  scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 10)) + 
  ggtitle("Total number of crashes per year")

grid.arrange(A1, A2, nrow = 2, heights=2:1)



Fatalities <- AirplaneCrash %>% group_by(Year) %>% 
  summarise(total_fatalities = sum(Fatalities), total_passengers = sum(Aboard))

f1 <- ggplot(Fatalities, aes(y = (total_fatalities/total_passengers)*100, x = Year, group = 10))  + 
  geom_line(size = 1, linetype = 1, color = "Red") + 
  geom_point(size = 3, shape = 20) + 
  geom_smooth() +
  xlab("Years") + ylab("% Fatalities") + 
  scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 10)) +
  ggtitle("Percent of fatalities per year")
f1



Location_Crash <-   AirplaneCrash %>% group_by(Location) %>% 
  summarise(total_fatalities = sum(Fatalities)) %>% arrange(desc(total_fatalities))

L1 <- ggplot(Location_Crash[1:10,], aes(x = reorder(Location, -total_fatalities), y = total_fatalities, alpha = total_fatalities)) + 
  geom_bar(stat = "identity", fill = "maroon", width = 0.5) +
  xlab("Countries") + ylab("Number of fatalities") + 
  ggtitle("Top 10 Countries with Maximum Fatalities")
L1



crash_operator <-   AirplaneCrash %>% group_by(Operator) %>% 
  summarise(Freq = n()) %>% arrange(desc(Freq))

operator <- ggplot(crash_operator[1:10,], aes(x = reorder(factor(Operator), Freq), y = Freq, alpha = Freq)) + 
  geom_bar(stat = "identity", fill = "Blue", width = 0.05) + geom_point(stat = "identity") + 
  xlab("Aircraft Operators") + ylab("Crashes") + ggtitle("Top 10 Aircraft Operator causing Aircrash") + 
  coord_flip() 
operator


crash_type <- AirplaneCrash %>% group_by(Type) %>% 
  summarise(Freq = n()) %>% arrange(desc(Freq))

type <- ggplot(crash_type[1:10,], aes(x = reorder(factor(Type), Freq), y = Freq, alpha = Freq)) + 
  geom_bar(stat = "identity", fill = "Purple", width = 0.05) + geom_point(stat = "identity") + 
  xlab("Types") + ylab("Crashes") + ggtitle("Top 10 Aircraft Type causing Aircrash") +
  coord_flip() 
type




data <- Corpus(VectorSource(AirplaneCrash$Summary))

corpus_clean <- tm_map(data, tolower)
corpus_clean <- tm_map(corpus_clean, removePunctuation)
#corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
corpus_clean <- tm_map(corpus_clean, removeWords, "flight")
corpus_clean <- tm_map(corpus_clean, removeWords, "crashed")
corpus_clean <- tm_map(corpus_clean, removeWords, "plane")
corpus_clean <- tm_map(corpus_clean, removeWords, "aircraft")

tdm <- DocumentTermMatrix(corpus_clean)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]

wordcloud(corpus_clean, max.words = 100, min.freq = 35, random.order = FALSE, colors=pal)


AirCrDataFrame <- as.data.frame(AirplaneCrash)
write.csv(AirCrDataFrame,"new.csv")

#####Kmeans
df1 = AirplaneCrash[,c("Aboard","Fatalities","Ground")]
dfCluster<-kmeans(df1,centers=2, iter.max = 17)
Aboard<-AirplaneCrash$Aboard
Fatalities<-AirplaneCrash$Fatalities
plot(Aboard,Fatalities,col=dfCluster$cluster,pch=1,cex=1, main="Major vs Minor Crash")


#####Kmeans 2nd
set.seed(123456789) ## to fix the random starting clusters
grp <- kmeans(df1, centers=2, nstart=10)
grp

o=order(grp$cluster)
data.frame(AirplaneCrash$Location[o],grp$cluster[o])
plot(df1$Aboard, df1$Fatalities, type="n", xlab="Aboard", ylab="Fatalities")
text(x=df1$Aboard, y=df1$Fatalities, labels=AirplaneCrash$Location,col=grp$cluster+1)