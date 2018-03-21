library(tm)
library(wordcloud)
library(ggmap)
library(ggplot2)

###############################################################################



###############################################################


#!!! IMPORTANT - set to working directory
setwd("~/R/project_city")


twee = read.csv("data/complaints.csv",header=TRUE,row.names=NULL,fileEncoding="latin1")

#twee = read.csv("data/complaints.csv",header=TRUE,row.names=NULL)

twee=as.data.frame(twee)

resolved_count=0
onjob_count=0
open_job=0

for(i in (1:nrow(twee)))
{	if(twee[i,]$Complaint.status=="Resolved")
			resolved_count=resolved_count+1
			   #print("resolved")
	else if(twee[i,]$Complaint.status=="Open")
			open_job=open_job+1
				#print("open"
	else if(twee[i,]$Complaint.status=="On-the-Job")
			onjob_count=onjob_count+1
				#print("onthejob")}
}

p=c(resolved_count,onjob_count,open_job)
labels=c("Resolved","On-the-Job","Open")
pct=round(p/sum(p)*100)
labels=paste(labels,pct)
labels=paste(labels,"%",sep="")
pie(p,labels=labels,main="State of complaints")

gggg
labels=c("Resolved","On-the-Job","Open")


for(i in labels)
{
current_type = subset(twee,twee$Complaint.status==i)
myCorpus=Corpus(VectorSource(current_type$Complaint.description))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)

#courpus = tm_map(myCorpus, stripWhitespace)

### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  
# add two extra stop words: 
myStopwords <- c(stopwords("english"), "available","r","via","can","amp","and","it","is","to","the","th","please","pleas","request")
gggg
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myCorpus <- tm_map(myCorpus, gsub, pattern = "garbag", replacement = "garbage")
myCorpus <- tm_map(myCorpus, gsub, pattern = "peopl", replacement = "people")
myCorpus <- tm_map(myCorpus, gsub, pattern = "vehicl", replacement = "vehicle")
myCorpus <- tm_map(myCorpus, gsub, pattern = "bangalor", replacement = "bangalore")

# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))

## Freqency words and Association
#idx <- which(dimnames(tdm)$Terms == "r")
#inspect(tdm[idx + (0:5), 1:10])

#inspect frequent words
wc_f=400
if(i=="Open")
	wc_f=100
(freq.terms <- findFreqTerms(tdm, lowfreq=wc_f))
#print(freq.terms)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = TRUE)

d <- data.frame(word = names(word.freq),freq=word.freq)
print(i)
print("")
print(head(d))
wc_f=400
if(i=="Open")
	wc_f=100

wordcloud(words = names(word.freq), freq = word.freq, min.freq = wc_f,random.order = F)	
}

print("After removing roads ")
for(i in labels)
{
current_type = subset(twee,twee$Complaint.status==i)
myCorpus=Corpus(VectorSource(current_type$Complaint.description))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)

#courpus = tm_map(myCorpus, stripWhitespace)

### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available","r","via","can","amp","and","it","is","to","the","th","please","pleas","request","road")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myCorpus <- tm_map(myCorpus, gsub, pattern = "garbag", replacement = "garbage")
myCorpus <- tm_map(myCorpus, gsub, pattern = "peopl", replacement = "people")
myCorpus <- tm_map(myCorpus, gsub, pattern = "vehicl", replacement = "vehicle")
myCorpus <- tm_map(myCorpus, gsub, pattern = "bangalor", replacement = "bangalore")

# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))

## Freqency words and Association
#idx <- which(dimnames(tdm)$Terms == "r")
#inspect(tdm[idx + (0:5), 101:110])

#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq=15))


m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
d <- data.frame(word = names(word.freq),freq=word.freq)
print(i)
print("")
print(head(d))
wc_f=400
if(i=="Open") # adjusting for Open complaints having much lesser data
	wc_f=100

wordcloud(words = names(word.freq), freq = word.freq, min.freq = wc_f,random.order = F)	
}


