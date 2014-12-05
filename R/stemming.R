stemming = function(tweets)
{
  require(tm)
  # convert tweets to a data frame
  # tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
  tweets.df<-twListToDF(tweets)
  # build a corpus, and specify the source to be character vectors
  myCorpus<-Corpus(VectorSource(tweets.df$text),readerControl = list(language="en"))
  # keep a copy of corpus to use later as a dictionary for stem completion
  myCorpusCopy <- myCorpus
  # convert to lower case
  myCorpus<-tm_map(myCorpus,content_transformer(tolower))
  # remove URLs
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  myCorpus <- tm_map(myCorpus, removeURL)
  # remove mentions
  removeMentions <- function(x) gsub("(@\\w+)", "", x)
  myCorpus<-tm_map(myCorpus,removeMentions)
  # remove html code
  removeHTMLCode<-function(x) gsub("(&\\w+;)"," ",x)
  myCorpus<-tm_map(myCorpus,removeHTMLCode)
  # remove punctuation
  removePunctuation<-function(x) gsub("[[:punct:]]"," ", x)
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # remove control characters
  removeCtrlCharacter<-function(x) gsub("[[:cntrl:]]"," ", x)
  myCorpus<-tm_map(myCorpus,removeCtrlCharacter)
  # remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
  # remove non-english characters
  removeNonEnglishChars<-function(x) gsub("[^[:alnum:]///' ]", " ", x)
  myCorpus<-tm_map(myCorpus,removeNonEnglishChars)
  # strip whitespace
  myCorpus<-tm_map(myCorpus,stripWhitespace)
  
  return(myCorpus)
}