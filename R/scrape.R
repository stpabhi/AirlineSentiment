#
# scrape.R - scrape web data and cache to the data/ directory:
#
# airline-related tweets via twitteR's searchTwitter()
#	ACSI scores with XML's readHTMLTable()
#
if (VERBOSE)
  print("Searching Twitter for airline tweets and saving to disk")

require(twitteR)

# load twitter credentials
load("twitter authentication.Rdata")
# register twitter credentials
registerTwitterOAuth(twitCred)

# retrieve tweets from Twitter
since<-'2014-01-01'
until<-'2014-12-04'

delta.tweets = searchTwitter('@delta',5000,since=since,until=until,cainfo="cacert.pem")
save(delta.tweets, file=file.path(dataDir, 'delta.tweets.RData'), ascii=T)

american.tweets = searchTwitter('@americanair',5000,since=since,until=until,cainfo="cacert.pem")
save(american.tweets, file=file.path(dataDir, 'american.tweets.RData'), ascii=T)

jetblue.tweets = searchTwitter('@jetblue',1100,cainfo="cacert.pem")
save(jetblue.tweets, file=file.path(dataDir, 'jetblue.tweets.RData'), ascii=T)

southwest.tweets = searchTwitter('@southwestair',5000,since=since,until=until,cainfo="cacert.pem")
save(southwest.tweets, file=file.path(dataDir, 'southwest.tweets.RData'), ascii=T)

united.tweets = searchTwitter('@united',5000,since=since,until=until,cainfo="cacert.pem")
save(united.tweets, file=file.path(dataDir, 'united.tweets.RData'), ascii=T)

us.tweets = searchTwitter('@usairways',5000,since=since,until=until,cainfo="cacert.pem")
save(us.tweets, file=file.path(dataDir, 'us.tweets.RData'), ascii=T)

if (VERBOSE)
  print("Scraping ACSI airline scores and saving to disk")

require(XML)

# this assumes 2014 scores
acsi.url = 'http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'

# we want the first table (which=1) on the page, which has column headers (header=T)
acsi.raw.df = readHTMLTable(acsi.url, header=T, which=1, stringsAsFactors=F)
acsi.df = acsi.raw.df[,c(1,22)]

# change the columnn names ("14" -> "score" since we're only looking at most recent)
colnames(acsi.df) = c('airline', 'score')

# add codes for later matching, and make sure score is treated as a number (not a string)
acsi.df$code = c('B6', 'WN', 'DL', NA, NA, 'AA', 'US', 'UA', 'CO', 'NW')
acsi.df$score = as.numeric(acsi.df$score)

save(acsi.raw.df, file=file.path(dataDir, 'acsi.raw.df.RData'), ascii=T)
save(acsi.df, file=file.path(dataDir, 'acsi.df.RData'), ascii=T)