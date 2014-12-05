if (VERBOSE)
{
  print("Extracting text from tweets & calculating sentiment scores")
  flush.console()
}
#### stemming data or cleaning data

## American corpus
american.tweets.corpus<-stemming(american.tweets)
#american.tweets.corpus_clean <- tm_map(american.tweets.corpus, PlainTextDocument)
#american.tdm <- TermDocumentMatrix(american.tweets.corpus_clean, control=list(removePunctuation=TRUE))
american.tweets_new <- as.list(american.tweets.corpus)

## Delta corpus
delta.tweets.corpus<-stemming(delta.tweets)
delta.tweets.corpus_clean <- tm_map(delta.tweets.corpus, PlainTextDocument)
delta.tdm <- TermDocumentMatrix(delta.tweets.corpus_clean, control=list(removePunctuation=TRUE))
delta.tweets_new <- as.list(delta.tweets.corpus)

## jetblue corpus
jetblue.tweets.corpus<-stemming(jetblue.tweets)
jetblue.tweets.corpus_clean <- tm_map(jetblue.tweets.corpus, PlainTextDocument)
jetblue.tdm <- TermDocumentMatrix(jetblue.tweets.corpus_clean, control=list(removePunctuation=TRUE))
jetblue.tweets_new <- as.list(jetblue.tweets.corpus)

## southwest corpus
southwest.tweets.corpus<-stemming(southwest.tweets)
southwest.tweets.corpus_clean <- tm_map(southwest.tweets.corpus, PlainTextDocument)
southwest.tdm <- TermDocumentMatrix(southwest.tweets.corpus_clean, control=list(removePunctuation=TRUE))
southwest.tweets_new <- as.list(southwest.tweets.corpus)

## united corpus
united.tweets.corpus<-stemming(united.tweets)
united.tweets.corpus_clean <- tm_map(united.tweets.corpus, PlainTextDocument)
united.tdm <- TermDocumentMatrix(united.tweets.corpus_clean, control=list(removePunctuation=TRUE))
united.tweets_new <- as.list(united.tweets.corpus)

## us airways corpus
us.tweets.corpus<-stemming(us.tweets)
us.tweets.corpus_clean <- tm_map(us.tweets.corpus, PlainTextDocument)
us.tdm <- TermDocumentMatrix(us.tweets.corpus_clean, control=list(removePunctuation=TRUE))
us.tweets_new <- as.list(us.tweets.corpus)

american.text = laply(american.tweets_new, function(t) t[1])
delta.text = laply(delta.tweets_new, function(t) t[1])
jetblue.text = laply(jetblue.tweets_new, function(t) t[1])
southwest.text = laply(southwest.tweets_new, function(t) t[1])
united.text = laply(united.tweets_new, function(t) t[1])
us.text = laply(us.tweets_new, function(t) t[1]) 

american.scores = score.sentiment(american.text, pos.words, neg.words, .progress='text')
delta.scores = score.sentiment(delta.text, pos.words, neg.words, .progress='text')
jetblue.scores = score.sentiment(jetblue.text, pos.words, neg.words, .progress='text')
southwest.scores = score.sentiment(southwest.text, pos.words, neg.words, .progress='text')
united.scores = score.sentiment(united.text, pos.words, neg.words, .progress='text')
us.scores = score.sentiment(us.text, pos.words, neg.words, .progress='text')

american.scores$airline = 'American'
american.scores$code = 'AA'
delta.scores$airline = 'Delta'
delta.scores$code = 'DL'
jetblue.scores$airline = 'JetBlue'
jetblue.scores$code = 'B6'
southwest.scores$airline = 'Southwest'
southwest.scores$code = 'WN'
united.scores$airline = 'United'
united.scores$code = 'UA'
us.scores$airline = 'US Airways'
us.scores$code = 'US'

all.scores = rbind(american.scores, delta.scores, jetblue.scores, 
                    southwest.scores, united.scores, us.scores)

if (VERBOSE)
  print("Plotting score distributions")

# ggplot works on data.frames, always
g.hist = ggplot(data=all.scores, mapping=aes(x=score, fill=airline))

# add a bar graph layer. Let it bin the data and compute frequencies
# (set binwidth=1 since scores are integers)
g.hist = g.hist + geom_bar(binwidth=1)

# make a separate plot for each airline
g.hist = g.hist + facet_grid(airline~.)

# plain display, nice colors
g.hist = g.hist + theme_bw() + scale_fill_brewer() 

print(g.hist)
ggsave(file.path(outputDir, 'twitter_score_histograms.pdf'), g.hist, width=6, height=5.5)


if (VERBOSE)
  print("Comparing Twitter & ACSI data")

all.scores$very.pos.bool = all.scores$score >= 2
all.scores$very.neg.bool = all.scores$score <= -2

all.scores$very.pos = as.numeric(all.scores$very.pos.bool)
all.scores$very.neg = as.numeric(all.scores$very.neg.bool)

twitter.df = ddply(all.scores, c('airline', 'code'), summarise, 
                   very.pos.count=sum(very.pos), 
                   very.neg.count=sum(very.neg))

twitter.df$very.tot = twitter.df$very.pos.count + twitter.df$very.neg.count

twitter.df$score = round(100 * twitter.df$very.pos.count / 
                            twitter.df$very.tot)

require(doBy)
orderBy(~-score, twitter.df)

compare.df = merge(twitter.df, acsi.df, by=c('code', 'airline'), 
                   suffixes=c('.twitter', '.acsi'))


# build scatter plot
g.scatter = ggplot(compare.df, aes(x=score.twitter, y=score.acsi)) + geom_point(aes(color=airline),size=5) + theme_bw() + theme(legend.position=c(0.2, 0.85))

# have ggplot2 fit and plot a linear model with R's lm() function
g.fit = g.scatter + geom_smooth(aes(group=1), se=F, method="lm")

print(g.scatter)
print(g.fit)

ggsave(file.path(outputDir, 'twitter_acsi_comparison.pdf'), g.scatter, width=7, height=7)
ggsave(file.path(outputDir, 'twitter_acsi_comparison_with_fit.pdf'), g.fit, width=7, height=7)
