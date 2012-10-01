rm(list=ls())

#installing necessary packages
install.packages("twitteR")
require(twitteR)
install.packages("ggplot2")
require(ggplot2)
install.packages("stringr")
require(stringr)
install.packages("plyr")
require(plyr)


#reading in data, either through twitteR or csvs

SOTUfull<-read.csv("C://Users//sbhaskar//desktop//SOTU.csv", 
                             header=TRUE, strip.white=TRUE);
SOTUbuffet<-read.csv("C://Users//sbhaskar//desktop//sotu_buffett.csv", 
                             header=TRUE, strip.white=TRUE);
SOTUmadeintheusa<-read.csv("C://Users//sbhaskar//desktop//sotu_madeintheusa.csv", 
                             header=TRUE, strip.white=TRUE);
SOTUmiddleclass<-read.csv("C://Users//sbhaskar//desktop//sotu_middleclass.csv", 
                             header=TRUE, strip.white=TRUE);
SOTUobamajobs<-read.csv("C://Users//sbhaskar//desktop//sotu_obama_jobs.csv", 
                             header=TRUE, strip.white=TRUE);
SOTUromneyjobs<-read.csv("C://Users//sbhaskar//desktop//sotu_romney_jobs.csv", 
                             header=TRUE, strip.white=TRUE);

#grep for certain phrases
indices_jobs<-grep("jobs", SOTUfull$status);
SOTUjobs<-SOTUfull[indices_jobs,];

indices_energy<-grep("energy", SOTUfull$status);
SOTUenergy<-SOTUfull[indices_energy,];

indices_tax<-grep("tax", SOTUfull$status);
SOTUtax<-SOTUfull[indices_tax,];

indices_equalpay<-grep("equal pay", SOTUfull$status);
SOTUequalpay<-SOTUfull[indices_equalpay,];

indices_women<-grep("women", SOTUfull$status);
SOTUwomen<-SOTUfull[indices_women,];

indices_education<-grep("education", SOTUfull$status);
SOTUeducation<-SOTUfull[indices_education,];

indices_economy<-grep("economy", SOTUfull$status);
SOTUeconomy<-SOTUfull[indices_economy,];

#binding twitteR data into data frames
  df_sotuFULL<- do.call("rbind", lapply(SOTUfull, as.data.frame))
  
  names(df_sopa)
  names(df_sopaobama)
  head(df_sopa,3)
  head(df_sopaobama,3)

      counts_sopaobama=table(df_sopaobama$screenName)
      barplot(counts_sopaobama)
      cc_sopaobama=subset(counts_sopaobama,counts>1)
      barplot(cc_sopaobama,las=2,cex.names=0.3)
      
      counts_sopa=table(df_sopa$screenName)
      barplot(counts_sopa)
      cc_sopa=subset(counts_sopa,counts>1)
      barplot(cc_sopa,las=2,cex.names=0.3)

      counts_sopaobamaupdate=table(df_sopaobamaupdate$V4)
      barplot(counts_sopa)
      cc_sopa=subset(counts_sopa,counts>1)
      barplot(cc_sopa,las=2,cex.names=0.3)

#adjusting text
df_sotu$text=sapply(df_sotu$text,function(row) iconv(row,to='UTF-8'))
df_sotu$text=sapply(df$text,function(row) iconv(row,to='UTF-8'))
df_SOTUfull$status=sapply(df$text,function(row) iconv(row,to='UTF-8'))

trim <- function (x) sub('@','',x)

#cleaning up RTs and Tos
library(stringr)
df_SOTUfull$to=sapply(df$text,function(tweet) str_extract(tweet,"^(@[[:alnum:]_]*)"))

df_SOTUfull$to=sapply(df$text,function(tweet) str_extract(tweet,"^(@[[:alnum:]_]*)"))

df_SOTUfull$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))

ggplot()+geom_bar(aes(x=na.omit(df_sopaobama$rt)))+opts(axis.text.x=theme_text(angle=-90,size=6))+xlab(NULL)

#reading in library of pos and neg words
hu.liu.pos<-scan('C://users//sbhaskar//desktop//positive-words.txt', what='character', comment.char=';')
hu.liu.neg<-scan('C://users//sbhaskar//desktop//negative-words.txt', what='character', comment.char=';')

#writing sentiment scoring function
                score.sentiment<-function(sentences, pos.words, neg.words, .progress='none')
                {
                    require(plyr)
                    require(stringr)
                     
                    # we got a vector of sentences. plyr will handle a list
                    # or a vector as an "l" for us
                    # we want a simple array ("a") of scores back, so we use
                    # "l" + "a" + "ply" = "laply":
                    scores<-laply(sentences, function(sentence, pos.words, neg.words) {
                         
                        # clean up sentences with R's regex-driven global substitute, gsub():
                        sentence<-gsub('[[:punct:]]', '', sentence)
                        sentence<-gsub('[[:cntrl:]]', '', sentence)
                        sentence<-gsub('\\d+', '', sentence)
                        # and convert to lower case:
                        sentence = tolower(sentence)
                 
                        # split into words. str_split is in the stringr package
                        word.list<-str_split(sentence, '\\s+')
                        # sometimes a list() is one level of hierarchy too much
                        words<-unlist(word.list)
                 
                        # compare our words to the dictionaries of positive & negative terms
                        pos.matches<-match(words, pos.words)
                        neg.matches<-match(words, neg.words)
                     
                        # match() returns the position of the matched term or NA
                        # we just want a TRUE/FALSE:
                        pos.matches<-!is.na(pos.matches)
                        neg.matches<-!is.na(neg.matches)
                 
                        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
                        score<-sum(pos.matches) - sum(neg.matches)
                 
                        return(score)
                    }, pos.words, neg.words, .progress=.progress )
                 
                    scores.df<-data.frame(score=scores, text=sentences)
                    return(scores.df)
                }

df2<-df_sopaobama$text[df_sopaobama$id %in% data]
df_sopaobama$id %in% data
length(df_sopaobama$id)

class(df2$text)

#scoring sentiments
df.scores<-score.sentiment(df_sotu$text, hu.liu.pos, hu.liu.neg, .progress='text');
df.scores2<-score.sentiment(SOTUjobs$status, hu.liu.pos, hu.liu.neg, .progress='text');
df.scores3<-score.sentiment(SOTUenergy$status, hu.liu.pos, hu.liu.neg, .progress='text');
df.scores4<-score.sentiment(SOTUeducation$status, hu.liu.pos, hu.liu.neg, .progress='text');
df.scores5<-score.sentiment(SOTUtax$status, hu.liu.pos, hu.liu.neg, .progress='text');
df.scores6<-score.sentiment(SOTUequalpay$status, hu.liu.pos, hu.liu.neg, .progress='text');
df.scores7<-score.sentiment(SOTUwomen$status, hu.liu.pos, hu.liu.neg, .progress='text');
df.scores8<-score.sentiment(SOTUeconomy$status, hu.liu.pos, hu.liu.neg, .progress='text');

#plotting results
mean(as.numeric(df.scores2$score));
mean(as.numeric(df.scores3$score));
mean(as.numeric(df.scores4$score));
mean(as.numeric(df.scores5$score));
mean(as.numeric(df.scores6$score));
mean(as.numeric(df.scores7$score));
mean(as.numeric(df.scores8$score));

hist(as.numeric(df.scores8$score), main='Sentiment Scores of "SOTU_economy"',sub=mean(as.numeric(df.scores8$score)),xlab='scores');
hist(as.numeric(df.scores3$score), main='Sentiment Scores SOTU_energy', xlab='scores')

#make a cool looking plot
data2all_unique$type<-"Keystone";
obamaSOPATweets$type<-"Keystone+Obama"
allTweets<-as.data.frame(df_sotu$text)

p<-ggplot(allTweets, aes(dt, fill=text))+geom_density(adjust=0.01, alpha=0.5)
p<-p+xlab("Tweet Date");
p<-p+scale_y_continuous("Tweet Density");



##comparing against BO followers
              
              followerIds<-data<-read.csv("C://Users//sbhaskhar//desktop//Twitter Follower Scraper//allFollowers_BarackObama_1220.txt", 
                             header=FALSE, strip.white=TRUE);
              rm(followerIds)
              table(df_sopa$id %in% data)
              count(df_sopa$id %in% data)
              count(sopaTWEETS$id %in% data)

followerIds<-sopaIDs<-read.csv("C://Users//sbhaskhar//desktop//Twitter Follower Scraper//SOPA_idsFromTweets.txt", 
                             header=FALSE, strip.white=TRUE);
              rm(followerIds)

followerIds<-sopaobamaIDs<-read.csv("C://Users//sbhaskhar//desktop//Twitter Follower Scraper//SOPA_AND_OBAMA_idsFromTweets.txt", 
                             header=FALSE, strip.white=TRUE);
              rm(followerIds)
