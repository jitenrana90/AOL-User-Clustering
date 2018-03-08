dir <- "/home/aol/""
setwd(dir)
library(tm)
library(topicmodels)
library(slam)
library(lsr)
library(entropy)
library(infotheo)

load('w8.RData')

data2<-data
data2$Query<-as.character(data2$Query)
data2<-data2[nchar(data2$Query) > 1,]
result<-aggregate(Query ~ AnonID, data = data2, unique, collapse = ' ')
r<-result

concat<-function(str){
  
  tmp<-paste(unlist(str), collapse = ' ')
  
}
r$Query1<-unlist(lapply(r$Query, 'concat'))
r<-r[,c(1,3)]
result<-r
rm(r)
docs<-Corpus(VectorSource(result$Query))
# writeLines(as.character(docs[1]))

#### Pre Processing ####
docs<-tm_map(docs, content_transformer(tolower))
docs<-tm_map(docs, removeNumbers)
docs<-tm_map(docs, content_transformer(gsub), pattern = '=', replacement = '')
docs<-tm_map(docs, content_transformer(gsub), pattern = '-', replacement = '')

myStopwords <- c('can', 'say','one','way','use',
                 'also','howev','tell','will',
                 'much','need','take','tend','even',
                 'like','particular','rather','said',
                 'get','well','make','ask','come','end',
                 'first','two','help','often','may',
                 'might','see','someth','thing','point',
                 'post','look','right','now','think','‘ve ',
                 '‘re ','anoth','put','set','new','good',
                 'want','sure','kind','larg','yes,','day','etc',
                 'quit','sinc','attempt','lack','seen','awar',
                 'littl','ever','moreov','though','found','abl',
                 'enough','far','earli','away','achiev','draw',
                 'last','never','brief','bit','entir','brief',
                 'great','lot','of', 'you','then','there','where'
                 ,'when','who','which','when','what','at','on','is','are',
                 'will','was','were','that','this','those','these',
                 'they','it','he','she','whose','could','would', 'all','but'
                 ,'u', 'me', 'for','how','for', 'i','the','and','with','free',
                 'from','does','do','did','done','have','has','had',
                 'http','www','www.','com','.com','about','above')
docs<-tm_map(docs, removeWords, myStopwords)

#### Create Document Term Matrix ####
dtm <- DocumentTermMatrix(docs)
dtm1 <- dtm[row_sums(dtm, na.rm = T) > 0,col_sums(dtm, na.rm = TRUE)>=30]
rownames(dtm1) <- as.character(result$AnonID[row_sums(dtm, na.rm = T) > 0])
dtm.new<-dtm1[row_sums(dtm1, na.rm = TRUE) > 0,]

freq<-colSums(as.matrix(dtm.new))
ord<-order(freq, descending = TRUE,na.last = TRUE)
write.csv(freq, 'word_frequency.csv')
####### Building Topic Models ########
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# Number of Topics
k <- seq(10,200,10)


dtm.new<-dtm.new[row_sums(dtm.new, na.rm = TRUE) > 0,col_sums(dtm.new) >= 400]
dtm.new<-dtm.new[row_sums(dtm.new) > 0,]


# rowTotals <- apply(dtm1 , 1, sum)
# dtm.new   <- dtm1[rowTotals> 0, ]


#Run LDA using Gibbs sampling
# lda_custom<-function(n){
ldaOut <-LDA(dtm.new,15, method='Gibbs', control=list(nstart=nstart, seed = seed, 
             best=best, burnin = burnin, iter = iter, thin=thin))
# }
# ldaOuts<-lapply(50, 'lda_custom')
# logliks<-lapply(unlist(ldaOuts), 'logLik')

# library(lda)
# ldaOut2<-lda.collapsed.gibbs.sampler(docs,K = 10,num.iterations = 20,compute.log.likelihood = TRUE)
transP<-ldaOut@gamma
rownames(transP)<-1:nrow(transP)
transP<-as.data.frame(transP)
transP<-tFrame(transP)
