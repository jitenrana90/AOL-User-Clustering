setwd(dir = 'Desktop/Co Laptop/Analytics Exercises/C1X/')
library(stringr)
library(sqldf)
library(caret)

###################### Reading the DataSet #################
data <- read.csv(file = 'user-ct-test-collection-02.csv', header = TRUE, sep = '\t')
data$index1<-1:nrow(data)
data$QueryTime<-as.POSIXct(data$QueryTime)
save(data, file = 'data.Rda')

##### Summary ######
# # of lines: 3537372
# # of Unique Users: 61592
# # of unique Queries: 1219923
# # of Overall Clicks: 1902838
# # of unique URLs Visited: 384917

######################


###################### Forming data set at User level #########################
data$index1<-1:nrow(data)

#### By User, getting no of queries
query<-"SELECT AnonID,
               COUNT(DISTINCT Query) as No_of_Queries
               FROM data
               GROUP BY AnonID"
clicks1<-sqldf(query)

data1<-data[!is.na(data$ItemRank),]

#### by User, getting no of queries yielding in clicks, page views, unique urls visit, 
query<-"SELECT AnonID, COUNT(DISTINCT Query) No_of_Relevant_Searches,
               COUNT(DISTINCT index1) as No_of_Pages_Viewed,
               COUNT(DISTINCT ClickURL) as Unique_URL_Visited,
               AVG(ItemRank) as PageRank
               FROM data1
               GROUP BY AnonID"
clicks2<-sqldf(query)
clicks<-merge(x = clicks1, y = clicks2, by = 'AnonID', all = TRUE)

### computing realted matrices ###
clicks$avg_relevant_searches<-clicks$No_of_Relevant_Searches/clicks$No_of_Queries
clicks$Average_Pages_Per_Query <- clicks$No_of_Pages_Viewed/clicks$No_of_Relevant_Searches


#### Relevant Search Summary ####
#  Min. 1st Qu.  Median    Mean  3rd Qu.   Max.    NA's 
# 0.009   0.308   0.469   0.472   0.600   1.000   12298 

#### Average Page per Query Summary ####
#   Min. 1st Qu.  Median  Mean    3rd Qu.  Max.    NA's 
# 1.000   1.167   1.667   2.043   2.251  111.800   12298 

##########################################
#### Time Spend Calculation ####
##########################################

data$TimeDiff<-0
data$TimeDiff[2:nrow(data)]<-difftime(data$QueryTime[2:nrow(data)],data$QueryTime[1:(nrow(data)-1)], units = 'secs')
data$TimeDiff[data$TimeDiff < 0]<-0
data$TimeDiff2<-data$TimeDiff

### By definition, two activities with difference of more than half an hour
### in time are considered to be from different sessions
data$TimeDiff2[data$TimeDiff2 > 1800]<-0

unc1<-unique(data$AnonID[is.na(data$ItemRank)])
unc2<-unique(data$AnonID[!is.na(data$ItemRank)])
unc<-unc1[!(unc1 %in% unc2)]
unc<-sqldf("SELECT AnonID, ")

#### function to calculate session duration for a user ####
calculate_session_duration<-function(userid){
  tmp<-data$TimeDiff2[data$AnonID == userid]
  count1<-1
  count2<-1
  if(length(tmp) <= 1){count1<-0;count2<-0; total_duration <- 0}
  else{
    b<-c(0,sapply(2:length(tmp),function(x) if(tmp[[x]] > 0 && tmp[[x-1]] == 0) 
    {return(1)} else{return(-1)} ))
    
    b[1]<-ifelse(tmp[1]== 0, -1 ,1)
    count1<-sum(b[b==1])+length(tmp[tmp==0])
    count2<-sum(b[b==1])+1
    total_duration <-sum(tmp, na.rm = TRUE)
  }
  return(c(total_duration,count1, count2))
}

unique_users<-unique(data$AnonID)
session_duration_users<-lapply(unique_users, 'calculate_session_duration')

#### AVerage Session length and no of sessions at user level computation
users<-as.data.frame(unique_users)
users$id<-unique_users
users$session<-0
users$count1<-0
users$count2<-0
for(i in 1:length(session_duration_users)){
  
  users$session[i]<-session_duration_users[[i]][1]
  users$count1[i]<-session_duration_users[[i]][2]
  users$count2[i]<-session_duration_users[[i]][3]
  print(i)
}
users<-users[,-1]
users$session_length<-users$session/users$count1
##########################################
#### Time Spend Calculation Ends####
##########################################

###################### User Level Computation Ends ######################

######################################################################
###################### Query Level Computation #######################
######################################################################

##################################################
##### Query Parsing and key words extraction #####
##################################################

####### Splitting Queries and Extracting Key Words
split_queries<-lapply(X = data_sample$Query, FUN = 'str_split', ' ')
split_queries
query_words<-unlist(split_queries)
query_words<-as.data.frame(query_words)
query_words$index1<-1
names(query_words)[1]<-'word'

query_words$word<-str_trim(query_words$word)
query_words$word<-tolower(query_words$word)

##### Key word wise roll up
query_keywords_freq<-sqldf("SELECT word, SUM(index1) as count
                           FROM query_words
                           GROUP BY word
                           ORDER BY SUM(index1) DESC")
query_keywords_freq$word<-as.character(query_keywords_freq$word)
query_keywords_freq_clean<-query_keywords_freq[nchar(query_keywords_freq$word) != 1,]


#### Reading list of words to be excluded from a file and removing them
#### from queries to arrive at keywords and query length

exclusion<-read.csv(file = 'exclusion.csv', header = TRUE)
exclusion$words<-as.character(exclusion$words)
query_keywords_freq_clean<-query_keywords_freq_clean[!(query_keywords_freq_clean$word %in% exclusion$words),]
# query_keywords_freq_clean<-query_keywords_freq_clean[!(query_keywords_freq_clean$word %in% c('of','and','the','for','to','from')),]

query_keywords_freq_clean$rank<-1:nrow(query_keywords_freq_clean)

query_parsed<-vector(length = 10)
for(i in 1:length(split_queries)){
  tmp<-str_trim(tolower(split_queries[[i]][[1]]))
  tmp2<-vector(length = ifelse(length(tmp) >= 10, 0,10-length(tmp)))
  if(length(tmp2)>0){
    tmp2[1:length(tmp2)]<-""
    tmp<-c(tmp, tmp2)
  }else{tmp<-tmp[1:10]}
  query_parsed<-rbind(query_parsed,tmp)
  print(paste('i: ',i))
}

query_parsed<-as.data.frame(query_parsed)
query_parsed<-query_parsed[-1,]

#### Reading list of words to be excluded from a file and removing them
#### from queries to arrive at keywords and query length
words_to_exclude<-read.csv(file = 'exclusion.csv', header = TRUE)
length_of_query<-function(str){
  str[[1]]<-tolower(str_trim(str[[1]]))
  len<-sum(!(str[[1]] %in% word_to_exclude))
  len
}
length_queries<-unlist(lapply(data$split_queries,'length_of_query'))
data$no_keywords<-length_queries

#############################################
##### Query Parsing Ends #####
#############################################


##### Roll up at Query level ######

#### by Query, getting no of users, no of keywords
common_query<-sqldf("SELECT Query, COUNT(DISTINCT AnonID) as count,
                    MIN(no_keywords) as length_query
                    FROM data GROUP BY Query 
                    ORDER BY COUNT(DISTINCT AnonID) 
                    DESC")
#### by Query, getting no of clicks, no of page views, average page rank
successful_clicks<-sqldf("SELECT Query, COUNT(DISTINCT AnonID) as No_of_Clicks, 
                         COUNT(DISTINCT index1) as Total_Clicks,
                         AVG(ItemRank) as Average_Page_Rank
                         FROM data1
                         GROUP BY Query")
common_query<-merge(x = common_query, y = successful_clicks, by = 'Query', all = TRUE)
common_query<-merge(common_query, td, by = 'Query', all.x = TRUE)

##### Computing other query related matrices
common_query$Click_Rate<-common_query$No_of_Clicks*100/common_query$count
common_query_orig<-common_query
common_query$Query<-as.character(common_query$Query)
common_query<-common_query[nchar(common_query$Query)>1,]
common_query$No_Pages_Opened<-common_query$Total_Clicks/common_query$No_of_Clicks
common_query<-sqldf("SELECT * FROM common_query ORDER BY count DESC")


############# Most and Least common Queries ################
most_common_queries<-common_query[common_query$Click_Rate >= 90 & !is.na(common_query$Click_Rate) 
                                  & common_query$count >=30,]
least_common_queries<-common_query[common_query$Click_Rate <= 20 & !is.na(common_query$Click_Rate) 
                                   & common_query$count >=30,]



### Query Length vs Click Rate Computation for length upto 20 ###
acc_rate<-vector()
for(i in 1:20){
  
  tmp<-sum(common_query[common_query$length == i & common_query$count>=30,]$No_of_Clicks, na.rm = TRUE)*100/
    sum(common_query[common_query$length == i & common_query$count>=30,]$count, na.rm = TRUE)
  acc_rate<-c(acc_rate,tmp)
}

#######################################################
########### Query Relevance Computation ###########
#######################################################

query_url<-sqldf("SELECT Query, ClickURL,AVG(ItemRank) as Rank FROM
                  data1
                 GROUP BY
                 Query,ClickURL")
query_url$Query<-as.character(query_url$Query)
query_url$ClickURL<-as.character(query_url$ClickURL)

########## Defining relevance function based on edit distance ############
relevance<-function(a,b){
  
  spl<-unlist(str_split(a,' '))
  distance<-(nchar(b)*length(spl) - sum(unlist(lapply(spl, 'adist',b))))/nchar(b)
  
}
edit_distance<-mapply('relevance',query_url$Query , query_url$ClickURL)
query_url$relevance<-as.numeric(edit_distance)

#### by Query, getting average relevance score
query_relevane_rank<-sqldf("SELECT Query, AVG(Rank) as Rank, AVG(relevance) as relevance
                           FROM query_url
                           WHERE relevance >=0
                           AND relevance <= 1
                           GROUP BY Query
                           ORDER BY AVG(relevance) DESC")
common_query<-merge(common_query,query_relevane_rank, by = 'Query', all.x = TRUE)
common_query<-sqldf("SELECT * FROM common_query ORDER BY count DESC")
common_query<-common_query[,-(ncol(common_query)-1)]
common_query_rank<-common_query[!is.na(common_query$Click_Rate),]

####### Extracting Top Queries by Relevance Score ######
common_query_rank<-sqldf("SELECT * FROM common_query_rank WHERE Average_Click_Rank >=1
                         AND count >=30 ORDER BY Average_Click_Rank")


#######################################################
########### Query Level Roll up Ends ###########
#######################################################


# ######### Sampling the Data ###########
# r<-createDataPartition(data$AnonID, p = 0.2, list = FALSE)
# write.csv(x = r, file = 'samples.csv', row.names = FALSE)
# data_sample<-data[r,]

###################### Query Level Computation Ends ######################



############ URL releated Computation ##########
################ Roll Ups by URL ################

query<-"SELECT ClickURL, COUNT(DISTINCT AnonID) as count,
        AVG(ItemRank) as avg_rank FROM
        data1
        WHERE ItemRank <> 0
        GROUP BY ClickURL
        ORDER BY COUNT(DISTINCT AnonID)
        DESC"

urls<-sqldf(query)
urls<-sqldf("SELECT * FROM urls WHERE count >= 30 ORDER BY avg_rank")
urls$ClickURL<-as.character(urls$ClickURL)

data$ClickURL<-as.character(data$ClickURL)
top50urls_query<-data[data$ClickURL %in% urls$ClickURL[1:50],]

## code for key words in top 50 url
## Extracting top 5 keywords from TOP 50 URLs displayed

unique_url<-as.character(unique(top50urls_query$ClickURL))
op <-vector()
for(i in 1:length(unique_url)){
  tmp<-unlist(top50urls_query[top50urls_query$ClickURL == unique_url[i],]$split_queries)
  tmp<-as.data.frame(tmp)
  names(tmp)<-'word'
  tmp$index1<-1
  tmp2<-sqldf("SELECT word,sum(index1) as count from tmp group by word ORDER BY SUM(index1) DESC LIMIT 5")
  words<-as.character(tmp2$word)
  op<-rbind(op,words)
  print(i)
}

op<-cbind(unique_url,op)
names(op)<-c('url', 'word1','word2','word3', 'word4', 'word5')


# len<-function(str,n){
#   tmp1<-str[[1]][n]
#   tmp1
# }
# tdq<-unlist(lapply(td,'len',1))
# tdn<-unlist(lapply(td,'len',2))
# tdn<-as.numeric(as.character(tdn))
# td<-data.frame(tdq)
# td<-cbind(td,tdn)

####################################################################################









