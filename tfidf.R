mydata = read.csv("C:/Users/impadmin.DESKTOP-4D45FH6/Desktop/Articles.csv")

#to remove stop words
remove_stopwords(as.character(x$Article), words)
tmp <- as.data.frame(unique(removeWords(posting,words)))
#to create posting list
posting <- strsplit(as.String(as.character(x$Article))," ")
#to remove punctuations
gsub("[[:punct:]]","",df$Word) -> df$Word

#to copy postings to data frame
df <- as.data.frame(posting)
#to count the number of docs that contain the term
length(grep("The",x$Article))
#saving the count in count variable
for (i in 1:length(df$Word))  df$count[i]<-length(grep(df$Word[i],x$Article))
#to find tf in each doc
for(i in 1:length(df$Word))
  + df$tf[i] <- length(grep(df$Word[i],as.list(unlist(strsplit(as.character(x$Article[1])," ")[1]))))
#to find the IDF val
for(i in 1:length(df$Word))
  df$IDF[i] <- log2(length(df$Word)/df$count[i])
--------------------------
  for(i in 1:length(tmp$word))
  tmp$idf[i] <- log2(length(tmp$word)/tmp$count[i])
---------------------------
#to change col names of tf-idf to all words
colnames(tfidf) <- idf_allterms$Word

#############################
# to make a dummy data frame
tmp <- as.data.frame(1:nrow(x))
for(i in 1:length(unique(df$Word))) 
  tmp[,i] <- 0
colnames(tmp) <- unique(df$Word)

# to fill the table with frequency
for(i in 1:length(unique(df$Word)))
  for(j in 1:nrow(x))
    tmp[j,i] <- length(grep(unique(df$Word[i]),as.list(unlist(strsplit(as.character(x$Article[j])," ")))))

############create a table for tf-idf

tfidf<- as.data.frame(1:nrow(x))
for(i in 1:length(unique(df$Word))) 
tfidf[,i] <- 0
#########
for(i in 1: ncol(tfidf))
tfidf[,i] <- tmp [ , i ]* idf_allterms$IDF[i]

#########to save a file ni da format
 save(list = ls(),file = "tfidf.rda")

###### to remove extra stopwords by names.
 tfidf <- tfidf[, !names(tfidf) %in% (colnames(tfidf) %in% words)]
query <- "Karachi peroleum , gas and cng government"

##### find sum usin names in the sum coloumn
for(i in 1:length(which(colnames(tfidf) %in% qlist)))
tfidf$sum <- tfidf$sum + tfidf[,which(colnames(tfidf) %in% qlist)[i]]
##### to copy the idf from idfallterms to qdata for te corresponding terms
for(i in 1:length(qdata$qword))
{
  if (tolower(as.character(qdata$qword[i])) %in% tolower(as.character(idf_allterms$Word)))
    qdata$idf[i]<- idf_allterms$IDF[which(tolower(as.character(idf_allterms$Word))==tolower(as.character(qdata$qword[i])))]
}
######to find out the cosine
for (i in 1:nrow(y))
       yval <- yval + y[i,]^2
 for(i in 1:length(x))
       xval <- xval + x[i]^2
 xval<- sqrt(xval)
 yval<- sqrt(yval)
 newval <- 0
newval<- xy /(xval * yval)
#for qdata$tf
for(i in 1:length(qdata$qword))
qdata$tf[i] <- length(grep(qdata$qword[i], keylist))
#method 2
 cosine(x,y)
##query to find the position of top 5 in costab
#reduce list o 5 then
 match(newlist,costab)
 # resul of match funcion - > 1 110   9 157 148
 ##check why the values are different 