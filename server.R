 

library(shiny)
library(tm)
library(lsa)

shinyServer
(
  
  
  function(input, output) 
   {
    output$result<-renderText({
    keyw <-input$Phrase 
   # keylist<- as.character(strsplit(toString(input$Phrase) ," "))
    keylist<- unlist(strsplit(as.character(input$Phrase)," "))
      keylist<-removeWords(keylist, words)
    
    })
   
    output$func <- renderDataTable({
     costab <- c(0,0)
      keyw <-input$Phrase 
      keylist<- unlist(strsplit(as.character(keyw)," "))
      keylist<-removeWords(keylist, words)
      qdata <- as.data.frame(keylist)
      names(qdata)[1] <- paste("qword")
      qdata$tf <- 0
      qdata$idf<-0
      qdata$tfidf <-0
#      print(head(qdata))
  
      if(length(which(qdata$qword==""))>0)
        qdata <- qdata[-which(qdata$qword==""),]
      row.names(qdata)<-NULL
      
      for(i in 1:length(qdata$qword))
        qdata$tf[i] <- length(grep(qdata$qword[i], keylist))
      for(i in 1:length(qdata$qword))
      {
        if (tolower(as.character(qdata$qword[i])) %in% tolower(as.character(idf_allterms$Word)))
          qdata$idf[i]<- idf_allterms$IDF[which(tolower(as.character(idf_allterms$Word))==tolower(as.character(qdata$qword[i])))]
      }
      for(i in 1:length(qdata$qword))
        qdata$tfidf[i] <- qdata$tf[i] * qdata$idf[i]
      
    ############### done with cosine################  
      
      cost= lsa(tfidf, dims=dimcalc_share())
      coss = as.textmatrix(cost)
      
#     x<- as.vector(as.double(qdata[,4]))
#
#           for(i in 1:199)
#    {   if(length(grep("",keylist)>0))
#          y<- as.vector(tfidf[which(rownames(tfidf) %in% keylist),i])
#    
#   
#      costab[i]<- cosine(x,y)
 #          } 
   
       x<- as.vector(as.double(qdata[,4]))
      
                 for(i in 1:199)
          {   if(length(grep("",keylist)>0))
                y<- as.vector(coss[which(rownames(tfidf) %in% keylist),i])

            costab[i]<- cosine(x,y)
                } 
     tmp <- as.data.frame(costab)
     tmp$Docnum <- 0
     tmp <- tmp[order(tmp$costab,decreasing = T),]
     tmp$Docnum <- row.names(tmp)
     row.names(tmp) <- NULL
   #tmp <- tmp[1:10,] 
      tmp$Doc <- ""
      tmp$Doc <- mydata$Heading[as.integer(tmp$Docnum)]
   #   tmp <- tmp[1:10,] 
      
    colnames(tmp)<- c("score","Doc number","Document")
   ##################################################
      
      
      
     #########does the same thing######
   #  newlist<- sort(costab,decreasing = TRUE)
     #print(match(newlist,costab))
     #################################
     
    #  prlist<-newlist[1:20]
   # final <-as.data.frame(which(costab %in% prlist))
   # final$score <- 0
   # final$score <- prlist
  #  colnames(final) <- c("Document","Score")
    #for(i in 1:length(final))
    
    # newdataf <- as.data.frame( which(costab %in% prlist[rownames(final)]))
    # newdataf$score <- 0
    # newdataf$score <- prlist
     #colnames(newdataf) <- c("Document","Score")
    
  #  print(which(costab %in% prlist[6]))
    
   #row.names(final)<- prlist[which(costab %in% prlist)]
    
    
    #check prlist and arrange the documents in that order
    
    #########################################################
    # print(which(costab %in% prlist[6]))
    #final <-as.data.frame(sort(costab,decreasing = TRUE))
    # prlist<-final[1:20,]
    # final$doc <- 0
    # final$doc <- which(costab %in% prlist[rownames(final)])
    ########################################################### 
    
    return(tmp)
  })
})

##check for other input values##

