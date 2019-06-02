require(httr)
require(jsonlite)

#function for getting comments

comments <- function(name, key){
  url <- paste0('https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&maxResults=100&videoId=',name,'&maxResults=50&key=',key)
  res<-GET(url)
  res<-rawToChar(res$content)
  res<- fromJSON(res)
  nexttoken<-res$nextPageToken
  result0<-res$items$snippet$topLevelComment$snippet$textOriginal
  
  while (!is.null(nexttoken)) {
    url2<-paste0(url,'&pageToken=',nexttoken)
    res<-GET(url2)
    result<-rawToChar(res$content)
    result<- fromJSON(result)
    result1<-result$items$snippet$topLevelComment$snippet$textOriginal
    nexttoken<-result$nextPageToken
    result0<-rbind(result0,result1)
  }
  return(data.frame(Comments = c(t(result0))))
}

com1 <- comments("SSrjAXK5pGw", "AIzaSyDjUAfD8u4RVt5AxiEHd7NjZT99KhanyJ4")
com2 <- comments("TCy_UOjEir0", "AIzaSyDjUAfD8u4RVt5AxiEHd7NjZT99KhanyJ4")
com3 <- comments("OhX2KQs3v5w", "AIzaSyDjUAfD8u4RVt5AxiEHd7NjZT99KhanyJ4")
#Video 4 has comments blocked, so no comment-analysis for Video 4
  