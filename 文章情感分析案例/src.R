library("sqldf")
library("jiebaR")

  cappend<-function(x,a){
  return (c(x[1:length(x)-1],a,x[length(x)]))
}

  #test calculate("F:/handler.txt","F:/data.csv")
  calculate<-function(filepath,datapath){
  x<c()
  n <- read.table(filepath) 
  data <- read.table(datapath,header=T, sep=",") 
  for(i in 1 : length(n)){
    text <- paste0(n[,i],"")
    res <-sqldf::sqldf(paste0("select * from data where 词语='",text,"'"))
    r = paste0(unlist(res['情感分类']),"")
      if( r != ""){
        x<-cappend(x,r) 
    }
  }
  barplot(table(x),col = runif(length(unique(x)),min=1,max=length(colors())))
  return(x)
}