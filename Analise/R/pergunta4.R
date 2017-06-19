setwd("cadeiras/bi/analise de dados/trabalho/")
library(grid)
library(gdata)
library(arulesViz)
library(Matrix)
library(arules)

set.seed(123456)

ficheiro<-read.csv("outMovieFinal.csv",header = T, stringsAsFactors = TRUE)

for(i in 29:length(ficheiro)) {
  
  ficheiro[is.na(ficheiro[i]),i]<-0
  
}

ficheiro<-ficheiro[,c(-4,-16,-17,-18,-22,-27,-10)]





array<-which(ficheiro[2]=="", arr.ind = T)

ficheiro<-ficheiro[-array[,1],]

array<-which(ficheiro[1]=="", arr.ind = T)

for(i in 1:length(array[,"row"])){
  
  ficheiro[array[i,1],"color"]<-"Color"
  
}

for(i in 1:length(ficheiro)){
 array<-which(is.na(ficheiro[i]),arr.ind = T)
if(length(array)>0) {
       ficheiro<-ficheiro[-array[,1],]
}
}


pergunta4<-ficheiro[,c(1,2,6,9,13,15,16,18,20)]
#pergunta4<-pergunta4[,-c(1,6,7)]
#pergunta4<-pergunta4[,-c(1,6,7,3,4,5)]
#pergunta4<-pergunta4[,-c(1,2,6,7,5)]
pergunta4<-pergunta4[,-c(1,6,7,3,5,8)]
#pergunta4<-pergunta4[,-c(1,2,6,7,5,8)]

ARRAY<-which(pergunta4[3]>=8.5, arr.ind = T)


for(i in 1:nrow(pergunta4)) {
  
  if(pergunta4[i,"imdb_score"]>=0 && pergunta4[i,"imdb_score"]<=3 ) {
    
    pergunta4[i,"imdb_score"]<-"muito_fraco"
    
  } 
  
  if(pergunta4[i,"imdb_score"]>3 && pergunta4[i,"imdb_score"]<=6 ) {
    
    pergunta4[i,"imdb_score"]<-"fraco"
    
  } 
  
  
  
  if(pergunta4[i,"imdb_score"]>=6.1 && pergunta4[i,"imdb_score"]<=7 ) {
    
    pergunta4[i,"imdb_score"]<-"medio"
    
    
  }
  
  if(pergunta4[i,"imdb_score"]>7 && pergunta4[i,"imdb_score"]<7.5 ) {
    
    pergunta4[i,"imdb_score"]<-"bom"
    
    
  }
  
  if(pergunta4[i,"imdb_score"]>=7.5 && pergunta4[i,"imdb_score"]<8.5 ) {
    
    pergunta4[i,"imdb_score"]<-"muito_bom"
    
    
  }
  
}

pergunta4[ARRAY[,"row"],"imdb_score"]<-"sucesso"


par(mfrow=c(1,2))

barplot(table(ficheiro$imdb_score))
barplot(table(pergunta4$imdb_score))
min(ficheiro$imdb_score)
max(ficheiro$imdb_score)
#pergunta4$imdb_score<-discretize(pergunta4$imdb_score, "frequency", categories=10)

#table(pergunta4$imdb_score)
write.csv(pergunta4,"ItemList.csv", row.names = TRUE)

txn4 = read.transactions(file="ItemList.csv", rm.duplicates= TRUE,sep=",");

summary(txn4)
inspect(txn4[1:10])


rules.4 <-  apriori(txn4,parameter = list(support =0.0005, confidence = 0.60,target = "rules", minlen = 2))




df.4 <- as(rules.4, "data.frame") 
df.4[order(df.4$lift, df.4$confidence), ]

summary(rules.4)

inspect(sort(rules.4 , by = "lift")[1:15])
inspect(rules.4)
inspect(rules.4[1:15])