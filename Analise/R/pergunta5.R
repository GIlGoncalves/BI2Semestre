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
ficheiro<-ficheiro[,c(-4,-16,-17,-18,-27,-10)]

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
cenas2=c(0)

pergunta5<-ficheiro[,c(2,9,8,18)]
pergunta5[,"Lucro"]<-cenas2

pergunta5$Lucro<-pergunta5$gross - pergunta5$budget
par(mfrow=c(1,2))

barplot(table(pergunta5$Lucro))

ARRAY<- which(pergunta5[5]>0,arr.ind = T)

pergunta5=pergunta5[,-c(3,4)]


for(i in 1:nrow(pergunta5)){
  
  if(pergunta5[i,"Lucro"]<0) {
    
    pergunta5[i,"Lucro"]<-"Prejuizo"
    
    
  }
  
  if(pergunta5[i,"Lucro"]==0) {
    
    pergunta5[i,"Lucro"]<-"Lucro zero"
    
    
  }
  
}

pergunta5[ARRAY[,"row"],"Lucro"]<-"Lucro"

barplot(table(pergunta5$Lucro))



write.csv(pergunta5,"Idade.csv", row.names = TRUE)

txn = read.transactions(file="Idade.csv", rm.duplicates= TRUE,sep=",")

summary(txn)
inspect(txn[1:10])


rules <-  apriori(txn,parameter = list(support =0.0005, confidence = 0.6,target = "rules", minlen = 2))




df <- as(rules, "data.frame") 
df[order(df$lift, df$confidence), ]

summary(rules)

inspect(sort(rules , by = "lift")[1:15])

inspect(rules)
inspect(rules[1:10])