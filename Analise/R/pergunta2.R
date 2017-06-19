set.seed(123456)
library(rpart)
library(rpart.plot)
library(randomForest)

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

pergunta2<-ficheiro[,c(4,5,7,8,11,12,17,18,19,20,21)]


#soma=0
#for(i in 1 : length(pergunta2)) {
#soma=soma+sum(is.na(pergunta2[,7]))
#}

for(i in 1:length(pergunta2)){
 array<-which(is.na(pergunta2[i]),arr.ind = T)
if(length(array)>0) {
  pergunta2<-pergunta2[-array[,1],]
}
}






train <- sample(1:nrow(pergunta2),size=ceiling(0.7*nrow(pergunta2)),replace =
                    FALSE)
hr.train <- pergunta2[train,]
hr.test <- pergunta2[-train,]


maxs <- apply(pergunta2, 2, max) 
mins <- apply(pergunta2, 2, min)

scaled <- as.data.frame(scale(hr.train, center = mins, scale = maxs - mins))

scaled_test <- as.data.frame(scale(hr.test, center = mins, scale = maxs - mins))

#Regressão linear
glm.model <- glm(gross~.,data=scaled)
glm.predict <- predict(glm.model, scaled_test, type="response")
glm.R2 <-  1 - ( sum((scaled_test$gross - glm.predict)^2) / 
                    sum((scaled_test$gross - mean(scaled_test$gross))^2))
glm.RMSE <- sqrt(mean(glm.predict - scaled_test$gross)^2)


# decision tree
dt.model <- rpart(gross~.,data=scaled)
dt.predict <- predict(dt.model, scaled_test[,-4])
dt.R2 <-  1 - ( sum((scaled_test$gross - dt.predict)^2) / 
                    sum((scaled_test$gross - mean(scaled_test$gross))^2) )
dt.RMSE <- sqrt(mean(dt.predict - scaled_test$gross)^2)

# random forest
rf.model <- randomForest(gross~., scaled, mtry = floor(sqrt(ncol(scaled))), ntree = 1000)
rf.predict <- predict(rf.model, scaled_test[,-4])
rf.R2 <-  1 - ( sum((scaled_test$gross - rf.predict)^2) / 
                  sum((scaled_test$gross - mean(scaled_test$gross))^2) )
rf.RMSE <- sqrt(mean(rf.predict - scaled_test$gross)^2)


