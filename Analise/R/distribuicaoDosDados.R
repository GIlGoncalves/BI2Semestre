setwd("cadeiras/bi/analise de dados/trabalho/")
ficheiro<-read.csv("outMovieFinal.csv",header = T)

for(i in 29:length(ficheiro)) {
  
  ficheiro[is.na(ficheiro[i]),i]<-0
  
}


ficheiro<-ficheiro[,c(-4,-16,-17,-18,-22,-27,-10)]

array<-which(ficheiro[2]=="", arr.ind = T)

ficheiro<-ficheiro[-array[,1],]



#fit0 <- lm(ficheiro[,8] ~ficheiro[,20] , data=ficheiro)
#summary(fit0) # show results

#plot(fit)

#dim(residuals(fit0))
#predict(fit)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


png("pairs3.png",width = 5080,height = 5080)
pairs(ficheiro,lower.panel = panel.smooth,upper.panel = panel.cor)
dev.off()
#################################################################################
####Cold deck->Retirar de uma base de dados#####################################
array<-which(ficheiro[1]=="", arr.ind = T)

for(i in 1:length(array[,"row"])){

  ficheiro[array[i,1],"color"]<-"Color"

}

################################################################################




ficheiro[is.na(ficheiro[3]),3]<--1


num_critic_for_reviews<-as.data.frame(table(ficheiro[3]),stringsAsFactors =FALSE)


num_critic_for_reviews1<-rbind(num_critic_for_reviews[num_critic_for_reviews$Var1==-1,],
                      nrow(num_critic_for_reviews[num_critic_for_reviews$Var1!=-1,]))

num_critic_for_reviews1[1,][1]<-"Desconhecidos"

png("num_critic_for_reviews.png",width = 1080,height = 1080)
barplot(num_critic_for_reviews1$Freq,names.arg = num_critic_for_reviews1$Var1,main="Criticas para revisao",xlab = "Criticas",col = c("yellow","blue"))
dev.off()



#################################################################################

ficheiro[is.na(ficheiro[4]),4]<- -1



director_facebook_likes<-as.data.frame(table(ficheiro[4]),stringsAsFactors =FALSE)

director_facebook_likes1<-rbind(director_facebook_likes[director_facebook_likes$Var1==-1,],
                                director_facebook_likes[director_facebook_likes$Var1==0,],
                               sum(director_facebook_likes[(director_facebook_likes$Var1!=-1 & director_facebook_likes$Var1!=0 ),]$Freq))




director_facebook_likes1[1,][1]<-"Desconhecidos"
director_facebook_likes1[2,][1]<-"Igual a zero"
director_facebook_likes1[3,][1]<-"Superiores a zero"

png("director_facebook_likes.png",width = 1080,height = 1080)
barplot(director_facebook_likes1$Freq,names.arg = director_facebook_likes1$Var1,main="Criticas para revisao",xlab = "Criticas",col = c("yellow","blue"))
dev.off()



###############################################################################

ficheiro[is.na(ficheiro[5]),5]<- -1

actor_3_facebook_likes<-as.data.frame(table(ficheiro[5]),stringsAsFactors =FALSE)

actor_3_facebook_likes1<-rbind(actor_3_facebook_likes[actor_3_facebook_likes$Var1==-1,],
                               actor_3_facebook_likes[actor_3_facebook_likes$Var1==0,],
                                sum(actor_3_facebook_likes[(actor_3_facebook_likes$Var1!=-1 & actor_3_facebook_likes$Var1!=0 ),]$Freq))




actor_3_facebook_likes1[1,][1]<-"Desconhecidos"
actor_3_facebook_likes1[2,][1]<-"Igual a zero"
actor_3_facebook_likes1[3,][1]<-"Superiores a zero"

png("actor_3_facebook_likes.png",width = 1080,height = 1080)
barplot(actor_3_facebook_likes1$Freq,names.arg = actor_3_facebook_likes1$Var1,main="Número de likes do actor 3",xlab = "Número",col = c("yellow","blue"))
dev.off()
###############################################################################################

actor_2_name<-as.data.frame(table(ficheiro[7]),stringsAsFactors = FALSE)
actor_2_name[1,][1]<-"Desconhecido"

actor_2_name1<-rbind(actor_2_name[actor_2_name$Var1=="Desconhecido",],
                               sum(actor_2_name[actor_2_name$Var1!="Desconhecido",]$Freq))




actor_2_name1[2,][1]<-"Restantes"


png("actor_2_name.png",width = 1080,height = 1080)
barplot(actor_2_name1$Freq,names.arg = actor_2_name1$Var1,main="Nome dos actores 2",xlab = "Nome",col = c("yellow","blue"))
dev.off()

###############################################################################





ficheiro[is.na(ficheiro[8]),8]<--1

actor_1_facebook_likes<-as.data.frame(table(ficheiro[8]),stringsAsFactors =FALSE)

actor_1_facebook_likes1<-rbind(actor_1_facebook_likes[actor_1_facebook_likes$Var1==-1,],
                               actor_1_facebook_likes[actor_1_facebook_likes$Var1==0,],
                               sum(actor_1_facebook_likes[(actor_1_facebook_likes$Var1!=-1 & actor_1_facebook_likes$Var1!=0 ),]$Freq))




actor_1_facebook_likes1[1,][1]<-"Desconhecidos"
actor_1_facebook_likes1[2,][1]<-"Igual a zero"
actor_1_facebook_likes1[3,][1]<-"Superiores a zero"

png("actor_1_facebook_likes.png",width = 1080,height = 1080)
barplot(actor_1_facebook_likes1$Freq,names.arg = actor_1_facebook_likes1$Var1,main="Número de likes do actor 1",xlab = "Número",col = c("yellow","blue"))
dev.off()


################################################################################

ficheiro[is.na(ficheiro[8]),8]<--1


gross<-as.data.frame(table(ficheiro[8]),stringsAsFactors =FALSE)



gross1<-rbind(gross[gross$Var1==-1,],
                      sum(gross[(gross$Var1!=-1),]$Freq))

gross1[1,][1]<-"Desconhecido"
gross1[2,][1]<-"Restantes"

png("gross.png",width = 1080,height = 1080)
barplot(gross1$Freq,names.arg = gross1$Var1,main="Dinheiro Gasto",xlab = "Valores",col = c("yellow","blue"))
dev.off()
#############################################################################

genres<-as.data.frame(table(ficheiro[9]),stringsAsFactors =FALSE)

png("genres.png",width = 1080,height = 1080)
barplot(genres$Freq,names.arg = genres$Var1,main="Genero dos filmes",xlab = "Genero",col = c("yellow","blue"))
dev.off()

################################################################################

actor_1_name <-as.data.frame(table(ficheiro[11]),stringsAsFactors =FALSE)

actor_1_name[1,][1]<-"Desconhecido"

actor_1_name1<-rbind(actor_1_name[actor_1_name$Var1=="Desconhecido",],
              sum(actor_1_name[(actor_1_name$Var1!="Desconhecido"),]$Freq))

actor_1_name1[1,][1]<-"Desconhecido"
actor_1_name1[2,][1]<-"Restantes"

png("actor_1_name.png",width = 1080,height = 1080)
barplot(actor_1_name1$Freq,names.arg = actor_1_name1$Var1,main="Nome dos actores 1",xlab = "Nomes",col = c("yellow","blue"))
dev.off()

################################################################################

movie_title <- as.data.frame(table(ficheiro[12]),stringsAsFactors =FALSE)

png("movie_title.png",width = 1080,height = 1080)
barplot(movie_title$Freq,names.arg = movie_title$Var1,main="Titulo dos filmes",xlab = "Titulo",col = c("yellow","blue"))
dev.off()
################################################################################

ficheiro[is.na(ficheiro[13]),13]<--1

num_voted_users<-as.data.frame(table(ficheiro[13]),stringsAsFactors =FALSE)


################################################################################
ficheiro[is.na(ficheiro[14]),14]<--1

cast_total_facebook_likes<-as.data.frame(table(ficheiro[14]),stringsAsFactors =FALSE)

cast_total_facebook_likes1<-rbind(cast_total_facebook_likes[cast_total_facebook_likes$Var1==0,],
                            sum(cast_total_facebook_likes[(cast_total_facebook_likes$Var1!=0),]$Freq))

cast_total_facebook_likes1[1,][1]<-"Zero"
cast_total_facebook_likes1[2,][1]<-"Restantes"

png("cast_total_facebook_likes.png",width = 1080,height = 1080)
barplot(cast_total_facebook_likes1$Freq,names.arg = cast_total_facebook_likes1$Var1,main="Total de like dos actores",xlab = "Valores",col = c("yellow","blue"))
dev.off()

################################################################################

actor_3_name<-as.data.frame(table(ficheiro[15]),stringsAsFactors =FALSE)

actor_3_name[1,][1]<-"Desconhecido"

actor_3_name1<-rbind(actor_3_name[actor_3_name$Var1=="Desconhecido",],
                     sum(actor_3_name[(actor_3_name$Var1!="Desconhecido"),]$Freq))

actor_3_name1[1,][1]<-"Desconhecido"
actor_3_name1[2,][1]<-"Restantes"

png("actor_3_name.png",width = 1080,height = 1080)
barplot(actor_3_name1$Freq,names.arg = actor_3_name1$Var1,main="Nome dos actores 3",xlab = "Nomes",col = c("yellow","blue"))
dev.off()

#################################################################################
ficheiro[is.na(ficheiro[19]),19]<--1


num_user_for_reviews<-as.data.frame(table(ficheiro[19]),stringsAsFactors =FALSE)

num_user_for_reviews1<-rbind(num_user_for_reviews[num_user_for_reviews$Var1==-1,],
              sum(num_user_for_reviews[(num_user_for_reviews$Var1!=-1),]$Freq))

num_user_for_reviews1[1,][1]<-"Desconhecido"
num_user_for_reviews1[2,][1]<-"Restantes"

png("num_user_for_reviews.png",width = 1080,height = 1080)
barplot(num_user_for_reviews1$Freq,names.arg = num_user_for_reviews1$Var1,main="Numero de utilizadores para revisao",xlab = "Valores",col = c("yellow","blue"))
dev.off()
#################################################################################

lingua<-as.data.frame(table(ficheiro[20]),stringsAsFactors = FALSE)
lingua[1,][1]<-"Desconhecida"

lingua1<-rbind(lingua[lingua$Var1=="Desconhecida",],
                     nrow(lingua[(lingua$Var1!="Desconhecida"),]))

lingua1[1,][1]<-"Desconhecida"
lingua1[2,][1]<-"Restantes"

png("lingua.png",width = 1080,height = 1080)
barplot(lingua1$Freq,names.arg = lingua1$Var1,main="Lingua dos filmes",xlab = "Lingua",col = c("yellow","blue"))
dev.off()


################################################################################

country<-as.data.frame(table(ficheiro[21]),stringsAsFactors = FALSE)
country[1,][1]<-"Desconhecido"


country1<-rbind(country[country$Var1=="Desconhecido",],
               nrow(country[(country$Var1!="Desconhecido"),]))

country1[1,][1]<-"Desconhecido"
country1[2,][1]<-"Restantes"

png("country.png",width = 1080,height = 1080)
barplot(country1$Freq,names.arg = country1$Var1,main="Paises dos filmes",xlab = "Paises",col = c("yellow","blue"))
dev.off()

#################################################################################
ficheiro[is.na(ficheiro[23]),23]<--1

budget<-as.data.frame(table(ficheiro[23]),stringsAsFactors = FALSE)


budget1<-rbind(budget[budget$Var1==-1,],
                             sum(budget[(budget$Var1!=-1),]$Freq))

budget1[1,][1]<-"Desconhecido"
budget1[2,][1]<-"Restantes"

png("budget.png",width = 1080,height = 1080)
barplot(budget1$Freq,names.arg = budget1$Var1,main="Despesas",xlab = "Valores",col = c("yellow","blue"))
dev.off()

#################################################################################
ficheiro[is.na(ficheiro[24]),24]<--1

title_year<-as.data.frame(table(ficheiro[24]),stringsAsFactors = FALSE)

title_year1<-rbind(title_year[title_year$Var1==-1,],
               sum(title_year[(title_year$Var1!=-1),]$Freq))

title_year1[1,][1]<-"Desconhecido"
title_year1[2,][1]<-"Restantes"

png("title_year.png",width = 1080,height = 1080)
barplot(title_year1$Freq,names.arg = title_year1$Var1,main="Anos dos filmes",xlab = "Anos",col = c("yellow","blue"))
dev.off()

#################################################################################
ficheiro[is.na(ficheiro[25]),25]<--1

actor_2_facebook_likes<-as.data.frame(table(ficheiro[25]),stringsAsFactors = FALSE)

actor_2_facebook_likes1<-rbind(actor_2_facebook_likes[actor_2_facebook_likes$Var1==-1,],
                               actor_2_facebook_likes[actor_2_facebook_likes$Var1==0,],
                               sum(actor_2_facebook_likes[(actor_2_facebook_likes$Var1!=-1 & actor_2_facebook_likes$Var1!=0 ),]$Freq))




actor_2_facebook_likes1[1,][1]<-"Desconhecidos"
actor_2_facebook_likes1[2,][1]<-"Igual a zero"
actor_2_facebook_likes1[3,][1]<-"Superiores a zero"

png("actor_2_facebook_likes.png",width = 1080,height = 1080)
barplot(actor_2_facebook_likes1$Freq,names.arg = actor_2_facebook_likes1$Var1,main="Número de likes do actor 2",xlab = "Número",col = c("yellow","blue"))
dev.off()

#################################################################################
ficheiro[is.na(ficheiro[26]),26]<--1

imdb_score<-as.data.frame(table(ficheiro[26]),stringsAsFactors = FALSE)


################################################################################
ficheiro[is.na(ficheiro[28]),28]<--1


movie_facebook_likes<-as.data.frame(table(ficheiro[28]),stringsAsFactors = FALSE)

movie_facebook_likes1<-rbind(movie_facebook_likes[movie_facebook_likes$Var1==0,],
                                  sum(movie_facebook_likes[(movie_facebook_likes$Var1!=0),]$Freq))

movie_facebook_likes1[1,][1]<-"Zero"
movie_facebook_likes1[2,][1]<-"Restantes"

png("cast_total_facebook_likes.png",width = 1080,height = 1080)
barplot(movie_facebook_likes1$Freq,names.arg = movie_facebook_likes1$Var1,main="Total de like do filme",xlab = "Valores",col = c("yellow","blue"))
dev.off()
##################################################################################
png("graficaoPequeno.png",width = 2080,height = 2080)

par(mfrow = c(2,2))

barplot(imdb_score$Freq,names.arg = imdb_score$Var1,main="Imdb_Score",xlab = "Raking")
barplot(title_year$Freq,names.arg = title_year$Var1,main="Title_year",xlab = "Ano")
barplot(gross$Freq,names.arg = gross$Var1,main="Gross",xlab = "Valor")
barplot(budget$Freq,names.arg = budget$Var1,main="Budget",xlab = "Despesa")
dev.off()

save.image("trabalho")







