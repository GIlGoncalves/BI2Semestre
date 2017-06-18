movies<-read.csv("/Users/josepedromonteiro/GitHub/BI2Semestre/outMovieFinal.csv",header = T, stringsAsFactors = FALSE)

library(reshape) # melt
library(Hmisc) # impute



## Step 1: Get directors with gross and molten genres
##
## First dataframe: directorWithGenres
## genres | director | gross
##

# Split genre into a vector of genres
print("Split genre into a vector of genres.")
movies$genres <- strsplit(movies$genres, "|", fixed = T)

# Melt genre
print("Melt genre.")
directorWithGenres <- melt(movies$genres)
colnames(directorWithGenres) <- c("genres", "director")

# Assign director name and gross to molten genre
print("Assign director name and gross to molten genre.")
directorWithGenres['gross'] <- as.numeric(0)
i <- 1
for (x in directorWithGenres$director){
  directorWithGenres$director[i] <- movies$director_name[x]
  directorWithGenres$gross[i] <- movies$gross[x]
  i <- i + 1
}

head(directorWithGenres)
# Replace empty gross with 0 using imputation and remove rows with NAs
print("Replace empty gross with 0 using imputation and remove rows with NAs.")
directorWithGenres$gross <- impute(directorWithGenres$gross, 0)
directorWithGenres <- na.omit(directorWithGenres)

## Step 2: Get top 20 directors with highest gross, their top genre 
## (can be composite due to same highest frequency) and its frequency
##
## Second data frame: topDirector
## director | total gross | top genre | genre count
##

# Create a data frame with 4 columns: 
# director, total gross, top genre and genre count.
print("Create a data frame with 4 columns: director, total gross, top genre and genre count.")
topDirector <- data.frame(director = unique(directorWithGenres$director), total_gross = 0, top_genre = "", genre_count = 0, stringsAsFactors = F)

# Compute and assign total gross for each director
print("Compute and assign total gross for each director.")
i <- 1
for (name in topDirector$director){
  topDirector$total_gross[i] <- sum(directorWithGenres$gross[directorWithGenres$director == name])
  i <- i + 1
}

# Get the top 10 directors with the highest total gross
print("Get the top 10 directors with the highest total gross")
topDirector <- topDirector[order(-topDirector$total_gross),][1:20,]

# For each top 10 directors, assign genre with highest frequency and its frequency
print("For each top 10 directors, assign genre with highest frequency and its frequency.")
i <- 1
for (dir in topDirector$director){
  # Get a top 10 director with his/her genres 
  aDirectorWithGenres <- directorWithGenres[directorWithGenres$director == dir,]
  
  # Get genres with their frequency
  genreWithCount <- data.frame(table(aDirectorWithGenres$genres))
  
  # Get the genre with highest frequency
  maxGenreCount <- max(genreWithCount$Freq)
  genreWithMaxCount <- as.character(genreWithCount$Var1[genreWithCount$Freq == maxGenreCount])
  
  # When there are multiple genres with the same highest frequency, 
  # concatenate the genres
  if (length(genreWithMaxCount) > 1){
    concatGenre <- NULL
    for (genre in genreWithMaxCount){
      concatGenre <- paste(concatGenre, genre, sep = "|")
    }
    genreWithMaxCount <- substring(concatGenre, 2) # Remove | in first element
  }
  
  # Assign genre with highest frequency and its frequency to the data frame
  topDirector$top_genre[i] <- genreWithMaxCount 
  topDirector$genre_count[i] <- maxGenreCount
  i <- i + 1
}

topDirector

## Step 3: Get mean gross for top genre
##
## Third data frame: meanGrossForGenre
## genre | director | genre count | mean gross
##

# Create a data frame with top 20 directors' genres
print("Create a data frame with top 20 directors' genres.")
meanGrossForGenre <- data.frame(genre = topDirector$top_genre,  stringsAsFactors = F)

# Split genre into a vector of genres
print("Split genre into a vector of genres.")
meanGrossForGenre$genre <- strsplit(meanGrossForGenre$genre, split = "|", fixed = T)

# Melt genre
print("Melt genre.")
meanGrossForGenre <- melt(meanGrossForGenre$genre)
colnames(meanGrossForGenre) <- c("genre", "director")

# Assign director and genre count
print("Assign director and genre count.")
meanGrossForGenre['genre_count'] <- as.numeric(0)
i <- 1
for (x in meanGrossForGenre$director){
  meanGrossForGenre$director[i] <- topDirector$director[x]
  meanGrossForGenre$genre_count[i] <- topDirector$genre_count[x]
  i <- i + 1
}


head(meanGrossForGenre)

# Compute and assign mean gross
print("Compute and assign mean gross.")
meanGrossForGenre['mean_gross'] <- as.numeric(0)
for(i in 1:nrow(meanGrossForGenre)){
  # Subset by director and then by genre
  genreWithGross <- directorWithGenres[directorWithGenres$director == meanGrossForGenre$director[i],]  
  genreWithGross <- genreWithGross[genreWithGross$genres == as.character(meanGrossForGenre$genre[i]), ]
  
  meanGrossForGenre$mean_gross[i] <- mean(genreWithGross$gross)
}

meanGrossForGenre

## Step 4: Plot number of genres agaisnt genre count for top 20 profitable 
## directors
##

# Rearrange columns and round mean gross to zero decimal places.
print("Rearrange columns and round mean gross to zero decimal places.")
meanGrossForGenre <- meanGrossForGenre[,c(3,1,2,4)]
meanGrossForGenre$mean_gross <- round(meanGrossForGenre$mean_gross, 0)

library(ggplot2)
# Plot of number of genres agaisnt genre count for top 20 profitable directors
print("Plot of number of genres agaisnt genre count for top 20 profitable directors.")
ggplot(meanGrossForGenre, aes(x = genre_count)) + 
  #colorFacet("blue") +
  facet_wrap(~director + mean_gross, labeller = label_context) +
  geom_bar(aes(fill = genre)) +
  xlab("Número de Filmes por genereo") + 
  ylab("Número de Generos por diretos") +
  ggtitle("Top 20 Diretores mais rentáveis") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

