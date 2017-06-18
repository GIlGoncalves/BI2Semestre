movies<-read.csv("/Users/josepedromonteiro/GitHub/BI2Semestre/outMovieFinal.csv",header = T, stringsAsFactors = FALSE)


library(reshape)

## Issue 6: There are composite words in keyword and genre.
# Solution: Melt genre


# Split genre into a vector of genres
print("Split genre into a vector of genres.")
movies$genres <- strsplit(movies$genres, "|", fixed = TRUE)

# Melt genre
print("Melt genre.")
directorWithGenres <- melt(movies$genres)
colnames(directorWithGenres) <- c("genres", "director")

# Assign director name to molten genre
print("Assign director name to molten genre.")
i <- 1
for (x in directorWithGenres$director){
  directorWithGenres$director[i] <- movies$director_name[x]
  i <- i + 1
}

head(directorWithGenres)
## Issue 7: Column order in dataset is random.
# Solution: Rearrange column order in dataset.
print("Rearrange column order in dataset.")
dataset <- dataset[, c(12,28,2,5,22,10,17,24,3,13,19,26,20,21,23,9,27,1,4,11,8,7,25,15,6,14,16,18)]

print("Save the arranged dataset to a csv file.")
write.csv(dataset, file="rearrangedDf.csv")