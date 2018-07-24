library(dplyr)

#import ngram tables
fr1 <- read.table("data/fr1.txt", 
                  header = TRUE, sep="\t") 
fr2 <- read.table("data/fr2.txt", 
                  header = TRUE, sep="\t") 
fr3 <- read.table("data/fr3.txt", 
                  header = TRUE, sep="\t") 
fr4 <- read.table("data/fr4.txt", 
                  header = TRUE, sep="\t")
fr5 <- read.table("data/fr5.txt", 
                  header = TRUE, sep="\t")

prediction_table <- rbind(fr1,fr2,fr3, fr4, fr5)
prediciton_table <- arrange(prediction_table, p)
prediction_table$word <- as.character(prediction_table$word)


#start of 5gram in table
s5 <- length(prediction_table$word) - length(fr5$word) + 1 
#start of 4gram in table
s4 <- s5 - length(fr4$word) 
#start of 3gram in table
s3 <- s4 - length(fr3$word) 
#start of 2 gram in table
s2 <- s3 - length(fr2$word) 


#word prediction function
word_predict <- function(phrase) {
  phrase <- tolower(phrase)
  phrase <- gsub("'", " ", phrase)
  phrase <- gsub("[[:punct:]]", "", phrase)
  phrase <- tail(strsplit(phrase, " ")[[1]], 4) #make 4gram to search
  phrase <- paste(phrase, "")
  phrase <- paste(gsub(" ", "_", phrase), collapse = "")
  phrase <- paste("^", phrase, sep = "")
  word <- grep(phrase, prediction_table$word[s5:length(prediction_table$word)],
               value = TRUE) #find 4gram in word table
  words <- NULL
  if(length(word) > 0) {
    for (i in 1:length(word)){
      words <- c(words, tail(strsplit(word, "_")[[i]], 1))
      order <- prediction_table$p[
        grep(phrase, prediction_table$word[s5:length(prediction_table$word)])]
    }
  } else {
    phrase <- tail(strsplit(phrase, "_")[[1]], 3) #make 3gram to search
    phrase <- paste(phrase, "")
    phrase <- paste(gsub(" ", "_", phrase), collapse = "")
    phrase <- paste("^", phrase, sep = "")
    word <- grep(phrase, prediction_table$word[s4:s5], value = TRUE)
    if (length(word) > 0) {
      for (i in 1:length(word)){
        words <- c(words, tail(strsplit(word, "_")[[i]], 1)) #find 3gram in word table
        order <- prediction_table$p[grep(phrase, prediction_table$word[s4:s5])]
        }
    } else {
      phrase <- tail(strsplit(phrase, "_")[[1]], 2) #make 2 gram to search
      phrase <- paste(phrase, "")
      phrase <- paste(gsub(" ", "_", phrase), collapse = "")
      phrase <- paste("^", phrase, sep = "")
      word <- grep(phrase, prediction_table$word[s3:s4], value = TRUE)
      if(length(word) > 0){
        for (i in 1:length(word)){
          words <- c(words, tail(strsplit(word, "_")[[i]], 1)) #find 2 gram in word table
          order <- prediction_table$p[grep(phrase, prediction_table$word[s3:s4])]
          }
      } else { 
        phrase <- tail(strsplit(phrase, "_")[[1]], 1) #make unigram to search
        phrase <- paste(phrase, "")
        phrase <- paste(gsub(" ", "_", phrase), collapse = "")
        phrase <- paste("^", phrase, sep = "")
        word <- grep(phrase, prediction_table$word[s2:s3], value = TRUE)
        if(length(word) > 0){
        for (i in 1:length(word)){
          words <- c(words, tail(strsplit(word, "_")[[i]], 1)) #find unigram in word table
          order <- prediction_table$p[grep(phrase, prediction_table$word[s2:s3])]
        }
        }
      }
    }
    
  }
  
  prediction <- as.data.frame((cbind(words, order)))
  prediction <- arrange(prediction, desc(order))
  print(head(as.character(prediction$word), 1))
}
library(ggplot2)
qplot(data = prediction, aes(x = prediction$word[1:5], y = prediction$p[1:5], geom = barplot)
)
