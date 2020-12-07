library(readr)
library(tidyverse)
library(tm)
library(corpus)
library(SnowballC)
library(openxlsx)
library(tidytext)
library(wordcloud)
library(widyr)
library(influential)
library(ggraph)
library(dplyr)
library(rvest)
library(httr)

rm(list = ls())

# Scraping Trustpilot to get the data--------------------------------------------------

path <- "https://dk.trustpilot.com/review/www.whiteaway.com"
master_df <- data.frame(matrix(NA,0,3)) %>% `colnames<-`(c("title","text","stars"))
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
prop_table <- data.frame(
  perc = c(0.05,0.03,0.04,0.11,0.77)
)


for (i in 1:5) {
  timeslot <- rnorm(1,1)
  Sys.sleep(ifelse(timeslot < 0, 1, timeslot))
  n_path <- paste0("https://dk.trustpilot.com/review/www.whiteaway.com?page=1&stars=",i)
  
  n_max <- prop_table$perc[i]*html_session(n_path, ua) %>% html_nodes("span.headline__review-count") %>% html_text() %>% gsub(pattern = "\\.",replacement = "") %>% as.numeric()
  
  n <- ceiling(n_max/20)
  for (j in 1:n) {
    timeslot <- rnorm(1,1)
    Sys.sleep(ifelse(timeslot < 0, 1, timeslot))
    path_read <- paste0("https://dk.trustpilot.com/review/www.whiteaway.com?page=",j,"&stars=",i)
    
    rev_url <- paste0(
      "https://dk.trustpilot.com/",
      html_session(path_read,ua) %>% html_nodes("a.link.link--large.link--dark") %>% html_attr("href")
    )
    
    for (x in rev_url) {
      timeslot <- rnorm(1,1)
      Sys.sleep(ifelse(timeslot < 0, 1, timeslot))
      title <- html_session(x,ua) %>% html_nodes("a.link.link--large.link--dark") %>% html_text()
      if (length(title) == 0) {title = 0}
      text <- html_session(x,ua) %>% html_nodes("p.review-content__text") %>% html_text()
      if (length(text) == 0) {text = NA}
      
      df <- data.frame(title = title,
                       text = text,
                       stars = i)
      master_df <- rbind(master_df,df)
      
    }
    
  }
  
  print(paste0(i," star(s) finished...."))
  
}

openxlsx::write.xlsx(master_df,"master_wag.xlsx")
hist(master_df$stars)


#Data cleaning--------------------------------------------------

rm(list = ls())

#To start the datacleaning I import the dataset in a xlsx format.
master_df_1 <- openxlsx::read.xlsx("C:/Users/Aksel Roald/OneDrive - Aarhus universitet/5. semester/Cultural data science/master_wag.xlsx")

# I then create a new coumn wich merges title and text
master_df_1$title_and_text <- paste0(master_df_1$title,", ",master_df_1$text)


# I inspect the data, by for example creating a histogram of the distribution and the str-function
hist(master_df_1$stars,xlab = "Stars",main = "Distribution of Stars",labels = T,breaks = c(0,1,2,3,4,5))
table(master_df_1$stars)
str(master_df_1)
glimpse(master_df_1)


# All dupicated data in the dataset are the removed
master_df_unique <- distinct(master_df_1, title_and_text, .keep_all = T)


# Check reviews with missing title
master_title <- master_df_unique %>% 
  subset(is.na(title))

hist(master_title$stars,breaks = 5)

# Check reviews with missing text
master_text <- master_df_unique %>% 
  subset(is.na(text))

# Check reviews with missing title and text
master_title_text <- master_df_unique %>% 
  subset(is.na(title) & is.na(text))

# Remove the observation where title and text is missing
master_filtered <- master_df_unique %>% 
  subset(title_and_text != "NA, NA")

# Remove hardcoded NA's from title_and_text
master_filtered$title_and_text <- gsub(pattern = "NA",replacement = "",master_filtered$title_and_text)


# Prepare for statistics --------------------------------------------------
#remove capital letters
master <- master_filtered
master$text <-  tolower(c(master$text))
master$title <- tolower(c(master$title))
master$title_and_text <-  tolower(c(master$title_and_text))
master$X <- rownames(master)

#All Danish characters and non-alphanumeric characters are replaced
#All words containing only a single character are removed.
#This is to remove any new nords that may have arised from removing
#all non-alphanumeric characters
for (i in c(1,2,4)) {
  master[,i] = str_replace_all(master[,i], "[Åå]", "aa")
  master[,i] = str_replace_all(master[,i], "[Øø]", "o")
  master[,i] = str_replace_all(master[,i], "[Ææ]", "ae")
  master[,i] = str_replace_all(master[,i], "[^[:alnum:]]", " ")
  master[,i] = gsub(" *\\b[[:alpha:]]{1}\\b *", " ", master[,i])
}

# A list of new stop words are created
remove_list <- c("paa", "saa", "kan", "fik", "faa", "gang", "igen", "ved", "kobt", "faar", "vaer", "naar", "ogsaa", "whiteaway","vaere")

# A loop is created so that the words in the stop word list are replaced with blanc spaces
for (i in c(1,2,4)) {
  for (j in remove_list) {
    master[,i] <- gsub(pattern = j,replacement = "",x = master[,i])
  }
  
# Words similar to "delivery" and "order" are manually stemmed
stem_list_levering <- c("levering","leveret","leveringen","levere","lever","levers")
stem_list_bestilt <- c("bestille","bestil","bestilt","bestilte","bestilling")

  for (x in stem_list_bestilt) {
    master[,i] <- gsub(pattern = x,replacement = "bestil",x = master[,i])
  }
  for (y in stem_list_levering) {
    master[,i] <- gsub(pattern = y,replacement = "lever",x = master[,i])
  }
}


# Most frequent word analysis ---------------------------------------------

word_master <- master %>%
  unnest_tokens(word, title_and_text, token = "ngrams", n = 1) %>% 
  subset(!word %in% stopwords("danish")) %>%
  subset(!word %in% remove_list) %>% 
  subset(!grepl(" *\\b[[:alpha:]]{1}\\b *", word)) %>% 
  mutate(word = SnowballC::wordStem(word,language = "danish")) %>% 
  count(word, sort = TRUE)

word_one <- master %>%
  subset(stars == 1) %>% 
  unnest_tokens(word, title_and_text, token = "ngrams", n = 1) %>% 
  subset(!word %in% stopwords("danish")) %>%
  subset(!word %in% remove_list) %>% 
  subset(!grepl(" *\\b[[:alpha:]]{1}\\b *", word)) %>% 
  mutate(word = SnowballC::wordStem(word,language = "danish")) %>% 
  count(word, sort = TRUE)


word_two <- master %>%
  subset(stars == 2) %>% 
  unnest_tokens(word, title_and_text, token = "ngrams", n = 1) %>% 
  subset(!word %in% stopwords("danish")) %>%
  subset(!word %in% remove_list) %>% 
  subset(!grepl(" *\\b[[:alpha:]]{1}\\b *", word)) %>% 
  mutate(word = SnowballC::wordStem(word,language = "danish")) %>% 
  count(word, sort = TRUE)


word_three <- master %>%
  subset(stars == 3) %>% 
  unnest_tokens(word, title_and_text, token = "ngrams", n = 1) %>% 
  subset(!word %in% stopwords("danish")) %>%
  subset(!word %in% remove_list) %>% 
  subset(!grepl(" *\\b[[:alpha:]]{1}\\b *", word)) %>% 
  mutate(word = SnowballC::wordStem(word,language = "danish")) %>% 
  count(word, sort = TRUE)


word_four <- master %>%
  subset(stars == 4) %>% 
  unnest_tokens(word, title_and_text, token = "ngrams", n = 1) %>% 
  subset(!word %in% stopwords("danish")) %>%
  subset(!word %in% remove_list) %>% 
  subset(!grepl(" *\\b[[:alpha:]]{1}\\b *", word)) %>% 
  mutate(word = SnowballC::wordStem(word,language = "danish")) %>% 
  count(word, sort = TRUE)


word_five <- master %>%
  subset(stars == 5) %>% 
  unnest_tokens(word, title_and_text, token = "ngrams", n = 1) %>% 
  subset(!word %in% stopwords("danish")) %>%
  subset(!word %in% remove_list) %>% 
  subset(!grepl(" *\\b[[:alpha:]]{1}\\b *", word)) %>% 
  mutate(word = SnowballC::wordStem(word,language = "danish")) %>% 
  count(word, sort = TRUE)


# I then create bar plots with the 15 most frequent words from each stars
par(mfrow=c(2,3))
barplot(word_one[1:15,]$n, las = 2, names.arg = word_one[1:15,]$word,
        col ="red", main ="Most frequent words, Stars = 1",
        ylab = "Word frequencies")
barplot(word_two[1:15,]$n, las = 2, names.arg = word_two[1:15,]$word,
        col ="red", main ="Most frequent words, Stars = 2",
        ylab = "Word frequencies")
barplot(word_three[1:15,]$n, las = 2, names.arg = word_three[1:15,]$word,
        col ="red", main ="Most frequent words, Stars = 3",
        ylab = "Word frequencies")
barplot(word_four[1:15,]$n, las = 2, names.arg = word_four[1:15,]$word,
        col ="red", main ="Most frequent words, Stars = 4",
        ylab = "Word frequencies")
barplot(word_five[1:15,]$n, las = 2, names.arg = word_five[1:15,]$word,
        col ="red", main ="Most frequent words, Stars = 5",
        ylab = "Word frequencies")
dev.off()


# We create a word cloud
set.seed(1234) # setting the seed increases replication and FAIR principles as we remove stocasticity and makes it possible for future researchers to obtain same results.
par(mfrow=c(1,1))
wordcloud(words = word_master$word, freq = word_master$n, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()



