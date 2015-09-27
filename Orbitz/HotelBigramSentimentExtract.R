# installing required libraries
install.packages("stringr")
library(stringr)
library(tm)
library(plyr)

# read city_rating files 
city_files <- list.files(pattern = "*_city_rating.csv")

for (i in 1:length(city_files))
{
  # reading one city data at a time
  city_df <- read.csv(city_files[21], header = TRUE)
  
  city_name <- as.character(unique(city_df$City))
  
  # create directory to save bigram table of each hotel of the city
  dir.create(file.path("C:/Users/Parikshita/Desktop/Data Science/Summer 2015/SummerPracticum/Orbitz"
                       , city_name, warning = FALSE))
  
  dirC <- file.path("C:/Users/Parikshita/Desktop/Data Science/Summer 2015/SummerPracticum/Orbitz"
                    , city_name)
  
  #extracting unique names of the hotels within the city
  hotel_city <- as.character(unique(city_df$Hotel))
  
  for (j in 1:length(hotel_city))
  {
    hotel <- hotel_city[1]
    hotel_filter <- which(sapply(lapply(1:nrow(city_df), function(x) any(which(city_df$Hotel[x] %in% hotel)))
                                 , isTRUE))   
    
    hotel_rt <- city_df[hotel_filter,]
    
    # break the reviews into tokens using scan_tokenizer from library tm
    content_token <- lapply(1:length(hotel_rt), function(i) scan_tokenizer(hotel_rt$Content[[i]]))
    content_token <- lapply(1:length(content_token), function(i) tolower(content_token[[i]]))
    
    # removing first element of each row of content_token and adding "." at the end
    content_token2 <- lapply(1:length(content_token), function(x) content_token[[x]][-1])
    
    # creating bigrams using paste function
    content_bigram <- lapply(1:length(content_token), function(x) paste(content_token[[x]], content_token2[[x]]))
    
    # converting to data frame and setting column name
    uni_content_bigram <- as.character(unique(unlist(content_bigram)))
    
    # creating table bigram_hash which will contain count of each bigram for each rating
    bigram_hash <- data.frame(City = hotel, bigram = uni_content_bigram, rating_1 = 0, rating_2 = 0
                              , rating_3 = 0, rating_4 = 0, rating_5 = 0)

    # running loop to insert the values in the table bigram_hash
    for (x in 1:length(bigram_hash$bigram))
    {
      # saving bigram as a character
      uni_cb <- as.character(bigram_hash$bigram[x])
      
      # running loop for matching string with review rating 1
      #  r1 <- count(sapply(lapply(1:nrow(city_df), function(y) 
      #    (city_df$Rating[y] == 1 & str_detect(city_df$Content[y], uni_cb))), isTRUE))
      
      r1 <- count(sapply(lapply(1:nrow(city_df), function(y)
        (city_df$Rating[y] == 1 & str_count(city_df$Content[y], uni_cb))), isTRUE))
      
      #bigram_hash$rating_1[x] <- r1
      
      bigram_hash$rating_1[x] <- as.numericifelse(ifelse(length(r1$freq[r1$x == "TRUE"]) == 0, 0, r1$freq[r1$x == "TRUE"]))
      
      # running loop for matching string with review rating 2
      #r2 <- count(sapply(lapply(1:nrow(city_df), function(y) 
      #  (city_df$Rating[y] == 2 & str_detect(city_df$Content[y], uni_cb))), isTRUE))
      
      r2 <- count(sapply(lapply(1:nrow(city_df), function(y)
        (city_df$Rating[y] == 2 & str_count(city_df$Content[y], uni_cb))), isTRUE))
      
      #bigram_hash$rating_2[x] <- r2
      
      bigram_hash$rating_2[x] <- as.numericifelse(ifelse(length(r2$freq[r2$x == "TRUE"]) == 0, 0, r2$freq[r2$x == "TRUE"]))
      
      # running loop for matching string with review rating 3
      #r3 <- count(sapply(lapply(1:nrow(city_df), function(y) 
      #  (city_df$Rating[y] == 3 & str_detect(city_df$Content[y], uni_cb))), isTRUE))
      
      r3 <- count(sapply(lapply(1:nrow(city_df), function(y)
        (city_df$Rating[y] == 3 & str_count(city_df$Content[y], uni_cb))), isTRUE))
      
      #bigram_hash$rating_3[x] <- r3
      
      bigram_hash$rating_3[x] <- as.numericifelse(ifelse(length(r3$freq[r3$x == "TRUE"]) == 0, 0, r3$freq[r3$x == "TRUE"]))
      
      # running loop for matching string with review rating 4
      #r4 <- count(sapply(lapply(1:nrow(city_df), function(y) 
      #  (city_df$Rating[y] == 4 & str_detect(city_df$Content[y], uni_cb))), isTRUE))
      
      r4 <- count(sapply(lapply(1:nrow(city_df), function(y)
        (city_df$Rating[y] == 4 & str_count(city_df$Content[y], uni_cb))), isTRUE))
      
      #bigram_hash$rating_4[x] <- r4
      
      bigram_hash$rating_4[x] <- as.numericifelse(ifelse(length(r4$freq[r4$x == "TRUE"]) == 0, 0, r4$freq[r4$x == "TRUE"]))
      
      # running loop for matching string with review rating 5
      #r5 <- count(sapply(lapply(1:nrow(city_df), function(y) 
      #  (city_df$Rating[y] == 5 & str_detect(city_df$Content[y], uni_cb))), isTRUE))
      
      r5 <- count(sapply(lapply(1:nrow(city_df), function(y)
        (city_df$Rating[y] == 5 & str_count(city_df$Content[y], uni_cb))), isTRUE))
      
      #bigram_hash$rating_5[x] <- r5
        
      bigram_hash$rating_5[x] <- as.numeric(ifelse(length(r5$freq[r5$x == "TRUE"]) == 0, 0, r5$freq[r5$x == "TRUE"]))
    }
    
    # removing bigrams with total count = 0
    bigram_hash <- subset(bigram_hash, (bigram_hash$rating_1 + bigram_hash$rating_2 + 
                                          bigram_hash$rating_3 + bigram_hash$rating_4 + 
                                          bigram_hash$rating_5) > 0 )
    
    # calcualtion to get average rating for each biagram
    bigram_hash$AvgRating <- (((1 * bigram_hash$rating_1) + (2* bigram_hash$rating_2) + 
                                 (3 * bigram_hash$rating_3) + (4* bigram_hash$rating_4) + 
                                 (5 * bigram_hash$rating_5))/ (bigram_hash$rating_1 
                                                               + bigram_hash$rating_2 
                                                               + bigram_hash$rating_3
                                                               + bigram_hash$rating_4 
                                                               + bigram_hash$rating_5))
    
    # loop to calculate sentiment for each bigram
    bigram_hash$Sentiment <- lapply(1:nrow(bigram_hash), function(x) 
      ifelse(bigram_hash$AvgRating[x] < 3, "Negative",
             ifelse(bigram_hash$AvgRating[x] > 3, "Positive", "Neutral")))
    
    # writing csv for each hotel
    bigram_hash <- data.frame(lapply(bigram_hash, as.character), stringsAsFactors=FALSE)
    write.csv(bigram_hash, file.path(dirC, paste("hotel1", "bigram_hash.csv", sep = "_")), row.names = FALSE)
  }
}
