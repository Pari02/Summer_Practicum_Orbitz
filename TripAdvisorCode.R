# https://www.youtube.com/watch?v=DQGJhZNhG4M
# http://r.789695.n4.nabble.com/How-to-extract-sublist-from-a-list-td3717451.html
# http://www.inside-r.org/packages/cran/tm/docs/MC_tokenizer

install.packages("rjson")

library("rjson")
install.packages("Matrix")
install.packages("arules")
library(arules)

# set working directory where the file is saved
setwd("C:/Users/Parikshita/Desktop/Data Science/SummerSemester/Practicum-CSP 572/TripAdvisorJson/json")
oldwd <- getwd()

json_file <- 'C:\\Users\\Parikshita\\Desktop\\Data Science\\SummerSemester\\Practicum-CSP 572\\TripAdvisorJson\\json\\72572.json'
json_data <- fromJSON(file=json_file)

# getting number of rows for reviews
len_json_data <- length(json_data[[1]])

# getting data in content
content <- lapply(1:len_json_data, function(i) json_data$Reviews[[i]]["Content"])
df_content <- data.frame(matrix(unlist(content)), stringsAsFactors = FALSE)
colnames(df_content) <- c('content')
df_content <- lapply(1:len_json_data, function(i,j) tolower(df_content[i,j]))

hotel <- data.frame(hotelName = json_data$HotelInfo$Name)
hotel <- tolower(hotel$hotelName)

library(tm)

# find total length of the file
len_json_data <- length(json_data[[1]])

# break the reviews into tokens using MC_tokenizer from library tm
# unlist it and convert it into a dataframe
tokenize_content <- lapply(1:len_json_data, function(i) MC_tokenizer(df_content[i]))
tokenize_content <- lapply(1:len_json_data, function(i,j) (tokenize_content[[i]][tokenize_content[[i]][j] != ""]))
tokenize_content <- data.frame(matrix(unlist(tokenize_content)), stringsAsFactors = FALSE)

# break the hotel name into tokens using MC_tokenizer from library tm
# unlist it and convert it into a dataframe
tokenize_hotel <- MC_tokenizer(hotel[1])
tokenize_hotel <- data.frame(matrix(unlist(tokenize_hotel)), stringsAsFactors = FALSE)

# set the colnames
colnames(tokenize_content) <- 'Content'
colnames(tokenize_hotel) <- 'hotel'

# get the number of rows for review tokens and hotel 
len_content <- nrow(tokenize_content)
len_hotel <- nrow(tokenize_hotel)

# extract the indexes from the tokenize_content matching the hotel name
# remove the indexes from tokenize_content
common_index <- which(tokenize_content[c(1:len_content),1] %in% tokenize_hotel[c(1:len_hotel),1])
tokenize_content <- tokenize_content[-common_index, 1]
tokenize_content <- data.frame(matrix(unlist(tokenize_content)), stringsAsFactors = FALSE)

# remove stopwords from the list
stopwords <- stopwords(kind = "en")
stopwords <- data.frame(matrix(unlist(stopwords)), stringsAsFactors = FALSE)
colnames(stopwords) <- 'words'

len_content <- nrow(tokenize_content)
len_words <- nrow(stopwords)

words <- which(tokenize_content[c(1:len_content),1] %in% stopwords[c(1:len_words),1])
tokenize_content <- tokenize_content[-words, 1]
tokenize_content <- data.frame(matrix(unlist(tokenize_content)), stringsAsFactors = FALSE)
colnames(tokenize_content) <- 'Content'

# convert the list to a factor and sort it before
tokenize_content[,1] <- tokenize_content[sort.list(tokenize_content[,1]),1]

# convert the tokenize_content into a factor
trans <- data.frame(content = as.factor(tokenize_content$Content))

# Convert trans into transactions for applying apriori
trans1 <- as(trans, "transactions")

# run the rule apriori
rules <- apriori(trans1)
inspect(rules)

#install.packages("NLP")
#library(NLP)
#words(df_content[1,1])
#tokenize_hotel[1]
#tokenize_content[[1]][]
