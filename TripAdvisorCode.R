# https://www.youtube.com/watch?v=DQGJhZNhG4M
# http://r.789695.n4.nabble.com/How-to-extract-sublist-from-a-list-td3717451.html
# http://www.inside-r.org/packages/cran/tm/docs/MC_tokenizer
# http://www.r-bloggers.com/htmltotext-extracting-text-from-html-via-xpath/
# http://web.ydu.edu.tw/~alan9956/docu/refer/R06_AssociationRules.pdf

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
setwd("C:/Users/Parikshita/Desktop/Data Science/SummerSemester/Practicum-CSP 572")

# getting number of rows for reviews
len_json_data <- length(json_data[[1]])

# getting data in content
content <- lapply(1:len_json_data, function(i) json_data$Reviews[[i]]["Content"])
df_content <- data.frame(matrix(unlist(content)), stringsAsFactors = FALSE)
colnames(df_content) <- c('content')
df_content <- lapply(1:len_json_data, function(i,j) tolower(df_content[i,j]))

# do the same to get hotel data
# later this information will be used to remove it from the content
# to avoid discrepancies in the accuracy of the result 
hotel <- data.frame(hotelName = json_data$HotelInfo$Name)
hotel <- tolower(hotel$hotelName)

# extract address from html 
install.packages("RCurl")
install.packages("XML")
library(tm)
library(RCurl)
library(XML)

doc <- htmlParse(json_data$HotelInfo$Address, asText = TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
pattern <- which(plain.text[c(1:length(plain.text))] %in% c(" ", ", "))
address <- plain.text[-pattern]
address <- tolower(address)

# break the reviews into tokens using MC_tokenizer from library tm
tokenize_content <- lapply(1:len_json_data, function(i) MC_tokenizer(df_content[i]))
tokenize_content <- lapply(1:len_json_data, function(i,j) (tokenize_content[[i]][tokenize_content[[i]][j] != ""]))

# break the hotel name into tokens using MC_tokenizer from library tm
tokenize_hotel <- MC_tokenizer(hotel)
tokenize_hotel <- c(tokenize_hotel, address)

# get the number of rows for review tokens and hotel 
len_content <- length(tokenize_content)
len_hotel <- length(tokenize_hotel)

# extract the indexes from the tokenize_content matching the hotel name
# remove the indexes from tokenize_content
common_index <- lapply(1:len_content, function(i) which(tokenize_content[[i]] %in% tokenize_hotel))
tokenize_content <- lapply(1:len_content, function(i) {
  if(length(common_index[[i]]) != 0)
  {
    tokenize_content[[i]][-common_index[[i]]]
  }
  else
  {
    tokenize_content[[i]] <- tokenize_content[[i]]
  }
  })

# remove stopwords from the list
# as i downloaded the stopwords and then added them to the 
# stopwords pre -installed in the package
# you can just use the stopwords of the attached stopwords.csv file
# and run read.csv to upload 
stopwords <- stopwords(kind = "en")
stopwords1 <- read.csv(file = "stopwords1.csv", header = FALSE)
stop <- which(stopwords1[c(1:length(stopwords1)),1] %in% stopwords)
stopwords1 <- stopwords1[-stop,]
stopwords1 <- as.character(stopwords1)
stopwords <- c(stopwords, stopwords1)
write.csv(stopwords, "stopwords.csv", row.names=FALSE)

len_words <- length(stopwords)
words <- lapply(1:len_content, function(i) which(tokenize_content[[i]] %in% stopwords))
tokenize_content <- lapply(1:len_content, function(i) {
  if(length(words[[i]]) != 0)
  {
    tokenize_content[[i]][-words[[i]]]
  }
  else
  {
    tokenize_content[[i]] <- tokenize_content[[i]]
  }
})

tokens <- tokenize_content

# remove duplicate values in the list and add transaction ID
# convert it to transactions
tokens1 <- lapply(1:len_content, function(i) tokens[[i]][duplicated(tokens[[i]])==FALSE])
names(tokens1) <- paste("T", c(1:len_content), sep ="")
trans <- as(tokens1, "transactions")

# create image and generate summary of data
image(trans)
summary(trans)

# find itemFrequency and the frequency/ support
itemFrequency(trans, type = "relative")
itemFrequency(trans, type = "absolute")

# create itemFrequecy plot
itemFrequencyPlot(trans)

# tm library has inspect() method 
# and in order to run inspect  we need to detach tm and get arules library again
detach(package:tm, unload=TRUE)
library(arules)

# run the rule apriori
rules <- apriori(trans)
rules <- apriori(trans, parameter=list(support=0.15, confidence=.65))
inspect(rules)
