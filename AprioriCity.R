setwd("C:/Users/Parikshita/Desktop/Data Science/SummerSemester/Practicum-CSP 572")

# write the path of the json files
filename = list.files(path = "C:\\Users\\Parikshita\\Desktop\\Data Science\\SummerSemester\\Practicum-CSP 572\\TripAdvisorJson\\json\\",pattern = ".json", full.names = TRUE)

# download libraries
install.packages("rjson")
install.packages("jsonlite")
library(rjson)
library(jsonlite)

# extract the data from the files
myfiles <- lapply(filename, function(x) fromJSON(x))

# set the city name
city <- "seattle"

# install required packages fr text analysis and extraction of address from html text
install.packages("RCurl")
install.packages("XML")
install.packages("tm")
library(tm)
library(RCurl)
library(XML)

# get length of extracted data
len_pages <- length(myfiles)

# extract content, hotel information and id in respective variables
hotel_info <- lapply(1:len_pages, function(i) myfiles[[i]]$HotelInfo)
hotel_address <- lapply(1:len_pages, function(i) hotel_info[[i]]$Address)
hotel_ID <- sapply(1:len_pages, function(i) hotel_info[[i]]$HotelID)

# address extraction
doc<- lapply(1:length(hotel_info), function(x) htmlParse(hotel_address[x], asText = TRUE))
#doc1 <- tolower(plain.text)
plain.text <- lapply(1:length(doc), function(x) xpathSApply(doc[[x]], "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue))
pattern <- lapply(1:length(doc), function(x) which(plain.text[[x]] %in% c(" ", ", ")))
address <- lapply(1:length(plain.text), function(i) {
  if(length(pattern[[i]]) != 0)
  {
    plain.text[[i]][-pattern[[i]]]
  }
  else
  {
    plain.text[[i]] <- plain.text[[i]]
  }
})
address <- lapply(1:length(address), function(i) tolower(address[[i]]))

# getting information of hotels only in "Seattle"
city_filter <- which(sapply(lapply(1:length(address), function(i) any(which(address[[i]] %in% city))), isTRUE))

# extracting hotel IDs of Seattle hotels
hotel_ID <- hotel_ID[city_filter]

# extracting content of hotels in seattle
file_index <- which(sapply(lapply(1:len_pages, function(i) any(which(myfiles[[i]]$HotelInfo$HotelID %in% hotel_ID))), isTRUE))
myfiles_city <- myfiles[file_index]


hotel_Content <- lapply(1:length(myfiles_city), function(i) myfiles_city[[i]]$Reviews$Content)
hotel_name <- lapply(1:length(myfiles_city), function(i) myfiles_city[[i]]$HotelInfo$Name)

# removal of url and email address from the string
rm_url_content <- lapply(1:length(hotel_Content), function(i) gsub("(http[^ ]*)", "",hotel_Content[[i]]))
rm_email_content <- lapply(1:length(rm_url_content), function(i) gsub("*@*", "",rm_url_content[[i]]))

# break the reviews into tokens using MC_tokenizer from library tm
tokenize_content <- lapply(1:length(hotel_Content), function(i) MC_tokenizer(rm_url_content[[i]]))
tokenize_content <- lapply(1:length(tokenize_content), function(i,j) (tokenize_content[[i]][tokenize_content[[i]][j] != ""]))
tokenize_content <- lapply(1:length(tokenize_content), function(i) tolower(tokenize_content[[i]]))

# break the hotel name into tokens using MC_tokenizer from library tm
tokenize_hotel <- lapply(1:length(hotel_name), function(i) MC_tokenizer(hotel_name[[i]]))
tokenize_hotel <- lapply(1:length(tokenize_hotel), function(i,j) (tokenize_hotel[[i]][tokenize_hotel[[i]][j] != ""]))
tokenize_hotel <- lapply(1:length(tokenize_hotel), function(i) tolower(tokenize_hotel[[i]]))

# get only Seattle hotels  address
address_city <- address[file_index]

# removing hotel name and address from the content
comm_index1 <- lapply(1:length(tokenize_content), function(i) which(tokenize_content[[i]] %in% tokenize_hotel[[i]]))
comm_index2 <- lapply(1:length(tokenize_content), function(i) which(tokenize_content[[i]] %in% address_city[[i]]))

# hotel name
tokenize_content <- lapply(1:length(tokenize_content), function(i) {
  if(length(comm_index1[[i]]) != 0)
  {
    tokenize_content[[i]][-comm_index1[[i]]]
  }
  else
  {
    tokenize_content[[i]] <- tokenize_content[[i]]
  }
})

# address
tokenize_content <- lapply(1:length(tokenize_content), function(i) {
  if(length(comm_index2[[i]]) != 0)
  {
    tokenize_content[[i]][-comm_index2[[i]]]
  }
  else
  {
    tokenize_content[[i]] <- tokenize_content[[i]]
  }
})

# remove stopwords from the list
stopwords <- stopwords(kind = "en")
stopwords1 <- read.csv(file = "stopwords1.csv", header = FALSE)
stop <- which(stopwords1[c(1:length(stopwords1)),1] %in% stopwords)
stopwords1 <- stopwords1[-stop,]
stopwords1 <- as.character(stopwords1)
stopwords <- c(stopwords, stopwords1)
write.csv(stopwords, "stopwords.csv", row.names=FALSE)

len_words <- length(stopwords)

words <- lapply(1:length(tokenize_content), function(i) which(tokenize_content[[i]] %in% stopwords))
tokenize_content <- lapply(1:length(tokenize_content), function(i) {
  if(length(words[[i]]) != 0)
  {
    tokenize_content[[i]][-words[[i]]]
  }
  else
  {
    tokenize_content[[i]] <- tokenize_content[[i]]
  }
})

# saving data to a new variable
tokens <- tokenize_content

# loading required libraries
installed.packages("arules")
library(arules)

# add transaction ID
tokens1 <- lapply(1:length(tokens), function(i) tokens[[i]][duplicated(tokens[[i]])==FALSE])
names(tokens1) <- paste("T", c(1:length(tokens1)), sep ="")
trans <- as(tokens1, "transactions")

# tm library has inspect() method 
# and in order to run inspect  we need to detach tm and get arules library again
detach(package:tm, unload=TRUE)
library(arules)

# run the rule apriori
#rules <- apriori(trans)
rules <- apriori(trans, parameter=list(minlen = 2, maxlen = 2, support=.15, confidence=.50))
rules
inspect(head(rules))