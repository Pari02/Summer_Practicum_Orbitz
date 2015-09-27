# installing required libraries
install.packages("rjson")
install.packages("jsonlite")
install.packages("hydroTSM")
install.packages("RCurl")
install.packages("XML")
install.packages("tm")
install.packages("qdap")
library(rjson)
library(jsonlite)
library(hydroTSM)
library(plyr) 
library(RCurl)
library(XML)
library(tm)
library(qdap)

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "Summer 2015")
dirS <- file.path(dirP, "SummerPracticum", "USHotelCityRatingPlots") # cityRating for csv's
path <- file.path(dirP, "Practicum-CSP 572", "TripAdvisorJson", "json") 

# extract the data from the files
filename <- list.files(path, pattern = ".json", full.names = TRUE)

# extracting all the files
all_data <- lapply(filename, function(x) fromJSON(x))

# extracting cities and countries name
all_address <- lapply(1:length(all_data), function(i) all_data[[i]]$HotelInfo$Address)
all_doc <- lapply(1:length(all_address), function(x) htmlParse(all_address[x], asText = TRUE))

all_city <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//span[@property='v:locality']", xmlValue))
all_country <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//span[@property='v:country-name']", xmlValue))
all_country <- unique(subset(all_country, lapply(1:length(all_country), function(x) length(all_country[[x]])) > 0))

# extracting address of all the files
plain.text <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue))
pattern <- lapply(1:length(all_doc), function(x) which(plain.text[[x]] %in% c(" ", ", ")))
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

# creating directory where output will be saved
#dir.create(file.path(dirS, "Word"), showWarnings = FALSE)

# removing address of hotels which are not in United States
country_filter <- which(sapply(lapply(1:length(address), function(x) any(which(address[[x]] %in% all_country))), isTRUE))  
address <- address[-country_filter]
address <- lapply(1:length(address), function(i) tolower(address[[i]]))

# extracting rating & review date for all the hotels
all_rating <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Ratings$Overall)
rating_all <- all_rating[-country_filter]
all_hotel <- lapply(1:length(all_data), function(i) all_data[[i]]$HotelInfo$Name)
hotels <- all_hotel[-country_filter]
all_content <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Content)
content <- all_content[-country_filter]

content_new <- do.call(rbind.data.frame, content)

# extarcting us_cities
us_cities <- all_city[-country_filter]
cities <- unique(subset(us_cities, lapply(1:length(us_cities), function(x) length(us_cities[[x]])) > 0))


# write city names in CSv
write.csv(as.data.frame(cities), "us_cities.csv")

for(i in 1:length(cities))
{
  
  #dir.create(file.path(dirS), showWarnings = FALSE)
  city <- tolower(cities[[i]])
  
  # getting only those indexes which have city name in them
  city_filter <- which(sapply(lapply(1:length(address), function(x) any(which(address[[x]] %in% city)))
                              , isTRUE))  
  
  if (length(city_filter) >= 30)
  {
    # using the index field to get rating, review date and content information
    rating_city <- rating_all[city_filter]
    hotelName <- hotels[city_filter]
    content_city <- content[city_filter]
    address_city <- address[city_filter]
    
    # combining city name, review date and rating
    city_rating <- lapply(1:length(city_filter), function(x) cbind(hotelName[[x]], rating_city[[x]]
                                                                   , content_city[[x]]))
    
    # command to create flatlist in R by row 
    city_rating <- do.call(rbind.data.frame, city_rating)
    city_rating <- cbind(city, city_rating)
    
    # assigning names to columns
    colnames(city_rating) <- c("City", "Hotel", "Rating", "Content")
    
    # as.character(f) requires a "primitive lookup" to find the function as.character.factor()
    #, which is defined as as.numeric(levels(f))[f]
    city_rating$Rating <- as.numeric(levels(city_rating$Rating))[city_rating$Rating]
    
    # removal of url, email address, numbers, punctuation and special characters from the string
    city_rating$Content <- lapply(1:nrow(city_rating), function(x) 
      gsub("(http[^ ]*)|[[:punct:]]|*[[:digit:]]+|[][!#$%()*,.:;<=>@^_`|~.{}]", "", city_rating$Content[[x]]))
    
    # remove stopwords from the list
    stopwords <- read.csv(file = "stopwords.csv", header = FALSE)
    stopwords <- as.character(stopwords$V1)
    
    city_rating$Content <- lapply(1:nrow(city_rating), function(x) 
      rm_stopwords(city_rating$Content[[x]], stopwords = stopwords, separate = FALSE))
    
    # break the hotel name into tokens using scan_tokenizer from library tm
    hotel_token <- lapply(1:length(city_rating$Hotel), function(x) 
      scan_tokenizer(unique(city_rating$Hotel[x])))
    hotel_token <- lapply(1:length(hotel_token), function(x) tolower(hotel_token[[x]]))
    hotel_token <- unique(unlist(hotel_token))
    
    address_city <- unique(unlist(address_city))
    
    # removing hotel name and address from the content
    city_rating$Content <- lapply(1:nrow(city_rating), function(x) 
      rm_stopwords(city_rating$Content[[x]], stopwords = hotel_token, separate = FALSE))
    
    city_rating$Content <- lapply(1:nrow(city_rating), function(x) 
      rm_stopwords(city_rating$Content[[x]], stopwords = address_city, separate = FALSE))
    
    city_rating_new <- data.frame(lapply(city_rating, as.character), stringsAsFactors=FALSE)
    
    # write csv file for each city
    write.csv(city_rating_new, paste(city, "city_rating.csv", sep = "_"))
  }
}
