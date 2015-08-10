# installing required libraries
install.packages("rjson")
install.packages("jsonlite")
install.packages("hydroTSM")
install.packages("RCurl")
install.packages("XML")
library(rjson)
library(jsonlite)
library(hydroTSM)
library(plyr) 
library(RCurl)
library(XML)

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "SummerSemester")
dirS <- file.path(dirP, "SummerPracticum", "CityRatingGraphs") # cityRating for csv's
path <- file.path(dirP, "Practicum-CSP 572", "TripAdvisorJson", "json") 

# creating directory where output will be saved
dir.create(file.path(dirS), showWarnings = FALSE)

# extract the data from the files
filename <- list.files(path, pattern = ".json", full.names = TRUE)

# extracting all the files
all_data <- lapply(filename, function(x) fromJSON(x))

# extracting address of all the files
all_address <- lapply(1:length(all_data), function(i) all_data[[i]]$HotelInfo$Address)
all_doc <- lapply(1:length(all_address), function(x) htmlParse(all_address[x], asText = TRUE))
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

all_city <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//span[@property='v:locality']", xmlValue))
all_city <- unique(subset(all_city, lapply(1:length(all_city), function(x) length(all_city[[x]])) > 0))

# extracting rating & review date for all the hotels
all_rating <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Ratings$Overall)
all_reviewDate <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Date)

# extracting bunch of hotels within a city
for(i in 1:length(all_city)){
  
  dir.create(file.path(dirS), showWarnings = FALSE)
  city <- all_city[[i]]

  # getting only those indexes which have city name in them
  city_filter <- which(sapply(lapply(1:length(address), function(i) any(which(address[[i]] %in% city))), isTRUE))  
  
  # using the index field to get rating, review date information
  rating_city <- all_rating[city_filter]
  reviewDate_city <- all_reviewDate[city_filter]
  
  
  # combining city name, review date and rating
  city_rating <- lapply(1:length(city_filter), function(x) cbind(city, reviewDate_city[[x]], rating_city[[x]]))
  
  # command to create flatlist in R by row 
  city_rating <- do.call(rbind.data.frame, city_rating)
  
  # assigning names to columns
  colnames(city_rating) <- c("City", "Date", "Rating")
  
  # getting average rating for the city by month
  avg_rating <- aggregate(as.numeric(city_rating[,"Rating"])
                          , by = list(city_rating[,"City"], as.yearmon(city_rating[,"Date"], "%B%d, %Y"))
                          , FUN = mean, na.rm = TRUE)
  
  # assigning names to columns
  colnames(avg_rating) <- c("City", "Date", "AvgRating")
  
  # writing the data into a csv (optional)
  #write.csv(avg_rating, paste(city , "avg_rating.csv", sep = "_"))
  
  file.name <- paste(city, "curveGraph.png", sep="_")
  savepath <- file.path(dirS, file.name)
  jpeg(savepath)
  
  plot(avg_rating$Date, avg_rating$AvgRating, type = "l", xlab = "Date", ylab = city)
  
  # to close the plot opened for each file
  dev.off()
}
  