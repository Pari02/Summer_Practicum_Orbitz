# installing required libraries
install.packages("rjson")
install.packages("jsonlite")
install.packages("hydroTSM")
install.packages("RCurl")
install.packages("XML")
install.packages("zoo")
installed.packages("ggplot2")
library(rjson)
library(jsonlite)
library(hydroTSM)
library(plyr) 
library(RCurl)
library(XML)
library(zoo)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "SummerSemester")
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
dir.create(file.path(dirS, "top5"), showWarnings = FALSE)

# removing address of hotels which are not in United States
country_filter <- which(sapply(lapply(1:length(address), function(x) any(which(address[[x]] %in% all_country))), isTRUE))  
address <- address[-country_filter]

# extracting rating & review date for all the hotels
all_rating <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Ratings$Overall)
rating_all <- all_rating[-country_filter]
all_reviewDate <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Date)
reviewDate_all <- all_reviewDate[-country_filter]
all_hotel <- lapply(1:length(all_data), function(i) all_data[[i]]$HotelInfo$Name)
hotels <- all_hotel[-country_filter]

# extarcting us_cities
us_cities <- all_city[-country_filter]
cities <- unique(subset(us_cities, lapply(1:length(us_cities), function(x) length(us_cities[[x]])) > 0))

# getting only those indexes which have city name in them
city_filter <- which(sapply(lapply(1:length(address)
                                   , function(x) any(which(address[[x]] %in% cities))), isTRUE))  

city_name <- us_cities[city_filter]
city_freq <- as.data.frame(table(unlist(city_name)))

# subsetting cities with hotel count >= 30
city_freq <- as.character(subset(city_freq$Var1, city_freq$Freq >= 30))

city_30 <- which(sapply(lapply(1:length(address)
                               , function(x) any(which(address[[x]] %in% city_freq))), isTRUE))  

# using the index field to get rating, review date information
rating_city <- rating_all[city_30]
reviewDate_city <- reviewDate_all[city_30]
hotelName <- hotels[city_30]
city <- us_cities[city_30]

# combining city name, review date and rating
city_rating <- lapply(1:length(city_30), function(x) cbind(city[[x]], hotelName[[x]], reviewDate_city[[x]], rating_city[[x]]))      

# command to create flatlist in R by row 
city_rating <- do.call(rbind.data.frame, city_rating)

# assigning names to columns
colnames(city_rating) <- c("City", "Hotel", "Date", "Rating")

# conversting datatype of Date column to Date and Rating to numeric
city_rating$Date <- as.Date(city_rating$Date, "%B%d, %Y")

# as.character(f) requires a "primitive lookup" to find the function as.character.factor()
#, which is defined as as.numeric(levels(f))[f]
city_rating$Rating <- as.numeric(levels(city_rating$Rating))[city_rating$Rating]

# subsetting thr data set for review dates > year 2012
cr_2012 <- subset(city_rating, city_rating$Date >= "2012-01-01")
cr_2012$Date <- format(as.Date(cr_2012$Date, "%Y-%m-%d"), "%Y-%m")

city_avg <- ddply(cr_2012, c("City"), summarise, AvgRating = mean(Rating))
top_5 <- head(city_avg[order(city_avg$AvgRating, decreasing= T),], n = 5)

# extracting average top 5 citis from city avg
top5_filter <- which(sapply(lapply(1:nrow(city_avg)
                                   , function(x) any(which(city_avg[x,1] %in% as.character(top_5$City))))
                            , isTRUE))
city_avg_5 <- city_avg[top5_filter,]

top5 <- as.character(top_5$City)

for (i in 1:length(top5))
{
  city <- top5[i]
  indexes <- which(sapply(lapply(1:nrow(cr_2012)
                                  , function(x) any(which(cr_2012[x,"City"] %in% city)))
                           , isTRUE))
  city_5 <- cr_2012[indexes,]
  
  # aggregating data by hotel name
  hotel_5_avg <- ddply(city_5, c("Hotel", "Date"), summarise, AvgRating = mean(Rating))
  hotel_5 <- ddply(city_5, c("Hotel"), summarise, AvgRating = mean(Rating))
  top_5_hotel <- head(hotel_5[order(hotel_5$AvgRating, decreasing= T),], n = 5)
  filter_5_hotel <- which(sapply(lapply(1:nrow(hotel_5_avg)
                                        , function(x) any(which(hotel_5_avg[x,"Hotel"] %in% as.character(top_5_hotel$Hotel))))
                                 , isTRUE))
  hotel_5_avg <- hotel_5_avg[filter_5_hotel,]
  colnames(hotel_5_avg) <- c("Name", "Date", "AvgRating")
  
  city_5_avg <- ddply(city_5, c("City", "Date"), summarise, AvgRating = mean(Rating))
  colnames(city_5_avg) <- c("Name", "Date", "AvgRating")
  df_5_avg <- do.call(rbind, list(city_5_avg, hotel_5_avg))
  
  # setting dimensions for saving the plot
  file.name <- paste(city, "top5_Hotels.png", sep="_")
  savepath <- file.path(dirS, "top5", file.name)
  
  # writing the data into a csv (optional)
  #write.csv(avg_rating, paste(city , "USavg_rating.csv", sep = "_"))
  #len <- length(unique(df_5_avg$Name))
  col <- palette(rev(rich.colors(6)))

  # generating plot
  df_5_plot <- ggplot(data = df_5_avg, aes(Date, AvgRating, group = Name, colour = Name)) + 
    geom_line(size = 2) + 
    scale_colour_manual(name = "City & Hotels", breaks = df_5_avg$Name, values = col) + 
    xlab("Date") + ggtitle(paste("Average Rating of Top 5 Hotels -", city)) +
    theme(legend.justification = c(1, 0), legend.position = c(1, 0), 
          legend.background = element_rect(colour = "black"))
  
  # save the plot
  ggsave(savepath, plot = df_5_plot, width = 12, height = 6, units = 'in')
    
}
