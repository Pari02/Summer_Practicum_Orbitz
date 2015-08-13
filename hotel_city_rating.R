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

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "SummerSemester")
dirS <- file.path(dirP, "SummerPracticum", "USHotelBar") # cityRating for csv's
path <- file.path(dirP, "Practicum-CSP 572", "TripAdvisorJson", "json") 

# creating directory where output will be saved
dir.create(file.path(dirS), showWarnings = FALSE)

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
us_cities <- unique(subset(us_cities, lapply(1:length(us_cities), function(x) length(us_cities[[x]])) > 0))

# extracting bunch of hotels within a city
for(i in 1:length(us_cities))
{
  dir.create(file.path(dirS), showWarnings = FALSE)
  #city <- us_cities[[i]]
  city <- "Miami"
  # getting only those indexes which have city name in them
  city_filter <- which(sapply(lapply(1:length(address), function(x) any(which(address[[x]] %in% city))), isTRUE))  
  
  if (length(city_filter) >= 30)
  {
    # using the index field to get rating, review date information
    rating_city <- rating_all[city_filter]
    reviewDate_city <- reviewDate_all[city_filter]
    hotelName <- hotels[city_filter]
  
    # combining city name, review date and rating
    city_rating <- lapply(1:length(city_filter), function(x) cbind(hotelName[[x]], reviewDate_city[[x]], rating_city[[x]]))      
    
    # command to create flatlist in R by row 
    city_rating <- do.call(rbind.data.frame, city_rating)
    city_rating <- cbind(city, city_rating)
    
    # assigning names to columns
    colnames(city_rating) <- c("City", "Hotel", "Date", "Rating")
    
    # conversting datatype of Date column to Date and Rating to numeric
    city_rating$Date <- as.Date(city_rating$Date, "%B%d, %Y")
    
    # as.character(f) requires a "primitive lookup" to find the function as.character.factor()
    #, which is defined as as.numeric(levels(f))[f]
    city_rating$Rating <- as.numeric(levels(city_rating$Rating))[city_rating$Rating]
    
    # subsetting thr data set for review dates > year 2010
    cr_2010 <- subset(city_rating, city_rating$Date >= "2012-01-01")
    cr_2010$Date <- format(as.Date(cr_2010$Date, "%Y-%m-%d"), "%Y-%m")
    
    # aggregating data by hotel name
    hotel_avg <- ddply(cr_2010, c("Hotel", "Date"), summarise, AvgRating = mean(Rating))
    colnames(hotel_avg) <- c("Name", "Date", "AvgRating")
    
    city_avg <- ddply(cr_2010, c("City", "Date"), summarise, AvgRating = mean(Rating))
    colnames(city_avg) <- c("Name", "Date", "AvgRating")
    
    # subsetting the data set for average ratings > 4.5
    hotel_avg <- subset(hotel_avg, hotel_avg$AvgRating > 4.5, row.names = FALSE)
    #city_avg <- subset(city_avg, city_avg$AvgRating > 4.0, row.names = FALSE)
    
    df_avg <- do.call(rbind, list(city_avg, hotel_avg))
    
    # setting dimensions for saving the plot
    file.name <- paste(city, "hotelPlot.png", sep="_")
    savepath <- file.path(dirS, file.name)
    
    
    # writing the data into a csv (optional)
    #write.csv(avg_rating, paste(city , "USavg_rating.csv", sep = "_"))
    
    # generating color palette
    gs.pal <- colorRampPalette(c("yellow", "red", "green", "blue"), bias=.1, space="rgb")
    len <- length(unique(df_avg$Name))
    
    # generating plot
    hotel_plot <- ggplot(data = df_avg, aes(Date, AvgRating, group = Name, colour = Name)) + 
      geom_line(size = 2) + 
      #geom_point(size = 2) + 
      scale_colour_manual(name = "City & Hotels", breaks = df_avg$Name, values = gs.pal(len)) + 
      xlab("Date") + ggtitle(paste("Average Rating of Hotels -", city)) +
      #stat_smooth(method="lm", se=FALSE) + 
      theme(legend.justification = c(1, 0), legend.position = c(1, 0), 
           legend.background = element_rect(colour = "black"))
    
    # save the plot
    ggsave(savepath, plot = hotel_plot, width = 24, height = 16, units = 'in')
        
    }
}
