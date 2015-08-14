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
dir.create(file.path(dirS, "abc"), showWarnings = FALSE)

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

# subsetting thr data set for review dates > year 2010
cr_2012 <- subset(city_rating, city_rating$Date >= "2012-01-01")
cr_2012$Date <- format(as.Date(cr_2012$Date, "%Y-%m-%d"), "%Y-%m")

# aggregating data by City and renaming the column names
city_avg <- ddply(cr_2012, c("City", "Date"), summarise, AvgRating = mean(Rating))
colnames(city_avg) <- c("Name", "Date", "AvgRating")

city_avg1 <- ddply(cr_2012, c("City"), summarise, AvgRating = mean(Rating))
top_5 <- head(city_avg1[order(city_avg1$AvgRating, decreasing= T),], n = 5)

# extracting average top 5 citis from city avg
top5_filter <- which(sapply(lapply(1:nrow(city_avg)
                                   , function(x) any(which(city_avg[x,1] %in% as.character(top_5$City))))
                            , isTRUE))
city_avg_5 <- city_avg[top5_filter,]

# setting dimensions for saving the plot
file_plot <- paste("US", "citylPlot.png", sep="_")
file_plot_5 <- paste("US", "citylPlot_5.png", sep="_")
file_wordcloud <- paste("US", "citylWordcloud.png", sep="_")
file_top5 <- paste("US", "citylTop5.png", sep="_")
savepath1 <- file.path(dirS, "abc", file_plot)
savepath2 <- file.path(dirS, "abc", file_wordcloud)
savepath3 <- file.path(dirS, "abc", file_top5)
savepath4 <- file.path(dirS, "abc", file_plot_5)

# writing the data into a csv (optional)
#write.csv(avg_rating, paste(city , "USavg_rating.csv", sep = "_"))

# generating color palette
len <- length(unique(city_avg$Name))
gs.pal <- palette(rev(rich.colors(len)))
gs.pal_5 <- palette(rev(rich.colors(5)))

# generating plot
city_plot <- ggplot(data = city_avg, aes(Date, AvgRating, group = Name, colour = Name)) + 
  geom_line(size = 2) + 
  scale_colour_manual(name = "Cities", breaks = city_avg$Name, values = gs.pal) + 
  xlab("Date") + ggtitle("Average Rating of US Cities") +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0), 
        legend.background = element_rect(colour = "black"))

city_plot5 <- ggplot(data = city_avg_5, aes(Date, AvgRating, group = Name, colour = Name)) + 
  geom_line(size = 2) + 
  scale_colour_manual(name = "Cities", breaks = city_avg$Name, values = gs.pal_5) + 
  xlab("Date") + ggtitle("Average Rating of US Cities") +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0), 
        legend.background = element_rect(colour = "black"))

# save the plot
ggsave(savepath1, plot = city_plot, width = 20, height = 8, units = 'in')
ggsave(savepath4, plot = city_plot5, width = 20, height = 8, units = 'in')

#generating and saving wordclouds
png(savepath2)
wordcloud(city_avg1$City, city_avg1$AvgRating, colors = gs.pal, ordered.colors = TRUE
          , max.words = 50, random.order = FALSE, scale = c(2, 0.1))
dev.off()

png(savepath3)
wordcloud(as.character(top_5$City), top_5$AvgRating, colors = palette(rev(rich.colors(nrow(top_5))))
          , ordered.colors = TRUE, max.words = 10, random.order = FALSE, scale = c(2, 0.1))
dev.off()
