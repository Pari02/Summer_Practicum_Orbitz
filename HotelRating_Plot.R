# installing required libraries
install.packages("rjson")
install.packages("jsonlite")
install.packages("hydroTSM")
library(rjson)
library(jsonlite)
library(hydroTSM)
library(wordcloud)
library(plyr)

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "SummerSemester")
dirS <- file.path(dirP, "SummerPracticum", "CurveGraph")
path <- file.path(dirP, "Practicum-CSP 572", "TripAdvisorJson", "json") 

# creating directory where output will be saved
dir.create(file.path(dirS), showWarnings = FALSE)

# extract the data from the files
filename <- list.files(path, pattern = ".json", full.names = TRUE)

for(i in 1:length(filename)){
  file_data <- fromJSON(filename[i])
  
  # extract content, hotel information and id in respective variables
  hotel_rating <- file_data$Reviews$Ratings$Overall
  hotel_name <- file_data$HotelInfo$Name
  review_date <- file_data$Reviews$Date
  
  if(length(hotel_name) != 0)
  {
    # combining hotel_rating, hotel_name, review_date as one nested list
    rating_hotel <- cbind(hotel_rating, hotel_name, review_date)
    
    # aggregating each hotel rating by month and year
    avg_rating <- aggregate(as.numeric(rating_hotel[,"hotel_rating"])
                            , by = list(rating_hotel[,"hotel_name"], as.yearmon(rating_hotel[,"review_date"], "%B%d, %Y"))
                            , FUN = mean, na.rm = TRUE)
    
    # assigning column names to the list
    colnames(avg_rating) <- c("hotel", "Date", "AvgRating")
    
    # writing the data into a csv (optional)
    # write.csv(avg_rating, paste(i , "avg_rating.csv", sep = "_"))
    
    file.name <- paste(i, "curveGraph.png", sep="_")
    savepath <- file.path(dirS, file.name)
    jpeg(savepath)
    
    plot(avg_rating$Date, avg_rating$AvgRating, type = "l", xlab = "Date", ylab = hotel_name)
    
    # to close the plot opened for each file
    dev.off()
  }  
}
