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
df_content <- data.frame(matrix(unlist(content)))
colnames(df_content) <- c('content')
hotel_info <- data.frame(hotelID = json_data$HotelInfo$HotelID, hotelName = json_data$HotelInfo$Name)
hotel <- merge(df_content, hotel_info, all =T)
colnames(hotel) <- c('Content', 'hotelID', 'hotelName')

rules <- apriori(hotel)

# Parameter specification:
confidence minval smax arem  aval originalSupport support minlen maxlen target   ext
0.8    0.1    1 none FALSE            TRUE     0.1      1     10  rules FALSE

# Algorithmic control:
#  filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE

# apriori - find association rules with the apriori algorithm
# version 4.21 (2004.05.09)        (c) 1996-2004   Christian Borgelt
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[235 item(s), 233 transaction(s)] done [0.00s].
# sorting and recoding items ... [2 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 done [0.00s].
# writing ... [4 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s].

inspect(rules)