# calling libraries
library(wordcloud)
library(plyr)

# creaing function, wich will be used later while creating wordcloud
colfunc<-colorRamp(c("red", "green"))

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "SummerSemester"
                  , "SummerPracticum", "Old Feature & Fequency")
dirS <- file.path(dirP, "WordCloud")
path <- file.path(dirP, "Feature Results") 

# extracting name of all the files that will be used to create wordcloud
file.names <- dir(path, pattern = ".csv")

# creating a variable rating storing sentiment
rating <- c("Positive", "Negative", "Neutral")

# loop for creating wordcloud for each city
for (k in 1:length(rating)){
  dir.create(file.path(dirS, rating[k]), showWarnings = FALSE)
  for (i in 1:length(file.names)){
    csvIn <- read.csv(file.path(path, file.names[i]), header = TRUE)
    ifelse(rating[k] == "Positive", file <- csvIn[csvIn$sentiment == "pos",]
           , ifelse(rating[k] == "Negative", file <- csvIn[csvIn$sentiment == "neg",]
                    , file <- csvIn[csvIn$sentiment == "neutral",]))
    filename <- paste(gsub("Freq.+", "", file.names[i]), rating[k], "WordCloud.png", sep="_")
    savepath <- file.path(dirS, rating[k], filename)
    png(savepath)
    
    # pasting word1 and word2 together for the wordcloud
    words_paste <- paste(file$word1, file$word2, sep=" ")
    
    # addding the new field created by adding word1 and word2 to the file extracted
    # and saving it in another variable
    combo <- cbind(file, words_paste)
    
    # using the count function from "plyr" library counting the combination of the new field
    combo <- count(combo, "words_paste")
    new_freq <- lapply(1:nrow(combo), function(x) ifelse(combo[x,2]>=100, combo[x,2]/100, 
                                                         ifelse(combo[x,2]>=10, combo[x,2]/10, combo[x,2])))
    new_freq <- as.data.frame(unlist(new_freq))
    combo <- cbind(combo, new_freq)
    colnames(combo) <- c("wordPair", "oldFreq", "newFreq")
    
    # passing values to the colfun created earlier, the value obtained for each combo
    # will be used to find their rgb values
    col <- colfunc(ifelse(combo$newFreq >= 10, 
                          ifelse((combo$newFreq/10)>7, floor(((combo$newFreq/10)-5)/4), 
                                 ifelse ((combo$newFreq/10)>= 4, (floor(combo$newFreq/10)-3)/4, floor(combo$newFreq/40))), 
                          ifelse(combo$newFreq>7, floor((combo$newFreq-5)/4), 
                                 ifelse(combo$newFreq>= 4, floor((combo$newFreq-3)/4), floor(combo$newFreq/4)))))
    
    wordcloud(combo$wordPair, combo$newFreq, colors = rgb(col, max = 255), ordered.colors = TRUE, max.words = 50, random.order = FALSE, scale = c(2, 0.1))
    dev.off()
  }
}
