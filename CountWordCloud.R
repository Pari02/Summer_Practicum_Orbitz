# calling libraries
library(wordcloud)
library(plyr)

# creaing function, wich will be used later while creating wordcloud
colfunc<-colorRamp(c("red", "green"))

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "SummerSemester", "SummerPracticum")
dirS <- file.path(dirP, "WordCloud")
path <- file.path(dirP, "Feature Results") 

# extracting name of all the files that will be used to create wordcloud
file.names <- dir(path, pattern = ".csv")

# creating directory where output will be saved
dir.create(file.path(dirS, "count"), showWarnings = FALSE)

# loop for creating wordcloud
for(i in 1:length(file.names)){
  file <- read.csv(file.path(path, file.names[i]), header = TRUE)
  filename <- paste(gsub("Freq.+", "", file.names[i]), "count", "WordCloud.png", sep="_")
  savepath <- file.path(dirS, "count", filename)
  png(savepath)
  # pasting word1 and word2 together for the wordcloud
  words_paste <- paste(file$word1, file$word2, sep=" ")
  # addding the new field created by adding word1 and word2 to the file extracted
  # and saving it in another variable
  combo <- cbind(file, words_paste)
  # using the count function from "plyr" library counting the combination of the new field
  combo <- count(combo, "words_paste")
  # passing values to the colfun created earlier, the value obtained for each combo
  # will be used to find their rgb values
  col <- colfunc(ifelse(combo$freq >= 10, 
                        ifelse((combo$freq/10)>7, (round(combo$freq/10)-5)/4, 
                               ifelse ((combo$freq/10)>= 4, (round(combo$freq/10)-3)/4, round(combo$freq/40))), 
                        ifelse(combo$freq>7, round((combo$freq-5)/4), 
                               ifelse(combo$freq>= 4, round((combo$freq-3)/4), round(combo$freq/4)))))
  wordcloud(combo$words_paste, combo$freq, colors = rgb(col, max = 255), ordered.colors = TRUE, max.words = 50, random.order = FALSE, scale = c(2, 0.1))
  dev.off()
}