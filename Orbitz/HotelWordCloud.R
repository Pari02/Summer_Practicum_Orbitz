# calling libraries
library(wordcloud)
library(plyr)
library(ggplot2)
library(gplots)
library(RColorBrewer)

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science", "Summer 2015"
                  , "SummerPracticum", "Orbitz")
#dirS <- file.path(dirP, "Ins_City_WordCloud")
path <- file.path(dirP, "seattle") 

# extracting name of all the files that will be used to create wordcloud
file.names <- dir(path, pattern = ".csv")

# creating a variable rating storing sentiment
rating <- c("Positive", "Negative", "Neutral")

# loop for creating wordcloud for each city
for (k in 1:length(rating))
{
  dir.create(file.path(path, rating[k]), showWarnings = FALSE)
  for (i in 1:length(file.names))
  {
    csvIn <- read.csv(file.path(path, file.names[i]), header = TRUE)
    if (rating[k] == "Positive")
    {
      file <- csvIn[csvIn$Sentiment == "Positive",]
    }
    else if(rating[k] == "Negative")
    {
      file <- csvIn[csvIn$Sentiment == "Negative",] 
    }
    else
    {
      file <- csvIn[csvIn$Sentiment == "Neutral",]
    }
    
    filename <- paste(i, rating[k], "WordCloud.png", sep="_")
    savepath <- file.path(path, rating[k], filename)
    png(savepath)
  }
  
  len <- length(file$bigram)
  
  # creaing function, wich will be used later while creating wordcloud
  if(len != 0)
  {
    col_new <- rev(rich.colors(len))
    wordcloud(file$bigram, file$AvgRating, colors = col_new, ordered.colors = TRUE, max.words = 20, random.order = FALSE, scale = c(2, 0.5))
    dev.off()
  }
}
    #df_5_plot <- ggplot(data = file, aes(bigram, AvgRating, group = Sentiment, colour = Sentiment)) + 
    #  geom_line(size = 2) + 
    #  scale_colour_manual(name = "City & Hotel", breaks = file$Sentiment, values = col) + 
    #  xlab("bigram") + ggtitle("test") + 
    #  theme(legend.justification = c(1, 0), legend.position = c(1, 0), 
    #        legend.background = element_rect(colour = "black"),
    #        axis.text.x = element_text(angle = 90, hjust=1))
    
    # save the plot
    #ggsave(savepath, plot = df_5_plot, width = 24, height = 10, units = 'in')
