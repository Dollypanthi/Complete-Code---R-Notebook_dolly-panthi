#Getting the current working directory
getwd()

#Setting new working directory
setwd('C:/KMPlayer/archive (4)')
# Loading the readr package
library(readr)
# Loading the readr package
library(readr)

#Import the dataset and specifying the column types for "Price" as numeric and "Rate" as integer, treating "NA" as missing values.
flipkart <- read_csv("flipkart_product.csv", col_types = cols(Price = col_number(), Rate = col_integer()), na = "NA")
View(flipkart)


# Removing Rows with missing values
sum(is.na(flipkart))
flipkart <- na.omit(flipkart)

# Checking the structure of the Summary Column
str(flipkart$Summary)

# Now we have to remove punctuation from summary column
library(dplyr)
library(stringi) # for advanced string manipulation 

# The stri_trans_general function from the stringi package is used here to convert non-Latin characters to their closest Latin equivalents.
flipkart <- flipkart %>%
  mutate(
    Summary = stri_trans_general(Summary, "Latin-ASCII"),
    ProductName = stri_trans_general(ProductName, "Latin-ASCII"))

# Removing the punctuation
flipkart <- flipkart %>%
  mutate(Summary = gsub("[[:punct:]]", "", Summary))
View(flipkart)
# Assuming 'data' is a vector or list
head(flipkart, 10)



####Visualization 

plot(flipkart$Price, flipkart$Rate, col = 'blue', pch = 16, main = 'Ratings vs. Prices', xlab = 'Price', ylab = 'Rating')


summary(flipkart)

# Calculate the counts of each unique value in 'Rate'
star_counts <- table(flipart$Rate)

# Create a bar plot
barplot(star_counts,
        main = 'Count of Reviews by Stars',
        xlab = 'Review Stars',
        ylab = 'Count',
        col = 'skyblue',  # You can customize the color
        ylim = c(0, max(star_counts) + 5),  # Adjust ylim for better visualization
        beside = TRUE  # Display bars beside each other
)


####3Null values can affect the overall shape of the dataset, 
##which can affect the sentiment analysis too. We need to check whether the dataset contains null values or not. 
# Assuming 'data' is your data frame
missing_values_count <- colSums(is.na(flipkart))

# Display the result
print(missing_values_count)
####Clean data############


library(tm)
corpus <- iconv(flipkart$Summary)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

###Clean Data

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])
##One sample analysis in R
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])
###Null hypothesis
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])
###remove the same
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
#####Text stemming##
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

############Term document matrix

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

####Word Cloud

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)


#####Sentiment analysis####


tweets <- iconv(flipkart$Review)

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

get_nrc_sentiment(ugly)

###Ugly has scores on disgust and negative
###Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')

##Conclusion
###This article explained reading text data into R,
##corpus creation, data cleaning, transformations and
#explained how to create a word frequency and word
##clouds to identify the occurrence of the text.

##Identification of sentiment scores,
##which proved useful in assigning a numeric 
###value to strength (of positivity or negativity)
###of sentiments in the text and 
#allowed interpreting score of the text.