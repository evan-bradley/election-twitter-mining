loadPackages <- function() {
    needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud",
                "biclust", "cluster", "igraph", "stringr", "lubridate",
				"syuzhet", "scales", "reshape2", "dplyr") 
    lapply(needed, require, character.only = TRUE)
}

hashtags <- c("election2016", "notmypresident", "electionnight",
    "trump", "clinton", "maga",
    "makeamericagreatagain", "americafirst", "draintheswamp")
	
words <- read.table("stopwords2.txt") 

expProcess <- function(unclean_tweet) {
    clean_tweet = gsub("&amp", "", unclean_tweet)
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    clean_tweet = gsub("@\\w+", "", clean_tweet)
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    clean_tweet = gsub("http\\w+", "", clean_tweet)
    clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 

    #get rid of unnecessary spaces
    clean_tweet <- str_replace_all(clean_tweet," "," ")
    # Get rid of URLs
    clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
    # Take out retweet header, there is only one
    clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
    # Get rid of hashtags
    clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
    # Get rid of references to other screennames
    clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")

    return(clean_tweet)
}

process <- function(data, dup = FALSE, stem = TRUE) {
	# Remove duplicate entries
	if(dup) {
		data <- unique(data)
	}
	
    myCorpus <- Corpus(VectorSource(data))
	myCorpus <- tm_map(myCorpus,
	                     content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
	                     mc.cores=1)
	
    # myCorpus <- tm_map(myCorpus, content_transformer(expProcess), lazy = TRUE)
    #removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    #myCorpus <- tm_map(myCorpus, content_transformer(removeURL), mc.cores = 1)
    # removeHash <- function(x) gsub("#[^[:space:]]*", "", x)
    # myCorpus <- tm_map(myCorpus, content_transformer(removeHash), lazy = TRUE)
    #removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    #myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct), mc.cores = 1)
	myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers), mc.cores = 1)
	myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation), mc.cores = 1)
    myCorpus <- tm_map(myCorpus, content_transformer(tolower), mc.cores = 1)
	myCorpus <- tm_map(myCorpus, removeWords, words, mc.cores = 1)
	
	#if(dup) {
	#	removeDup <- function(x) unique(x)
	#	myCorpus <- tm_map(myCorpus, removeDup, mc.cores = 1)
	#}
	
	if(stem) {
		myCorpus <- tm_map(myCorpus, stemDocument, mc.cores = 1)
	}
    myCorpus <- tm_map(myCorpus, stripWhitespace, mc.cores = 1)
	
   return(myCorpus) 
}

tdmidf <- function(corpus, sparse = .99) {
    tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
	tdm <- removeSparseTerms(tdm, sparse)
    return(tdm)
}

tdminf <- function(corpus, sparse = .99) {
    tdm <- TermDocumentMatrix(corpus,
    control = list(wordLengths = c(1, Inf)))
	tdm <- removeSparseTerms(tdm, sparse)
    return(tdm)
}

plotFreqTerms <- function(tdm, frequency) {
    term.freq <- rowSums(as.matrix(tdm), na.rm = TRUE)
    term.freq <- subset(term.freq, term.freq >= frequency)
    df <- data.frame(term = names(term.freq), freq = term.freq)

    ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
    xlab("Terms") + ylab("Count") + coord_flip() +
    theme(axis.text=element_text(size=12))
}

termsHash <- function(hash) {
	#png(paste(paste("terms", hash, sep="-"), ".png", sep=""), 
	#	width = 800, height = 800, units = "px", pointsize = 24)
	tweets <- read.csv(paste(paste("data", hash, sep="-"), "csv", sep="."))
	freq <- as.integer(length(tweets$text) / 30)
	plotFreqTerms(tdminf(process(tweets$text, dup = FALSE, stem = TRUE)), freq)
	ggsave(paste(paste("terms", hash, sep="-"), ".png", sep=""), scale = 0.5)
	#dev.off()
}

allHashTerms <- function() {
	for(hash in hashtags) {
		termsHash(hash)
	}
}

cluster <- function(tdm, title = "Cluster Dendrogram") {
	df <- as.data.frame(inspect(tdm))
	df.scale <- scale(df)
	d <- dist(df.scale, method = "euclidean") # distance matrix
	fit <- hclust(d, method="ward.D")
	plot(fit)
	
	groups <- cutree(fit, k=5) # cut tree into 5 clusters
	# draw dendogram with red borders around the 5 clusters
	rect.hclust(fit, k=5, border="red")
}

clusterHash <- function(hash) {
	png(paste(paste("cluster", hash, sep="-"), ".png", sep=""), 
		width = 800, height = 800, units = "px", pointsize = 24)
	tweets <- read.csv(paste(paste("data", hash, sep="-"), "csv", sep="."))
	freq <- as.integer(length(tweets$hashtags) / 10)
	cluster(tdminf(process(tweets$hashtags, dup = FALSE, stem = FALSE), sparse = .99),
		paste(paste("#", hash, sep=""), "Dendrogram", sep = " "))
	dev.off()
}

allHashClusters <- function() {
	for(hash in hashtags) {
		clusterHash(hash)
	}
}

freqTermsHash <- function(filename, frequency) {
	tweets <- read.csv(filename)
	plotFreqTerms(tweets)
}

genWordCloud <- function(tdm, frequency) {
    m <- as.matrix(tdm)
    word.freq <- sort(rowSums(m, na.rm = TRUE), decreasing = T)
    pal <- brewer.pal(9, "BuGn")[-(1:4)]


    wordcloud(words = names(word.freq), freq = word.freq, min.freq = frequency,
    random.order = F, colors = pal)
}

wordCloudHash <- function(hash) {
	png(paste(paste("cloud", hash, sep="-"), ".png", sep=""), 
		width = 800, height = 800, units = "px", pointsize = 24)
	tweets <- read.csv(paste(paste("data", hash, sep="-"), "csv", sep="."))
	freq <- as.integer(length(tweets$bio) / 10)
	genWordCloud(tdminf(process(tweets$bio, dup = TRUE, stem = FALSE), sparse = .99), freq)
	dev.off()
}

#Most of this code is from: http://juliasilge.com/blog/Joy-to-the-World/
posnegWeek <- function(filename) {
	tweets <- read.csv(filename, stringsAsFactors =  FALSE)
	mySentiment <- get_nrc_sentiment(tweets$text)
	tweets <- cbind(tweets, mySentiment)
	tweets$timestamp <- ymd_hms(tweets$time)
	posnegtime <- tweets %>% 
	        group_by(timestamp = cut(timestamp, breaks="1 day")) %>%
	        summarise(negative = mean(negative),
	                  positive = mean(positive)) %>% melt
	names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
	posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])

	ggplot(data = posnegtime, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
	        geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
	        geom_point(size = 0.5) +
	        ylim(0, NA) + 
	        scale_colour_manual(values = c("springgreen4", "firebrick3")) +
	        theme(legend.title=element_blank(), axis.title.x = element_blank()) +
	        scale_x_date(breaks = date_breaks("1 day"), 
	                     labels = date_format("%d-%h")) +
	        ylab("Average sentiment score") + 
	        ggtitle("Sentiment Over Time")
}

posnegHash <- function(hash) {
	posnegWeek(paste(paste("data", hash, sep="-"), "csv", sep="."))
	ggsave(paste(paste("sentiment", hash, sep="-"), ".png", sep=""), scale = 0.5)
}

allposnegHash <- function() {
	for(hash in hashtags) {
		posnegHash(hash)
	}
}

# Most of this code is from: http://juliasilge.com/blog/Joy-to-the-World/
sentimentWeek <- function(filename) {
	tweets <- read.csv(filename, stringsAsFactors =  FALSE)
	mySentiment <- get_nrc_sentiment(tweets$text)
	tweets <- cbind(tweets, mySentiment)
	tweets$timestamp <- ymd_hms(tweets$time)
	timesentiment <- tweets %>% group_by(timestamp = cut(timestamp, breaks="1 day")) %>% 
	        summarise(anger = mean(anger), 
	                  anticipation = mean(anticipation), 
	                  disgust = mean(disgust), 
	                  fear = mean(fear), 
	                  joy = mean(joy), 
	                  sadness = mean(sadness), 
	                  surprise = mean(surprise), 
	                  trust = mean(trust)) %>% melt
	names(timesentiment) <- c("timestamp", "sentiment", "meanvalue")

	ggplot(data = timesentiment, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
	        geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
	        geom_point(size = 0.5) +
	        ylim(0, NA) +
	        theme(legend.title=element_blank(), axis.title.x = element_blank()) +
	        scale_x_date(breaks = date_breaks("1 day"), 
	                     labels = date_format("%d-%h")) +
	        ylab("Average sentiment score") + 
	        ggtitle("Sentiment Over Time")
}

plotFreq <- function(filename) {
	tweets <- read.csv(filename, stringsAsFactors =  FALSE)
	tweets$time <- ymd_hms(tweets$time)
	ggplot(data = tweets, aes(x = time)) +
	        geom_histogram(aes(fill = ..count..), binwidth = 3600 * 12) +
	        theme(legend.position = "none") +
	        xlab("Time") + ylab("Number of tweets") + 
	        scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
}

freqHash <- function(hash) {
	#png(paste(paste("terms", hash, sep="-"), ".png", sep=""), 
	#	width = 800, height = 800, units = "px", pointsize = 24)
	plotFreq(paste(paste("data", hash, sep="-"), "csv", sep="."))
	ggsave(paste(paste("freq", hash, sep="-"), ".png", sep=""), scale = 0.5)
	#dev.off()
}

allHashFreqs <- function() {
	for(hash in hashtags) {
		freqHash(hash)
	}
}