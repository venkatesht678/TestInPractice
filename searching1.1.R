library(tm)
library(SnowballC)
files <- list.files(pattern = "sas")
#list containing all the files
filelist <- lapply(files , readLines)
flength <- length(filelist)
names(filelist) <- paste0("doc" , 1:flength)
query = "country image"
corpus <-  VectorSource(c(filelist , query))
corpus$Names <- c(names(filelist), "query")
corpus1 <- Corpus(corpus)
#plaintextdocument should appear before stemDocument
corpusclean <- tm_map(corpus1 , tolower)
corpusclean <- tm_map(corpusclean , PlainTextDocument)
corpusclean <- tm_map(corpusclean , removePunctuation)
corpusclean <- tm_map(corpusclean , stemDocument)
corpusclean <- tm_map(corpusclean , removeNumbers)
corpusclean <- tm_map(corpusclean , removeWords , stopwords())
corpusclean <- tm_map(corpusclean , stripWhitespace)
corpustdm <- TermDocumentMatrix(corpusclean)
inspect(corpustdm[1:5,])
corpustdm <- TermDocumentMatrix(corpusclean ,control = list(weighting = weightTfIdf))
inspect(corpustdm[1:5,])
queryvector <- corpustdm[,4]
termvector <- corpustdm[,1:3]
score <- t(as.matrix(queryvector)) %*% as.matrix(termvector)
#converts score to a vector vectorizing a matrix
results <- data.frame(doc = names(filelist) , score = c(t(score)) , text = unlist(filelist))
results <- results[order(results$score , decreasing = TRUE),]
options(width = 2000)
print(results, row.names = FALSE, right = FALSE, digits = 2)
