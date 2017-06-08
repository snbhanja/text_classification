# validation 

dfValidation <- read.csv("D:/ticket_class/ticket_class/dfValidation.csv", header = TRUE)

library(tm)
# make a corpus of training data.
corpus <- VCorpus(VectorSource(dfValidation$summary))

docs <-tm_map(corpus,content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "$")
docs <- tm_map(docs, toSpace, "%")
#docs <- tm_map(docs, toSpace, "•")
#docs <- tm_map(docs, toSpace, "“")
#docs <- tm_map(docs, toSpace, "”")

#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)

#Good practice to check every now and then
writeLines(as.character(docs[[30]]))

#Stem document
# A stemming algorithm reduces the words "fishing", "fished", and "fisher" to the root word, "fish"
docs <- tm_map(docs,stemDocument)
writeLines(as.character(docs[[30]]))

# Remove all single letter words and then excess space
#docs <-tm_map(docs, content_transformer( function(x) gsub(" *\\b[[:alpha:]]{1}\\b *", " ", x)))
#docs <-tm_map(docs, content_transformer( function(x) gsub("^ +| +$|( ) +", "\\1", x)))

#define and eliminate all custom stopwords
myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","‘ve ",
                 "‘re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot","don","isn","paul","didn","are","won","let",
                 "doesn","go","know","yes","lou","couldn","abn","amsterdam","ddmmyyyy","pleas","check",
                 "affect","item","occur","pm","hi","team","dear",
                 "gustav","mahlerlaan","fbspr","host","fullmsg","print","email","short","descript","date","time",
                 "step","taken","start","date","receiv","per","detail","helpdesk","person","phone","person","hi",
                 "fri","aug"
)
docs <- tm_map(docs, removeWords, myStopwords)

docs <-tm_map(docs, content_transformer( function(x) gsub(" *\\b[[:alpha:]]{1}\\b *", " ", x)))
docs <-tm_map(docs, content_transformer( function(x) gsub("^ +| +$|( ) +", "\\1", x)))

# read the saved dtm file from training
dtm2 <- readRDS("./dtm_db.rds")

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

# tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(dtm2), 
#                                                  removePunctuation = TRUE, 
#                                                  stopwords = TRUE, 
#                                                  stemming = TRUE, 
#                                                  removeNumbers = TRUE))

dtm2 <- DocumentTermMatrix(docs, control = list(dictionary = Terms(dtm2),
                                                tokenize = BigramTokenizer))
dim(dtm2)
#dtm2 = removeSparseTerms(dtm2, 0.99983)

# #-------------------------------------------------
# freq <- colSums(as.matrix(dtm2))
# 
# #length should be total number of terms
# length(freq)
# 
# #create sort order (descending)
# ord <- order(freq,decreasing=TRUE)
# 
# #List all terms in decreasing order of freq and write to disk
# # freq[ord]
# write.csv(freq[ord],"word_freq.csv")
#--------------------------------------------------
dtm2

# Create a document term matrix.
#tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

test <- as.data.frame(as.matrix(dtm2))

model <- readRDS("./ticket_class_pred_svmv1.rds")
predicted_class <- predict(model, newdata = test)

#---------------------------------------------------------
# test with train data
#pred <- predict(model, x)
# (same as:)
# Check accuracy:
conf_mat <- table(predicted_class, dfValidation$ticket_label)

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

accuracy

#--------------------------------------------------------
# Saving predicted class and actual ticket class side by side in a new csv file.

db_output_csv <- dfValidation

db_output_csv$predicted_class <- predicted_class

con = file("db_output.csv","w", encoding="utf8")
write.csv(db_output_csv, con, row.names=FALSE)
close(con)

