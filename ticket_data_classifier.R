# Predict class as 1,2,3 i.e. L1, L2, L3

dfTraining <- read.csv("D:/ticket_class/ticket_class/dfTraining.csv", header = TRUE)

library(tm)
# make a corpus of training data.
corpus <- VCorpus(VectorSource(dfTraining$summary))

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

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

dtm2 <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
dim(dtm2)
dtm2 = removeSparseTerms(dtm2, 0.99983)

# Save the dtm2 dtm for later use.
#save(dtm2, file = "dtm_db.rda")
saveRDS(dtm2, "./dtm_db.rds")
#-------------------------------------------------
freq <- colSums(as.matrix(dtm2))

#length should be total number of terms
length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)

#List all terms in decreasing order of freq and write to disk
# freq[ord]
write.csv(freq[ord],"word_freq.csv")
#--------------------------------------------------
dtm2

# Create a document term matrix.
#tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

train <- as.data.frame(as.matrix(dtm2))

train$ticket_label <- dfTraining$ticket_label

# Make the trainLabels a factor
train$ticket_label <- as.factor(train$ticket_label)

library( 'e1071' )

model <- svm( train$ticket_label ~ ., train)

saveRDS(model, "./ticket_class_pred_svmv1.rds")
