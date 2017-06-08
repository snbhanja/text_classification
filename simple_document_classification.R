library(caret)
library(tm)

# Training data.
data <- c('Cats like to chase mice.', 
          'Dogs like to eat big bones.', 
          'I like the fruit mango.',
          'Dog likes ball catch',
          'Gal gadot likes apple.',
          'Papaya fruit juice is yummy.',
          'I like labrador and German shepard dog breed.',
          'baby cats are cute.')
data

corpus <- VCorpus(VectorSource(data))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, 
                                       stopwords = TRUE, 
                                       stemming = TRUE, 
                                       removeNumbers = TRUE))

dim(tdm)
# the dtm is of 2 rows as two 2 documents and 8 columns as 8 unique 
# words from corpus after text pre processing.

tdm$dimnames

# Convert to a data.frame for training and assign a classification (factor) to each document.

train <- as.matrix(tdm)

train <- cbind(train, c(0, 1, 2, 1, 2, 2, 1, 0))

colnames(train)[ncol(train)] <- 'y'

train <- as.data.frame(train)

train$y <- as.factor(train$y)

# Train.
#fit <- train(y ~ ., data = train, method = 'bayesglm')

library( 'e1071' )
model <- svm( y ~ ., train )

# Check accuracy on training.
predict(model, newdata = train)

# Test data.
data2 <- c('Bats eat bugs.')

corpus <- VCorpus(VectorSource(data2))

tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), 
                                                 removePunctuation = TRUE, 
                                                 stopwords = TRUE, 
                                                 stemming = TRUE, 
                                                 removeNumbers = TRUE))
test <- as.matrix(tdm)
test

# Check accuracy on test.
predict(model, newdata = test)
