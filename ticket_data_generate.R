# Obj: To predict whether an incident is L1, L2, L3 based on summary/description column.
# The dependent column has L1, L2 or L3.
# Given a new ticket predict whether it is L1, L2 or L3.

# --------------------------------------------Data load and preparing for preprocessing--------------------------
library(readxl)

# Set the current location to the folder where file is present
getwd()
setwd("D:/ticket_class/ticket_class")

# Read the excel file and put it in a data.frame df
# reference:- https://mgimond.github.io/ES218/Week02b.html
db <- read_excel("D:/ticket_class/db_incidents.xlsx", sheet = "Incidents")

df <- subset(db, select = c(1, 9, 11))

# Remove empty rows and NA rows.
df <- df[!apply(is.na(df) | df == "", 1, all),]

colnames(df) <- c('incidentid', 'summary', 'ticket_label')

table(df$ticket_label)
df$ticket_label <- factor(df$ticket_label,
                   levels = c('L1', 'L2', 'L3'),
                   labels = c(1, 2, 3))

df$ticket_label <- as.factor(df$ticket_label)


# Input 2. Set the fractions of the dataframe you want to split into training, 
# validation, and test.
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(df))
sampleSizeValidation <- floor(fractionValidation * nrow(df))
sampleSizeTest       <- floor(fractionTest       * nrow(df))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(df)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test.
dfTraining   <- df[indicesTraining, ]
dfValidation <- df[indicesValidation, ]
dfTest       <- df[indicesTest, ]

table(dfTraining$label)
table(dfValidation$label)
table(dfTest$label)

# Save the training, test, and validation set to disk.
con = file("dfTraining.csv","w", encoding="utf8")
write.csv(dfTraining, con, row.names=FALSE)
close(con)

con = file("dfValidation.csv","w", encoding="utf8")
write.csv(dfValidation, con, row.names=FALSE)
close(con)

dfTest$label <- NULL
con = file("dfTest.csv","w", encoding="utf8")
write.csv(dfTest, con, row.names=FALSE)
close(con)
