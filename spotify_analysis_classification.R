library(caret)

df_raw <- read.csv("./EUR/De Doelen/Data/spotify.csv")

df <- df_raw[, -c(1,2,3,4)]
df <- as.data.frame(df)

for (i in colnames(df)[-12]) {
  df[,i] <- as.double(df[,i])
}

df[,12] <- as.factor(df[,12])


## CLASSIFICATION

## Split the data into random train and test subsets.

# Set the seed to make your partition reproducible
set.seed(123)

# 70% of the data for training
train_size = 0.7 * nrow(df)

# Set the seed to make your partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(df)), size = train_size)

# Define the columns with explanatory variables and the dependent column
X_cols = c('acousticness', 'danceability', 'energy', 'instrumentalness','loudness', 'tempo', 'liveness', 'valence', 'key', 'mode', 'speechiness')
y_col = 'city'

# Create the testing and training data
df_train = df[train_ind, c(X_cols, y_col)]
df_test = df[-train_ind, c(X_cols, y_col)]

# Scale the explanatory variables in the training data
df_train_scaled = cbind(scale(df_train[, X_cols]), df_train[, y_col, drop = F])

# Scale the explanatory variables in the test data
df_test_scaled = cbind(scale(df_test[, X_cols]), df_test[, y_col, drop = F])

# Mixed sampling
set.seed(1984)
df_train <- SMOTE(city ~., data = df_train_scaled)
table(df_train$city)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
model <- train(city ~ ., data = df_train, 
                 method = "regLogistic", 
                 trControl = fitControl)
                 ## This last option is actually one
                 ## for gbm() that passes through

fit <- predict(model)
varimp <- varImp(model)
plot(varimp)

pred <- predict(model, df_test_scaled[, -12])
table(pred, df_test[,12])
confusionMatrix(df_test_scaled$city, pred, mode = "everything")

de_doelen <- read.csv("./EUR/De Doelen/Data/de_doelen v1.csv")
doelen_scaled <- cbind(scale(de_doelen[, X_cols]), de_doelen[, c(1:4), drop = F])
results_doel <- predict(model, doelen_scaled[1:11])

view <- cbind(de_doelen[1:4], results_doel)
unq <- view[view$results_doel == "Rotterdam",]
unq <- unq[!duplicated(unq[,c('artist')]),]
unq

#Coefficients
model$finalModel$W
