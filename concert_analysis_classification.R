library(ROSE)
library(caret)
library(DMwR)
library(tidyverse)

df_raw <- read.csv("./EUR/De Doelen/Data/concerts.csv")
de_doelen <- read.csv("./EUR/De Doelen/Data/de_doelen v1.csv")
de_doelen <- de_doelen[, -c(7,9)]


head(df_raw)

df_descf <- df_raw[, c(1,2,3,4,17,18)]

df <- as.data.frame(df_raw[, -c(1,2,3,4,7,9,17,18)])

for (i in colnames(df)[-10]) {
  df[,i] <- as.double(df[,i])
}

df[,10] <- as.factor(df[,10])

##########################################################
## APPROACH 1 with Not Rotterdam and Rotterdam (BIVARIATE)
df_not <- df[df$city != "Rotterdam", ]
df_is <- df[df$city == "Rotterdam", ]

df_not$city <- "Not"

df2 <- rbind(df_not, df_is)


for (i in colnames(df2)[-10]) {
  df2[,i] <- as.double(df2[,i])
}

df2[,10] <- as.factor(df2[,10])


## Split the data into random train and test subsets.
# Set the seed to make your partition reproducible
set.seed(123)

# 70% of the data for training
train_size = 0.7 * nrow(df2)

# Set the seed to make your partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(df2)), size = train_size)

# Define the columns with explanatory variables and the dependent column
X_cols = c('acousticness', 'danceability', 'energy', 'instrumentalness','loudness', 'tempo', 'liveness', 'valence', 'speechiness')
y_col = 'city'

# Create the testing and training data
df2_train = df2[train_ind, c(X_cols, y_col)]
df2_test = df2[-train_ind, c(X_cols, y_col)]

# Scale the explanatory variables in the training data
df2_train_scaled = cbind(scale(df2_train[, X_cols]), df2_train[, y_col, drop = F])

# Scale the explanatory variables in the test data
df2_test_scaled = cbind(scale(df2_test[, X_cols]), df2_test[, y_col, drop = F])

#############################################################################
# Mixed sampling
set.seed(1984)
mix_train <- SMOTE(city ~., data = df2_train_scaled)
table(mix_train$city)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = T)

# Training
set.seed(1827)
model_mix <- caret::train(city ~.,
                          data = mix_train,
                          method = "regLogistic",
                          trControl = fitControl)

fit_mix <- predict(model_mix)
varimp_mix <- varImp(model_mix)
plot(varimp_mix)

results_mix <- predict(model_mix, df2_test_scaled)
logistic <- confusionMatrix(df2_test_scaled$city, results_mix, mode = "everything")


doelen_scaled <- cbind(scale(de_doelen[, X_cols]), de_doelen[, c(1:4), drop = F])
results_doel <- predict(model_mix, doelen_scaled[1:11])

view <- cbind(doelen_scaled[10:13], results_doel)

unq_rot <- view[view$results_doel == "Rotterdam",]
unq_rot <- unq_rot[!duplicated(unq_rot[,c('artist')]),]
unq_rot

unq_not <- view[view$results_doel == "Not",]
unq_not <- unq_not[!duplicated(unq_not[,c('artist')]),]
unq_not


#Coefficients
print(model_mix$finalModel$W, digits=2, lamda=0.1, weights=0.5)

set.seed(1827)
model_mix2 <- caret::train(city ~.,
                          data = mix_train,
                          method = "nnet",
                          trControl = fitControl)


fit_mix2 <- predict(model_mix2)
varimp_mix2 <- varImp(model_mix2)
plot(varimp_mix2)

results_mix2 <- predict(model_mix2, df2_test_scaled)
nnet_tot <- confusionMatrix(df2_test_scaled$city, results_mix2, mode = "everything")


set.seed(1827)
model_mix3 <- caret::train(city ~.,
                           data = mix_train,
                             method = "deepboost",
                           trControl = fitControl)


fit_mix3 <- predict(model_mix3)
varimp_mix3 <- varImp(model_mix3)
plot(varimp_mix3)

results_mix3 <- predict(model_mix3, df2_test_scaled)
cM <- confusionMatrix(df2_test_scaled$city, results_mix3, mode = "everything")
cM$overall['Accuracy']

doelen_scaled <- cbind(scale(de_doelen[, X_cols]), de_doelen[, c(1:4), drop = F])
results_doel <- predict(model_mix, doelen_scaled[1:11])

view <- cbind(doelen_scaled[1:4], results_doel)
view[view$results_doel == "Rotterdam",]


#Coefficients
model_mix$finalModel$W

####################################################
# APPROACH 2 with Rotterdam surrouding and Not surrounding
df_not_surr <- df[df$city == "Amsterdam" | df$city == "Utrecht", ]
df_surr <- df[df$city == "Rotterdam" | df$city == "Delft" | df$city == "Gouda" |
              df$city == "Dordrecht" | df$city == "The Hague", ]

df_not_surr$city <- "Not"
df_surr$city <- "Surrounding"

df_tot <- rbind(df_not_surr, df_surr)


for (i in colnames(df_tot)[-12]) {
  df_tot[,i] <- as.double(df_tot[,i])
}

df_tot[,12] <- as.factor(df_tot[,12])


## Split the data into random train and test subsets.
# Set the seed to make your partition reproducible
set.seed(123)

# 70% of the data for training
train_tot = 0.7 * nrow(df_tot)

# Set the seed to make your partition reproducible
set.seed(123)
train_ind_tot = sample(seq_len(nrow(df_tot)), size = train_tot)

# Define the columns with explanatory variables and the dependent column
X_cols = c('acousticness', 'danceability', 'energy', 'instrumentalness','loudness', 'tempo', 'liveness', 'valence', 'key', 'mode', 'speechiness')
y_col = 'city'

# Create the testing and training data
df_tot_train = df_tot[train_ind_tot, c(X_cols, y_col)]
df_tot_test = df_tot[-train_ind_tot, c(X_cols, y_col)]

# Scale the explanatory variables in the training data
df_tot_train_scaled = cbind(scale(df_tot_train[, X_cols]), df_tot_train[, y_col, drop = F])

# Scale the explanatory variables in the test data
df_tot_test_scaled = cbind(scale(df_tot_test[, X_cols]), df_tot_test[, y_col, drop = F])

#############################################################################
# Mixed sampling
set.seed(1984)
mix_tot_train <- SMOTE(city ~., data = df_tot_train_scaled)
table(mix_tot_train$city)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = T)

# Training
set.seed(1827)
model_tot_mix <- caret::train(city ~.,
                          data = mix_tot_train,
                          method = "regLogistic",
                          trControl = fitControl)

fit_tot_mix <- predict(model_tot_mix)
varimp_tot_mix <- varImp(model_tot_mix)
plot(varimp_tot_mix)

results_tot_mix <- predict(model_tot_mix, df_tot_test_scaled)
logistic_tot <- confusionMatrix(df_tot_test_scaled$city, results_tot_mix, mode = "everything")

# results_mix$Rotterdam[results_mix$Rotterdam > 0.70] <- "Not"
# results_mix$Rotterdam[results_mix$Rotterdam <= 0.70] <- "Rotterdam"
# results_mix$Rotterdam <- as.factor(results_mix$Rotterdam)


set.seed(1827)
model_tot_mix2 <- caret::train(city ~.,
                           data = mix_tot_train,
                           method = "nnet",
                           trControl = fitControl)


fit_tot_mix2 <- predict(model_tot_mix2)
varimp_tot_mix2 <- varImp(model_tot_mix2)
plot(varimp_tot_mix2)

results_tot_mix2 <- predict(model_tot_mix2, df_tot_test_scaled)
nnet <- confusionMatrix(df_tot_test_scaled$city, results_tot_mix2, mode = "everything")


set.seed(1827)
model_tot_mix3 <- caret::train(city ~.,
                           data = mix_tot_train,
                           method = "deepboost",
                           trControl = fitControl)


fit_tot_mix3 <- predict(model_tot_mix3)
varimp_tot_mix3 <- varImp(model_tot_mix3)
plot(varimp_tot_mix3)

results_tot_mix3 <- predict(model_tot_mix3, df_tot_test_scaled)
cM <- confusionMatrix(df_tot_test_scaled$city, results_tot_mix3, mode = "everything")
cM$overall['Accuracy']


results_tot_doel <- predict(model_tot_mix2, doelen_scaled[1:11])

view_tot <- cbind(de_doelen[1:4], results_tot_doel)

unq <- view_tot[view_tot$results_tot_doel == "Surrounding",]
unq <- unq[!duplicated(unq[,c('artist')]),]


#Coefficients
print(model_mix$finalModel$W, digits=2)

danceability <- list()
a <- 0
for(i in levels(df2$city)){
  a <- a + 1
  df1 <- df2 %>% filter(df2$city == i)
  danceability[i] <- mean(df1$danceability, na.rm = TRUE)
  
}

print(colMeans(df_not[, -c(12)], na.rm = TRUE), digits=1)





