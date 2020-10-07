library(caret)

df_rot <- read.csv("Sound of Cities v1/Rotterdam.csv")
df_ams <- read.csv("Sound of Cities v1/Amsterdam.csv")
df_cap <- read.csv("Sound of Cities v1/Capelle.csv")
df_del <- read.csv("Sound of Cities v1/Delft.csv")
df_haag <- read.csv("Sound of Cities v1/Den_Haag.csv")
df_dor <- read.csv("Sound of Cities v1/Dordrecht.csv")
df_gou <- read.csv("Sound of Cities v1/Gouda.csv")
df_gro <- read.csv("Sound of Cities v1/Groningen.csv")
df_sch <- read.csv("Sound of Cities v1/Schiedam.csv")
df_utr <- read.csv("Sound of Cities v1/Utrecht.csv")
df_vla <- read.csv("Sound of Cities v1/Vlaardingen.csv")

df_ams[,"city"] <- sprintf("Amsterdam", seq(1:nrow(df_ams)))
df_rot[,"city"] <- sprintf("Rotterdam", seq(1:nrow(df_rot)))
df_cap[,"city"] <- sprintf("Capelle", seq(1:nrow(df_cap)))
df_del[,"city"] <- sprintf("Delft", seq(1:nrow(df_del)))
df_haag[,"city"] <- sprintf("Den Haag", seq(1:nrow(df_haag)))
df_dor[,"city"] <- sprintf("Dordrecht", seq(1:nrow(df_dor)))
df_gou[,"city"] <- sprintf("Gouda", seq(1:nrow(df_gou)))
df_gro[,"city"] <- sprintf("Groningen", seq(1:nrow(df_gro)))
df_sch[,"city"] <- sprintf("Schiedam", seq(1:nrow(df_sch)))
df_utr[,"city"] <- sprintf("Utrecht", seq(1:nrow(df_utr)))
df_vla[,"city"] <- sprintf("Vlaardingen", seq(1:nrow(df_vla)))


df_raw <- rbind(df_ams,df_cap,df_del,df_dor,df_gou,df_gro,df_haag,df_rot,df_sch,df_utr,df_vla)

df <- df_raw[, -c(1,2,3,4)]
df <- as.data.frame(df)


for (i in colnames(df)[-12]) {
  df[,i] <- as.double(df[,i])
}


cities <- c("Amsterdam", "Rotterdam", "Delft", "Dordrecht", "Gouda", "Groningen",
           "Capelle", "Den Haag", "Schiedam", "Utrecht", "Vlaardingen")

## Initialize the plot_feature_distributions function that takes in the
## arguments df and feature and returns distribution plots of the feature for
## each city.

plot_feature_distributions = function(df, feature, bins=20, plot = F) {
  
  # Check if the function should be used to plot and if so install cowplot if
  # it has not been installed already
  if (plot) {
    if (!("cowplot" %in% rownames(installed.packages()))) {
      install.packages('cowplot')
    }
  }
  
  # Atomic vector storing the plots
  plots_list = list()
  
  # For each playlist, add the distribution plot of the given feature
  for (city in cities) {
    
    # Extract the feature values for the selected playlist
    feature_values = df[df[['city']] == city, feature, drop = F]
    
    # Check if the values are discrete or continuous and append a fitting distribution plot
    if (is.numeric(feature_values)) {
      plots_list[[city]] = ggplot2::ggplot(data = feature_values, mapping = gplot2::aes_string(feature)) + ggplot2::geom_density() + ggplot2::ggtitle(paste(feature, "in", city))
    } else {
      plots_list[[city]] = ggplot2::ggplot(data = feature_values, mapping = ggplot2::aes_string(feature)) + ggplot2::geom_histogram(bins = bins) + ggplot2::ggtitle(paste(feature, "in", city))
    }
  }
  # Return the plots
  if (plot) {
    cowplot::plot_grid(plotlist = plots_list)
  } else {
    return(plots_list)
  }
}

par(mfrow = c(4,3))
## Plot all distributions of the different features per city
for (col_name in colnames(df)) {
  if (col_name != 'city') {
    print(plot_feature_distributions(df, col_name, plot = T))
  }
}


## Initialize the plot_scatter_features function that takes in the arguments df
## f1 and f2 and plots a scatter plot with f1 on the x-axis and f2 on the y-axis
## that assigns different colors to the scatter points based on the song city

plot_scatter_features = function(df, feature1, feature2) {
  plot = ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes_string(x = feature1, y = feature2, colour = 'city')
  ) + ggplot2::geom_point() + ggplot2::ggtitle(paste(feature1, 'vs', feature2, 'data distribution'))
  return(plot)
}

## Plot the scatter plots for all different combinations of features with
## scatter points having different colors based on song city
features = colnames(df)[-ncol(df)]
num_features = length(features)

# Iterate over all possible combinations of features and print the results
for (f1_ind in 1:(num_features - 1)) {
  for (f2_ind in (f1_ind + 1):num_features) {
    print(plot_scatter_features(df, features[f1_ind], features[f2_ind]))
  }
}



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


## Train your classifiers.

classifiers = list(
  "KNN" = list(method = "knn"),
  "SVC" = list(kernel = "linear"),
  "AdaBoostClassifier" = list() 
  
  
)

# Initiliaze the sets of explanatory variables and the dependent variable
X_cols = c('acousticness', 'danceability', 'energy', 'instrumentalness','loudness', 'tempo', 'liveness', 'valence', 'key', 'mode', 'speechiness')
y_col = 'city'

train_classifiers = function(classifiers, df_train, y_col, X_cols, df_test = df_train) {
  for (classifier_name in names(classifiers)) {
    
    # Extract the train control settings
    if (is.null(classifiers[[classifier_name]][['tr_control']])) {
      tr_control = caret::trainControl()
    } else {
      tr_control = classifiers[[classifier_name]][['tr_control']]
    }
    
    # Train the classifier
    classifiers[[classifier_name]][['model']] = caret::train(
      x = df_train[, X_cols],
      y = df_train[, y_col],
      method = classifiers[[classifier_name]][['method']],
      metric = 'Accuracy',
      trControl = tr_control,
      tuneGrid = classifiers[[classifier_name]][['tune_grid']],
      tuneLength = ifelse(
        is.null(classifiers[[classifier_name]][['tune_length']]),
        3,
        classifiers[[classifier_name]][['tune_length']]
      )
    )
    
    # Extract the performance metrics on the test data and store them in a matrix
    classifiers[[classifier_name]][['metrics']] = confusionMatrix(
      data = predict(
        classifiers[[classifier_name]][['model']],
        newdata = df_test[, X_cols]),
      reference = df_test[, y_col],
      mode = "prec_recall"
    )[['byClass']][ , c('Recall', 'Precision', 'F1')]
    classifiers[[classifier_name]][['metrics']] = cbind(
      substr(rownames(classifiers[[classifier_name]][['metrics']]), 7, 100),
      data.frame(classifiers[[classifier_name]][['metrics']])
    )
    rownames(classifiers[[classifier_name]][['metrics']]) = c()
    colnames(classifiers[[classifier_name]][['metrics']])[1] = 'City'
    
    # Compute and store the weighted averages of the metrics
    classifiers[[classifier_name]][['avg_metrics']] = colSums(
      aggregate(
        cbind(count = danceability) ~ city,
        data = df_test, 
        FUN = function(x) {
          NROW(x)
        }
      )[['count']] * classifiers[[classifier_name]][['metrics']][ ,c('Precision', 'Recall', 'F1')]
    ) / nrow(df_test)
    names(classifiers[[classifier_name]][['avg_metrics']]) = c('Average precision', 'Average recall', 'Average F1')
  }
  return(classifiers)
}

classifiers = train_classifiers(classifiers, df_train, y_col, X_cols, df_test)