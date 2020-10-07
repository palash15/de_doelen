#### Libraries ####
library(lattice)
library(tidyverse)
library(ggplot2)
library(ggmap) # for creating maps and geocoding
library(textrank) # for keyword extraction
library(udpipe) # for keyword extraction
library(mice) # for imputation
library(cluster) # for clustering (duh)
if (!require("dummies")) install.packages("dummies")
library(mclust)
library(factoextra)
library(NbClust)
library(nFactors)

# Datasets and data cleaning ####
contacts <- read_excel("Case Erasmus ContactID.v2.xlsx")
tickets <- read_excel("Case Erasmus Tickets 010117.xlsx")
location <- read_excel("Locations corrected.xlsx")
events <- read_csv("Events cleaned v2.csv")
sales_channels <- read_excel("sales_channels_dataset.xlsx")

# Contacts
contacts_clean <- contacts
contacts_clean$city <- toupper(contacts_clean$city)
contacts_clean$birthdate <- as.character(contacts_clean$birthdate)
contacts_clean$birthdate <- as.Date(contacts_clean$birthdate, "%Y-%m-%d")
contacts_clean$sex <- as.factor(contacts_clean$sex)
contacts_clean$age <- round((as.Date(Sys.time()) - contacts_clean$birthdate)/365)
contacts_clean$age <- as.numeric(contacts_clean$age)

contacts_metro <- contacts_clean[contacts_clean$city %in% c(
                                                 'DELFT',                                                 
                                                 'SCHIEDAM',                                                 
                                                 'VLAARDINGEN',
                                                 'CAP.A.D.IJSSEL',
                                                 'BARENDRECHT',
                                                 'DORDRECHT',
                                                 'RIDDERKERK',
                                                 'KRIMPEN AAN DEN IJSSEL',
                                                 'DEN HAAG'),]
contacts_metro$city <- as.factor(contacts_metro$city)

contacts_rotterdam <- contacts_clean[contacts_clean$city == 'ROTTERDAM',]
contacts_rotterdam <- contacts_rotterdam[-(86899:nrow(contacts_rotterdam)),]
contacts_rotterdam$lon <- c(rep(0,nrow(contacts_rotterdam)))
contacts_rotterdam$lat <- c(rep(0,nrow(contacts_rotterdam)))
contacts_rotterdam$zip[c(which(is.na(contacts_rotterdam$zip) == TRUE))] <- "XXX"

# Data imputation
imp <- mice(contacts_metro[, -c(1,2,4)],
            m = 3)
contacts_metro[, c(3,7)] <- complete(imp)[, c(1,4)]

imp <- mice(contacts_rotterdam[, -c(1,2,4,8,9)],
            m = 3)
contacts_rotterdam[, c(3,7)] <- complete(imp)[, c(1,4)]
rm(imp)

contacts_metro <- contacts_metro %>% filter(between(age, 14,100))
contacts_rotterdam <- contacts_rotterdam %>% filter(between(age, 14,100))

# Events
events <- events[,-4] #delete column subtitle2nl where all values are NA
# Tickets
tickets_clean <- tickets
# Deleting orders from Rotterdam Orkest from tickets data
tickets_clean$row <- c(1:nrow(tickets_clean))
sales_channels <- sales_channels[sales_channels$saleschannelid==10001,]
t <- tickets_clean[tickets_clean$orderid %in% sales_channels$id,10]
t <- t$row[1:nrow(t)]
tickets_clean <- tickets_clean[-t,]
rm(t)

# Geocoding, maps etc. ########################################################
register_google(key = "AIzaSyBtxpk_OkCGaysD6y_otq711v29pi3dzvM", write = TRUE)
# Get all zipcodes from Rotterdam
# zipcodes_rotterdam <- contacts_metro %>%
#  group_by(zip,city) %>%
#  filter(city == "ROTTERDAM") %>%
#  distinct(zip)
# zipcodes_vector <- paste(zipcodes_rotterdam$zip, zipcodes_rotterdam$city)
# zipcodes_geo <- geocode(zipcodes_vector)!DON'T USE THIS LINE, CAUSE IT WILL CHARGE MY GCLOUD ACCOUNT!
# zipcodes_rotterdam <- cbind(zipcodes_rotterdam$city, zipcodes_rotterdam$zip, zipcodes_geo)
zipcodes_rotterdam <- read_csv("zipcode_location_rotterdam.csv")
colnames(zipcodes_rotterdam)[1:2] <- c("city", "zip")
zipcodes_rotterdam$zip <- as.character(zipcodes_rotterdam$zip)
zipcodes_rotterdam$zip[is.na(zipcodes_rotterdam$zip) == TRUE] <- 'XXX'

for (i in zipcodes_rotterdam$zip){
  contacts_rotterdam[contacts_rotterdam$zip == i, 8] <- zipcodes_rotterdam[zipcodes_rotterdam$zip==i,3]
  contacts_rotterdam[contacts_rotterdam$zip == i, 9] <- zipcodes_rotterdam[zipcodes_rotterdam$zip==i,4]
}

# replace invalid lon and lat with values from similar zipcode
contacts_rotterdam[16953, 8:9] <- contacts_rotterdam[85050, 8:9] 
rotterdam_map <- get_stamenmap(bbox = c(left = 4.3, bottom = 51.8, right = 4.6, top = 52),
                               maptype = "toner",
                               zoom = 12,
                               crop = TRUE)
ggmapplot(rotterdam_map)

metro_map <- get_stamenmap(bbox = c(left = 4.25, bottom = 51.75, right = 4.7, top = 52.1),
              maptype = "terrain",
              color = "bw",
              zoom = 11,
              crop = TRUE)
ggmapplot(metro_map)


just_rdam_zipcodes <- zipcodes_rotterdam %>% filter(4.3 <= lon & lon <= 4.6,
                         51.8 <= lat & lat <= 52)
# Contact density plot
contacts_density <- qmplot(lon,
       lat, 
       data = contacts_rotterdam,
       source = "stamen",
       mapcolor = "bw",
       geom = "blank",
       zoom = 14, 
       darken = .5,
       legend = "topleft",
       xlim = c(4.4, 4.6),
       ylim = c(51.87, 51.97)) +
  stat_density_2d(aes(fill = stat(density)), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Contacts density", 
                       low = "yellow", mid = "orange", high = "red")


# Word and keyword extraction from event descriptions and titles # DONE ###############
dutch_vocab <- udpipe_download_model(language = "dutch-alpino")
model <- udpipe_load_model(file = 'dutch-alpino-ud-2.4-190531.udpipe')
event_descriptions <- udpipe_annotate(model, events$subtitlenl)
event_descriptions <- as.data.frame(event_descriptions)
event_names <- udpipe_annotate(model, events$namenl)
event_names <- as.data.frame(event_names)

description_words <- txt_freq(event_descriptions[event_descriptions$upos %in% c("NOUN", "ADJ", "PROPN"), 7])
name_words <- txt_freq(event_names[event_names$upos %in% c("NOUN", "ADJ", "PROPN"), 7])

description_keywords <- keywords_rake(x = event_descriptions,
                          term = "lemma",
                          group = "doc_id",
                          relevant = event_descriptions$upos %in% c("NOUN", "ADJ", "PROPN"))
arrange(description_keywords, desc(freq))[1:100,]

name_keywords <- keywords_rake(x = event_names,
                               term = "lemma",
                               group = "doc_id",
                               relevant = event_names$upos %in% c("NOUN", "ADJ", "PROPN"))
arrange(name_keywords, desc(freq))[1:100,]

# Selecting valuable words and keywords from descriptions
description_words_selected <- description_words[-c(1,2,3,8,9,13,14,16,17,21,22,29,30,35,36,37,38,40,41,42,
                                          43,46,47,52,53,56,57,58,60,61,63,64,69,71,72,75,77,78,
                                          80,81,84,85,86,89,91,92,93,96,97,98,99,100:nrow(description_words)),]
description_keywords_selected <- description_keywords[c(4,5,6,7,9,10:15,24,34,36,43,49,50,53,62,64,67,69,79,80,
                                                        87,88,89,93,95,96,97),]

description_keywords_selected <- description_keywords_selected[,c(1,3)]
colnames(description_keywords_selected) <- c("key", "freq")
description_words_selected <- description_words_selected[,c(1,2)]
description_selected <- rbind(description_words_selected, description_keywords_selected)
description_selected <- arrange(description_selected, desc(freq))
# Final selection and cleaning - only words that appear 10+ times
description_selected <- description_selected[-c(1,5,15,25:nrow(description_selected)),]

# Selecting valuable words and keywords from event names
name_words_selected <- name_words[c(1,4,5,6,7,8,9,11,12,15,17,18,19,20,21,22,23,24,26,27,28,
                                    29,30,31,33,34,36,40,41,42,43,45,48,51,52,55,56,58,59),1:2]
name_keywords_selected <- name_keywords[c(6,7,13,19),c(1,3)]
colnames(name_keywords_selected) <- c("key", "freq")

name_selected <- rbind(name_keywords_selected, name_words_selected)
name_selected <- arrange(name_selected, desc(freq))
# Final selection and cleaning - only words that appear 10+ times
name_selected <- name_selected[-c(40:nrow(name_selected)),]
# Merge it all to obtain a vector with 53 keywords
name_and_description_selected <- rbind(name_selected, description_selected)
name_and_description_selected <- unique(c(description_selected$key, name_selected$key))
name_and_description_selected <- sort(name_and_description_selected)
name_and_description_selected <- name_and_description_selected[-c(14,19,32,34,39,44)]

# Merging event keywords with events
# Create columns for keywords in events
events <- cbind(events, matrix(nrow = nrow(events),
                              ncol = length(name_and_description_selected),
                              data = 0,
                              dimnames = list(1:nrow(events), name_and_description_selected)))
# Keyword loop
for (i in 1:nrow(events)){
  for (j in 1:length(name_and_description_selected)){
    if (grepl(name_and_description_selected[j], events[i,2], #If keyword appears in event name or description
              ignore.case = TRUE) == TRUE |
        grepl(name_and_description_selected[j], events[i,3],
              ignore.case = TRUE) == TRUE){
      events[i,16+j] <- 1
    }
  }
  print(i)
}

# Merge Orchestra with Orkest
for (i in 1:nrow(events)){
  if (events[i,49]==1){
    events[i,48] <- events[i,49]
  }
}
events <- events[,-49]
events <- events[,-17] #Delete "2+" because it was extracted badly
colnames(events)[1] <- "eventid"

# Merging events data with tickets # DONE ################################################
temp1 <- matrix(nrow = nrow(tickets_clean),
               ncol = ncol(events),
               data = 0,
               dimnames = list(1:nrow(tickets_clean),c(colnames(events))))
temp1 <- temp1[,-1]
temp1 <- as.data.frame(temp1)
tickets_clean <- cbind(tickets_clean, temp1)
rm(temp1)
tickets_clean <- tickets_clean[,-10]
# Loop that fills ticket data based on events they are for
for (i in events$eventid){
  tickets_clean[tickets_clean$eventid==i, 10:ncol(tickets_clean)] <- events[events$eventid==i, 2:ncol(events)]
  print(which(events$eventid==i))
}
# Merging tickets with contacts # DONE ######################################################
merged <- cbind(contacts_clean, matrix(nrow=nrow(contacts_clean), ncol = ncol(tickets_clean)))
temp <- matrix(nrow = nrow(tickets_clean), 
               ncol = ncol(merged), 
               dimnames = list(1:nrow(tickets_clean),c(colnames(merged))))
merged <- rbind(merged, temp)
merged[(nrow(contacts_clean)+1):nrow(merged),1] <- tickets_clean[,7]
merged[(nrow(contacts_clean)+1):nrow(merged),8:12] <- tickets_clean[, c(2,4,5,8,9)]
colnames(merged)[8:12] <- c("price", "eventid", "status", "nbroftickets", "totalamount")
merged[(nrow(contacts_clean)+1):nrow(merged),13:ncol(merged)] <- tickets_clean[,10:ncol(tickets_clean)]
colnames(merged)[13:ncol(merged)] <- colnames(tickets_clean)[10:ncol(tickets_clean)]
merged <- merged[,-c(78:81)]
rm(temp)

# Calculate total amount spent per customer
merged <- merged %>%
  group_by(id) %>%
  mutate(totalSpent = sum(na.omit(totalamount)))

# Loop to change NAs to 0s
for (i in 8:ncol(merged)){
  merged[is.na(merged[,i] == TRUE), i] <- 0
}

merged_metro <- merged[merged$id %in% contacts_metro$id, ]
merged_rotterdam <- merged[merged$id %in% contacts_rotterdam$id, ]


# Get sum of genres and keywords per contact # DONE  ####################################################
tickets_rotterdam <- tickets_clean[tickets_clean$customerid %in% contacts_rotterdam$id, ]
tickets_metro <- tickets_clean[tickets_clean$customerid %in% contacts_metro$id, ]

temp1 <- tickets_metro %>% group_by(customerid) %>% 
  summarise_at(vars(Klassiek:Doelenmenu), sum)
temp1 <- as.matrix(temp1)
temp1 <- as.data.frame(temp1)

contacts_metro <- cbind(contacts_metro, 
                        matrix(data = 0,
                               nrow = nrow(contacts_metro),
                               ncol = 11,
                               dimnames = list(1:nrow(contacts_metro),
                                               c(colnames(tickets_metro))[14:24])))

temp2 <- tickets_rotterdam %>% group_by(customerid) %>% 
  summarise_at(vars(Klassiek:Zondagochtendconcerten), sum)
temp2 <- as.matrix(temp2)
temp2 <- as.data.frame(temp2)

contacts_rotterdam <- cbind(contacts_rotterdam, 
                            matrix(data = 0,
                                  nrow = nrow(contacts_rotterdam),
                                  ncol = 56,
                                  dimnames = list(1:nrow(contacts_rotterdam),
                                              c(colnames(tickets_rotterdam))[14:ncol(tickets_rotterdam)])))
# Working version of loop that matches sum of genres/keywords per contact 
for (i in temp1$customerid){
    contacts_metro[contacts_metro$id == i, 8:ncol(contacts_metro)] <- temp1[temp1$customerid == i, 2:ncol(temp1)]
    print(which(temp1$id == i))
}

for (i in temp2$customerid){
  contacts_rotterdam[contacts_rotterdam$id == i, 10:ncol(contacts_rotterdam)] <- temp2[temp2$customerid == i, 2:ncol(temp2)]
  print(which(temp2$id == i))
}

rm(temp1)
rm(temp2)

# Clustering and PCA of keywords ##########################################################
contacts_rotterdam_clustering <- contacts_rotterdam %>%
  dplyr::select(c(Klassiek:Doelenmenu), -Muziektheater) %>%
  mutate(events_attended = rowSums(.)) %>%
  filter(events_attended != 0)

contacts_rotterdam_clustering_id <- contacts_rotterdam %>%
  select(c(id:Doelenmenu), -Muziektheater) %>%
  mutate(events_attended = rowSums(.[10:19])) %>%
  filter(events_attended != 0)

# Get percentage of events attended
for (i in 1:ncol(contacts_rotterdam_clustering)){
  contacts_rotterdam_clustering[,i] <- contacts_rotterdam_clustering[,i]/contacts_rotterdam_clustering$events_attended
}
contacts_rotterdam_clustering[is.na(contacts_rotterdam_clustering$Klassiek) == TRUE, ] <- 0
contacts_rotterdam_clustering <- contacts_rotterdam_clustering[,-ncol(contacts_rotterdam_clustering)]

# Determining the number of clusters
clustnb_kmeans <- fviz_nbclust(x = contacts_rotterdam_clustering, #5 clusters
                                FUNcluster = kmeans,
                                method = "gap_stat")
clustnb_kmeans2 <- fviz_nbclust(x = contacts_rotterdam_clustering, #6 clusters more compact clusters than with 5 
                                   FUNcluster = kmeans,
                                   method = "wss")
clustnb_kmeans3 <- fviz_nbclust(x = contacts_rotterdam_clustering, #10 or 5 clusters
                                FUNcluster = kmeans,
                                method = "silhouette")
# Kmeans clustering
clustering_kmeans <- kmeans(x = contacts_rotterdam_clustering,
                            centers = 5,
                            nstart = 10,
                            iter.max = 20)

fviz_cluster(object = clustering_kmeans, data = contacts_rotterdam_clustering)

contacts_rotterdam_clustering_id$cluster <- clustering_kmeans$cluster
contacts_rotterdam_clustering_id[,10:19] <- contacts_rotterdam_clustering

# Clustering - metro area
contacts_metro_clustering <- contacts_metro %>%
  select(c(Klassiek:Doelenmenu), -Muziektheater) %>%
  mutate(events_attended = rowSums(.)) %>%
  filter(events_attended != 0)

contacts_metro_clustering_id <- contacts_metro %>%
  select(c(id:Doelenmenu), -Muziektheater) %>%
  mutate(events_attended = rowSums(.[8:17])) %>%
  filter(events_attended != 0)

# Get percentage of events attended
for (i in 1:ncol(contacts_metro_clustering)){
  contacts_metro_clustering[,i] <- contacts_metro_clustering[,i]/contacts_metro_clustering$events_attended
}
contacts_metro_clustering <- contacts_metro_clustering[,-ncol(contacts_metro_clustering)]

clustering_kmeans_metro <- kmeans(x = contacts_metro_clustering,
                                  centers = 5,
                                  nstart = 10,
                                  iter.max = 20)
contacts_metro_clustering_id$cluster <- clustering_kmeans_metro$cluster

# Data exploration ########################################################
# Most popular genres
genres_sum <- events %>%
  select(c(6:16)) %>%
  summarise_all(list(sum=sum))
t(sort(genres_sum, decreasing = TRUE))
                                   
summary(contacts_clean$age) # Age: min 0, max 120, median 48
ggplot(contacts_clean, aes(x=age, fill=sex))+
  geom_histogram()+
  labs(y="Count", x="Age", 
       title="Figure 1: Distribution Age over Sex")

# Decriptives, histograms
histogram(contacts_clean$sex)
histogram(contacts_clean$age)
histogram(tickets_clean$totalamount, xlim = c(0,3000), breaks = 8000) 

#Lots of contacts between ~25-35 - worth looking into, maybe they buy tickets for parents/grandparents
#or maybe they buy for themselves, then it's worth investigating what they buy.
# How much each customer spends?
contacts_clean$totalSpent <- rep(0, nrow(contacts_clean))
contacts_clean$totalTicketsNumber <- rep(0, nrow(contacts_clean))

tickets_clean$customerid <- ifelse(is.na(tickets_clean$customerid)==TRUE, 0, tickets_clean$customerid)

restickets <- tickets_clean %>% group_by(nbroftickets) %>% summarise(Freq=n())
restickets
dev.off()
plot(restickets)
ggplot(tickets_clean, aes(x=price))+
  geom_histogram()+
  labs(y="Count", x="Price", 
       title="Figure 2: Distribution Price")


ggplot(tickets_clean, aes(x=totalamount)) +
  geom_histogram()+
  labs(y="Count", x="Total Amount Spent",
       title="Figure 3: Distribution Total Amount")

events %>% #695 events are related to the restaurants menu - might want to delete this
  mutate(name = as.factor(namenl)) %>%
  group_by(name)%>%
  summarize(count = n())%>%
  arrange(desc(count))

top_cities <- contacts_clean %>% group_by(city) %>% summarise(count=n()) %>% arrange(desc(count))
head(top_cities, 20)

##### Cluster analysis - Rotterdam ##########
clustering_kmeans$size
clustering_kmeans$cluster
rownames(clustering_kmeans$centers) <- c("Classical fans", "Easy listeners", "Jazz fans", "Family People", "Popular Crowd")
colnames(clustering_kmeans$centers)[5] <- "FamConcert"
colnames(clustering_kmeans$centers)[6] <- "PJ&Beyond"
colnames(clustering_kmeans$centers)[7] <- "MezzoMiddag"
colnames(clustering_kmeans$centers)[8] <- "PopClassx"
# Genre amount per cluster plots
par(mfrow = c(3,3))
barplot(clustering_kmeans$centers[,1], main = "Klassiek", col = "sienna1", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,2], main = "Populair", col = "violetred1", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,3], main = "Operacursus", col = "royalblue4", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,4], main = "Muziekcursus", col = "royalblue", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,5], main = "Familieconcert", col = "palegreen", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,6], main = "Pop, Jazz & Beyond", col = "slateblue", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,7], main = "Mezzo-middag", col = "wheat", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,8], main = "Pop_Classics", col = "pink", space = 0.7, cex.names = 0.8)
barplot(clustering_kmeans$centers[,10], main = "Doelenmenu", col = "tomato", space = 0.7, cex.names = 0.8)
par(mfrow = c(1,1))
barplot(clustering_kmeans$centers[,9], main = "Jazz", col = runif(1,1,20))

# Characteristics of clusters
# Classical Fans - predominantly Klassiek + Muziekcursus, Populair, Operacursus.
# Cluster size: 4102 - biggest cluster.
# Mean age: 53 years, peaks at 25-35 and 55-75. 
# 53% women, 45% men, 2% unknown.
barplot(sort(clustering_kmeans$centers[1,], decreasing = TRUE)[1:5], 
        main = "Classical Fans", col = "orange1", cex.names = 1, space = 0.7)

mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==1, 7])
mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==1, 20])

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==1, 7], 
          xlab = "Histogram of Classic Listeners age", breaks = 20, col = "orange1")

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==1, 3],
          xlab = "Classical Fans gender", col = "orange1")

# Easy listeners - predominantly Pop, Jazz & Beyond + a dose of Populair
# Cluster size: 2077
# Mean age: 43 years, normal distribution skewed towards younger audiences with peak between 25-40.
# 66% female, 32% male, 2% unknown
barplot(sort(clustering_kmeans$centers[2,], decreasing = TRUE)[1:5], 
        main = "Easy listeners", col = "green2", cex.names = 1, space = 0.7) 

mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==2, 7])
mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==2, 20])

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==2, 7], 
          xlab = "Histogram of Easy Listeners age", breaks = 20, col = "green2")

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==2, 3],
          xlab = "Easy Listeners gender", col = "green2")

# Jazz fans - predominanly Jazz, a lot of Doelenmenu, Muziekcursus and Klassiek
# also attend all events except for Familieconcert and Mezzo-middag. They seem most versatile.
# Cluster size: 954 - smallest cluster
# Mean age: 50, peaks between 25-35 and 55-70.
# 52% female, 46% male, 1% unknown
barplot(sort(clustering_kmeans$centers[3,], decreasing = TRUE)[1:5], 
        main = "Jazz fans", col = "dodgerblue", cex.names = 1, space = 0.7)

mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==3, 7])
mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==3, 20])


histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==3, 7], 
          xlab = "Histogram of Jazz Fans age", breaks = 20, col = "dodgerblue")

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==3, 3],
          xlab = "Jazz Fans gender", col = "dodgerblue")

prop.table(summary(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==3, 3]))

# Family people - predominantly Familieconcert, with a little bit of Klassik, Mezzo-middag and Populair
# Cluster size: 1332
# Mean age: 46. Very big group between 30 and 50, with peak between 35 and 45.
# 71% female, 28% male, 1% unknown
barplot(sort(clustering_kmeans$centers[4,], decreasing = TRUE)[1:5], 
        main = "Family people", col = "palevioletred1", cex.names = 1, space = 0.7,)

mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==4, 7])
mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==4, 20])


histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==4, 7], 
          xlab = "Histogram of Family People age", breaks = 20, col = "palevioletred1")

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==4, 3],
          xlab = "Family People gender", col = "palevioletred1")

prop.table(summary(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==4, 3]))

# Popular crowd - almost only Populair, with a little bit of Pop Classics and Klassiek
# Cluster size: 2817
# Mean age: 49. Quite even distribution, with peaks between 25-35 and 55-65.
# 58% female, 38% male, 4% unkown
barplot(sort(clustering_kmeans$centers[5,], decreasing = TRUE)[1:5], 
        main = "Popular crowd", col = "cyan2", cex.names = 1, space = 0.7)

mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==5, 7])
mean(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==5, 20])

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==5, 7], 
          xlab = "Histogram of Popular Crowd age", breaks = 20, col = "cyan2")

histogram(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==5, 3],
          xlab = "Popular Crowd gender", col = "cyan2")

prop.table(summary(contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==5, 3]))

#### Events and genre analysis - metro area ####
clustering_kmeans_metro$centers

# Data table containing means of age
metro_preferences <- matrix(nrow = 7,
                            ncol = 12,
                            data = 0)
for (i in 1:7){
  metro_preferences[i,] <- 
    sapply(contacts_metro_clustering_id[contacts_metro_clustering_id$city == levels(contacts_metro$city)[i], 7:18], mean)}
metro_preferences <- as.data.frame(metro_preferences)
rownames(metro_preferences) <- levels(contacts_metro$city)
colnames(metro_preferences) <- colnames(contacts_metro_clustering_id)[7:18]
metro_preferences <- round(metro_preferences, 3)
metro_preferences_scaled <- metro_preferences
metro_preferences_scaled[, 2:11] <- scale(metro_preferences[, 2:11])

# Musical preferences, mean age, mean events attended and gender distribution per city
# Capelle An Den Ijsssel
sapply(contacts_metro_clustering_id[contacts_metro_clustering_id$city == "CAP.A.D.IJSSEL", 8:18], mean)

mean(contacts_metro_clustering_id[contacts_metro_clustering_id$city == "CAP.A.D.IJSSEL", 7])
mean(contacts_metro_clustering_id[contacts_metro_clustering_id$city == "CAP.A.D.IJSSEL", 7])


##### Plotting clusters on map #####

cluster1 <- qmplot(lon,
                   lat, 
                   data = contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==1,],
                   main = "Location of Classical Fans",
                   source = "stamen",
                   mapcolor = "bw",
                   geom = "blank",
                   zoom = 14, 
                   darken = .5,
                   legend = "topleft",
                   xlim = c(4.4, 4.6),
                           ylim = c(51.87, 51.97)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Density", 
                       low = "khaki1", mid = "goldenrod2", high = "red")

cluster2 <- qmplot(lon,
                   lat, 
                   data = contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==2,],
                   main = "Location of Easy Listeners",
                   source = "stamen",
                   maptype = "toner",
                   mapcolor = "bw",
                   geom = "blank",
                   zoom = 14, 
                   darken = .5,
                   legend = "topleft",
                   xlim = c(4.4, 4.6),
                   ylim = c(51.87, 51.97)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Density", 
                       low = "darkseagreen1", mid = "chartreuse2", high = "chartreuse4")

cluster3 <- qmplot(lon,
                   lat, 
                   data = contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==3,],
                   main = "Location of Jazz Fans",
                   source = "stamen",
                   maptype = "toner",
                   mapcolor = "bw",
                   geom = "blank",
                   zoom = 14, 
                   darken = .5,
                   legend = "topleft",
                   xlim = c(4.4, 4.6),
                   ylim = c(51.87, 51.97)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Density", 
                       low = "lightblue1", mid = "dodgerblue", high = "dodgerblue4")
cluster4 <- qmplot(lon,
                   lat, 
                   data = contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==4,],
                   main = "Location of Family People",
                   source = "stamen",
                   maptype = "toner",
                   mapcolor = "bw",
                   geom = "blank",
                   zoom = 14, 
                   darken = .5,
                   legend = "topleft",
                   xlim = c(4.4, 4.6),
                   ylim = c(51.87, 51.97)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Density", 
                       low = "pink", mid = "palevioletred1", high = "palevioletred4")

cluster5 <- qmplot(lon,
                   lat, 
                   data = contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==5,],
                   main = "Location of Popular Crowd",
                   source = "stamen",
                   maptype = "toner",
                   mapcolor = "bw",
                   geom = "blank",
                   zoom = 14, 
                   darken = .5,
                   legend = "topleft",
                   xlim = c(4.4, 4.6),
                   ylim = c(51.87, 51.97)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Density", 
                       low = "cadetblue", mid = "cyan1", high = "darkcyan")

cluster5a <- qmplot(lon,
                   lat, 
                   data = contacts_rotterdam_clustering_id[contacts_rotterdam_clustering_id$cluster==5,],
                   main = "Location of Popular Crowd",
                   source = "stamen",
                   maptype = "toner",
                   mapcolor = "bw",
                   geom = "blank",
                   zoom = 14, 
                   darken = .5,
                   legend = "topleft",
                   xlim = c(4.4, 4.6),
                   ylim = c(51.87, 51.97)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA, n = 256) +
  scale_fill_gradient2("Density", 
                       low = "cadetblue", mid = "cyan1", high = "darkcyan")
