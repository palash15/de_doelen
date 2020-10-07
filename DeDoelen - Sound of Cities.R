###########################################################################
# Sounds of cities #
###########################################################################

# Libraries
library(reshape2)
library(ggplot2)
library(lattice)
library(grid)
library(gridExtra)
library(readr)
library(tibble)
library(knitr)
library(dplyr)
library(forcats)
library(tidyverse)
library(plyr)
library(ggthemes)
library(extrafont)
library(cowplot)
library(ggrepel)
library(ggforce)
library(ggridges)
library(shiny)

# Data
Amsterdam <- read_csv("Sound of Cities v1/Amsterdam_new.csv")
# Making mode a binary Factor and renaming the values
Amsterdam$mode <- as.factor(Amsterdam$mode)
Amsterdam$mode <- mapvalues(Amsterdam$mode,from=c("0", "1"), to=c("Minor", "Major"))
View(Amsterdam)

Delft <- read_csv("Sound of Cities v1/Delft.csv")
Delft$mode <- as.factor(Delft$mode)
Delft$mode <- mapvalues(Delft$mode,from=c("0", "1"), to=c("Minor", "Major"))
View(Delft)

Den_Haag <- read_csv("Sound of Cities v1/Den_Haag.csv")
View(Den_Haag)
Den_Haag$mode <- as.factor(Den_Haag$mode)
Den_Haag$mode <- mapvalues(Den_Haag$mode,from=c("0", "1"), to=c("Minor", "Major"))

Dordrecht <- read_csv("Sound of Cities v1/Dordrecht.csv")
Dordrecht$mode <- as.factor(Dordrecht$mode)
Dordrecht$mode <- mapvalues(Dordrecht$mode,from=c("0", "1"), to=c("Minor", "Major"))

Rotterdam <- read_csv("Sound of Cities v1/Rotterdam.csv")
Rotterdam$mode <- as.factor(Rotterdam$mode)
Rotterdam$mode <- mapvalues(Rotterdam$mode,from=c("0", "1"), to=c("Minor", "Major"))

Utrecht <- read_csv("Sound of Cities v1/Utrecht.csv")
Utrecht$mode <- as.factor(Utrecht$mode)
Utrecht$mode <- mapvalues(Utrecht$mode,from=c("0", "1"), to=c("Minor", "Major"))

Vlaardingen <- read_csv("Sound of Cities v1/Vlaardingen.csv")
Vlaardingen$mode <- as.factor(Vlaardingen$mode)
Vlaardingen$mode <- mapvalues(Vlaardingen$mode,from=c("0", "1"), to=c("Minor", "Major"))

Schiedam <- read_csv("Sound of Cities v1/Schiedam.csv")
Schiedam$mode <- as.factor(Schiedam$mode)
Schiedam$mode <- mapvalues(Schiedam$mode,from=c("0", "1"), to=c("Minor", "Major"))

Capelle <- read_csv("Sound of Cities v1/Capelle.csv")
Capelle$mode <- as.factor(Capelle$mode)
Capelle$mode <- mapvalues(Capelle$mode,from=c("0", "1"), to=c("Minor", "Major"))

Doelen <- read_csv("de_doelen v1.csv")
View(Doelen)
Doelen$mode <- as.factor(Doelen$mode)
Doelen$mode <- mapvalues(Doelen$mode,from=c("0", "1"), to=c("Minor", "Major"))


ggplot(data=Capelle, mapping=aes(x=tempo))+
  geom_histogram(aes(fill=mode))+
  facet_wrap(~mode)+
  ggtitle("Capelle Minor/Major")

View(Delft)
View(Doelen)
# Calculating means for each city
# Getting rid of song name and popularity
Amsterdam_mean <- sapply(Amsterdam[,-c(1:2,4,9,16)], mean)
Delft_mean <- sapply(Delft[,-c(1:2,4,9)], mean)
Den_Haag <- sapply(Den_Haag[,-c(1:2,4,9)], mean)
Dordrecht_mean <- sapply(Dordrecht[,-c(1:2,4,9)], mean)
Rotterdam_mean <- sapply(Rotterdam[,-c(1:2,4,9)], mean)
Utrecht_mean <- sapply(Utrecht[,-c(1:2,4,9)], mean)
Vlaardingen_mean <- sapply(Vlaardingen[,-c(1:2,4,9)], mean)
Schiedam_mean <- sapply(Schiedam[,-c(1:2,4,9)], mean)
Capelle_mean <- sapply(Capelle[,-c(1:2,4,9)], mean)
Doelen_mean <- sapply(Doelen[,-c(1:2,4,9)], mean)

View(Delft_mean)

city_means <- data.frame()
city_means <- rbind(Amsterdam_mean,
                    Delft_mean,
                    Den_Haag,
                    Dordrecht_mean,
                    Rotterdam_mean,
                    Utrecht_mean,
                    Vlaardingen_mean,
                    Schiedam_mean,
                    Capelle_mean,
                    Doelen_mean)

city_means <- as.data.frame(city_means)
city_means <- rownames_to_column(city_means)
colnames(city_means)[1] <- "city"
city_means$city <- c("Amsterdam", "Delft", "Den_Haag", "Dordrecht",
                    "Rotterdam", "Utrecht", "Vlaardingen",
                     "Schiedam", "Capelle", "Doelen")
View(city_means)

#city_means_scaled <- cbind(city_means[,1], as.data.frame(scale(city_means[,2:12])))
#colnames(city_means_scaled)[1] <- "city"
#city_melted_scaled <- melt(city_means_scaled, id.vars = "city")


View(city_means)

#ggplot(data = city_melted_scaled) +
  geom_bar(mapping = aes(x = variable, y = value , fill = city),
           stat = "identity",
           position = "dodge")+
  labs(title="Figure 2: Audio Mean Features of Cities",
       y="Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#ggplot(data = city_melted) +
  geom_bar(mapping = aes(x = variable, y = value , fill = city),
           stat = "identity",
           position = "dodge")+
  labs(title="Figure 2: Audio Mean Features of Cities",
       y="Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
####################################################################################
# Rotterdam individual overview
####################################################################################
View(Rotterdam)
rot_dance <- ggplot(data=Rotterdam, aes(x=danceability)) +
  geom_histogram()+
  labs(title="Rotterdam Danceability")

rot_live <- ggplot(data=Rotterdam, aes(x=liveness)) +
  geom_histogram()+
  labs(title="Rotterdam Liveness")

rot_loud <- ggplot(data=Rotterdam, aes(x=loudness)) +
  geom_histogram()+
  labs(title="Rotterdam Loudness")

rot_ener <- ggplot(data=Rotterdam, aes(x=energy)) +
  geom_histogram()+
  labs(title="Rotterdam Energy")

rot_speech <- ggplot(data=Rotterdam, aes(x=speechiness)) +
  geom_histogram()+
  labs(title="Rotterdam Speechiness")

rot_acoustic <- ggplot(data=Rotterdam, aes(x=acousticness)) +
  geom_histogram()+
  labs(title="Rotterdam Acousticness")

rot_tempo <- ggplot(data=Rotterdam, aes(x=tempo)) +
  geom_histogram()+
  labs(title="Rotterdam Tempo")

rot_vale <- ggplot(data=Rotterdam, aes(x=valence)) +
  geom_histogram()+
  labs(title="Rotterdam Valence")

rot_instrum <- ggplot(data=Rotterdam, aes(x=instrumentalness)) +
  geom_histogram(binwidth=0.1)+
  labs(title="Rotterdam Instrumentalness")

rot_mode <- ggplot(data=Rotterdam, aes(x=mode)) +
  geom_histogram(stat="count")+
  labs(title="Rotterdam Mode")

grid.arrange(rot_acoustic, rot_dance, rot_instrum, rot_live, rot_loud,
             rot_speech, rot_tempo, rot_ener, rot_vale, rot_mode)

###################################################################################################################### 
# Spotify Feature Analysis for Cities and de Doelen
#######################

# 1. Danceabilty Plot
danceability <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-danceability) , y = danceability, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Danceability Feature",
       y="Danceability Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

danceability
# 2. Energy Plot
energy <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-energy) , y = energy, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Energy Feature",
       y="Energy Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

energy
# 3. Loudness Plot
loudness <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-loudness) , y = loudness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Loudness Feature",
       y="Loudness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

loudness

# 4. Speechiness Plot
speechiness <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-speechiness) , y = speechiness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Speechiness Feature",
       y="Speechiness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 5. Acousticness Plot
acousticness <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-acousticness) , y = acousticness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Acousticness Feature",
       y="Acousticness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 6. Instrumentalness Plot
instrumentalness <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-instrumentalness) , y = instrumentalness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Instrumentalness Feature",
       y="Instrumentalness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
# 7. Liveness Plot
liveness <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-liveness) , y = liveness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Liveness Feature",
       y="Liveness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
# 8. Valence Plot
valence <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-valence) , y = valence, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Valence Feature",
       y="Valence Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

valence

# 9. Tempo Feature
tempo <- ggplot(data = city_means) +
  geom_bar(mapping = aes(x= reorder(city,-tempo) , y = tempo, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Tempo Feature",
       y="Tempo Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
# 10. Popularity

popularity <- ggplot(data = city_means)+
  geom_bar(mapping = aes(x=reorder(city, - popularity), y=popularity, fill=city),
           stat="identity",
           position="dodge")+coord_flip()+
  labs(title="Popularity Feature",
       y="Popularity Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 11. Key
key <- ggplot(data = city_means)+
  geom_bar(mapping = aes(x=reorder(city, - key), y=key, fill=city),
           stat="identity",
           position="dodge")+coord_flip()+
  labs(title="Key Feature",
       y="Key Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

grid.arrange(danceability, energy, loudness, speechiness, acousticness, instrumentalness,
             liveness, valence, tempo, popularity, key)



####################################
# Sound of de Doelen Individual
####################################

de_doelen_v1 <- read_csv("de_doelen v1.csv")


# indivural Features
doelen_danceability <- ggplot(data=de_doelen_v1, aes(x=danceability)) +
  geom_histogram()+
  labs(title="De Doelen Danceability")

doelen_liveness <- ggplot(data=de_doelen_v1, aes(x=liveness)) +
  geom_histogram()+
  labs(title="De Doelen Liveness")

doelen_loudness <- ggplot(data=de_doelen_v1, aes(x=loudness)) +
  geom_histogram()+
  labs(title="De Doelen Loudness")

doelen_energy <- ggplot(data=de_doelen_v1, aes(x=energy)) +
  geom_histogram()+
  labs(title="De Doelen Energy")

doelen_speechiness <- ggplot(data=de_doelen_v1, aes(x=speechiness)) +
  geom_histogram()+
  labs(title="De Doelen Speechiness")

doelen_acousticness <- ggplot(data=de_doelen_v1, aes(x=acousticness)) +
  geom_histogram()+
  labs(title="De Doelen Acousticness")

doelen_tempo <- ggplot(data=de_doelen_v1, aes(x=tempo)) +
  geom_histogram()+
  labs(title="De Doelen Tempo")

doelen_valence <- ggplot(data=de_doelen_v1, aes(x=valence)) +
  geom_histogram()+
  labs(title="De Doelen Valence")

doelen_instrumentalness <- ggplot(data=de_doelen_v1, aes(x=instrumentalness)) +
  geom_histogram(binwidth=0.1)+
  labs(title="De Doelen Instrumentalness")


doelen_mode <- ggplot(data=de_doelen_v1, aes(x=mode)) +
  geom_histogram(stat="count")+
  labs(title="De Doelen Mode")
doelen_mode


grid.arrange(doelen_acousticness, doelen_danceability, doelen_instrumentalness, doelen_liveness,
             doelen_loudness, doelen_speechiness, doelen_tempo, doelen_valence, doelen_energy, doelen_mode)



count(artists)

length(de_doelen_v1$artist)
length(unique(de_doelen_v1$artist))
table(de_doelen_v1$artist)
sort(count(de_doelen_v1$artist), decreasing = T)
min(count(de_doelen_v1$artist))

length(unique(de_doelen_v1$genres))
sort(table(de_doelen_v1$genres), decreasing = T)



##################################################################################################
# Concert Data from Songkick 
##################################################################################################
concerts <- read_csv("concerts.csv")
concerts$city <- as.factor(concerts$city)

concerts_amsterdam <- concerts[concerts$city == "Amsterdam",] # 3,151
concerts_delft <- concerts[concerts$city == "Delft",] # 45
concerts_dordrecht <- concerts[concerts$city == "Dordrecht",] # 187
concerts_gouda <- concerts[concerts$city == "Gouda",] # 143
concerts_rotterdam <- concerts[concerts$city == "Rotterdam",] # 1,112
concerts_hague <- concerts[concerts$city == "The Hague",] # 412
concerts_utrecht <- concerts[concerts$city == "Utrecht",] # 1,417
concerts_vlaardingen <- concerts[concerts$city == "Vlaardingen",] #1,105
Doelen <- read_csv("de_doelen v1.csv")

View(concerts_amsterdam)

concerts_amsterdam_mean <- sapply(concerts_amsterdam[,-c(1:2,4,9,16:18)], mean)
concerts_delft_mean <- sapply(concerts_delft[,-c(1:2,4,9,16:18)], mean)
concerts_dordrecht_mean <- sapply(concerts_dordrecht[,-c(1:2,4,9,16:18)], mean)
concerts_gouda_mean <- sapply(concerts_gouda[,-c(1:2,4,9,16:18)], mean)
concerts_rotterdam_mean <- sapply(concerts_rotterdam[,-c(1:2,4,9,16:18)], mean)
concerts_hague_mean <- sapply(concerts_hague[,-c(1:2,4,9,16:18)], mean)
concerts_utrecht_mean <- sapply(concerts_utrecht[,-c(1:2,4,9,16:18)], mean)
concerts_vlaardingen_mean <- sapply(concerts_vlaardingen[,-c(1:2,4,9,16:18)], mean)
Doelen_mean <- sapply(Doelen[,-c(1:2,4,9)], mean)

concerts_means <- data.frame()
concerts_means <- rbind(concerts_amsterdam_mean,
                    concerts_delft_mean,
                    concerts_dordrecht_mean,
                    concerts_gouda_mean,
                    concerts_rotterdam_mean,
                    concerts_hague_mean,
                    concerts_utrecht_mean,
                    concerts_vlaardingen_mean,
                    Doelen_mean)

concerts_means <- as.data.frame(concerts_means)
concerts_means <- rownames_to_column(concerts_means)
colnames(concerts_means)[1] <- "city"
concerts_means$city <- c("Amsterdam", "Delft", "Dordrecht",
                     "Gouda", "Rotterdam", "Hague",
                     "Utrecht", "Vlaardingen", "Doelen")


View(concerts_means)

# 1. Danceabilty Plot
danceability <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-danceability) , y = danceability, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Danceability Feature",
       y="Danceability Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

danceability
# 2. Energy Plot
energy <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-energy) , y = energy, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Energy Feature",
       y="Energy Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

energy
# 3. Loudness Plot
loudness <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-loudness) , y = loudness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Loudness Feature",
       y="Loudness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

loudness

# 4. Speechiness Plot
speechiness <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-speechiness) , y = speechiness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Speechiness Feature",
       y="Speechiness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 5. Acousticness Plot
acousticness <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-acousticness) , y = acousticness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Acousticness Feature",
       y="Acousticness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 6. Instrumentalness Plot
instrumentalness <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-instrumentalness) , y = instrumentalness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Instrumentalness Feature",
       y="Instrumentalness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
# 7. Liveness Plot
liveness <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-liveness) , y = liveness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Liveness Feature",
       y="Liveness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
# 8. Valence Plot
valence <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-valence) , y = valence, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Valence Feature",
       y="Valence Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

valence

# 9. Tempo Feature
tempo <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-tempo) , y = tempo, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Tempo Feature",
       y="Tempo Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

grid.arrange(danceability, energy, loudness, speechiness, acousticness, instrumentalness,
             liveness, valence, tempo)


# 10. Popularity
popularity <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-popularity) , y = popularity, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Popularity Feature",
       y="Popularity Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
popularity


# 11. Key
key <- ggplot(data = concerts_means) +
  geom_bar(mapping = aes(x= reorder(city,-key) , y = key, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Key Feature",
       y="Key Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
key


grid.arrange(danceability, energy, loudness, speechiness, acousticness, instrumentalness,
             liveness, valence, tempo, popularity, key)

######################################################################################################
# Combine All Audio Features in One Plot
#######################################################################################################

# Data Spotify  Sound of City and artists De Doelen
Amsterdam <- read_csv("Sound of Cities v1/Amsterdam_new.csv")
Delft <- read_csv("Sound of Cities v1/Delft.csv")
Den_Haag <- read_csv("Sound of Cities v1/Den_Haag.csv")
Dordrecht <- read_csv("Sound of Cities v1/Dordrecht.csv")
Rotterdam <- read_csv("Sound of Cities v1/Rotterdam.csv")
Utrecht <- read_csv("Sound of Cities v1/Utrecht.csv")
Vlaardingen <- read_csv("Sound of Cities v1/Vlaardingen.csv")
Schiedam <- read_csv("Sound of Cities v1/Schiedam.csv")
Capelle <- read_csv("Sound of Cities v1/Capelle.csv")
Doelen <- read_csv("de_doelen v1.csv")
View(Doelen)
# Calculating means for each city
# Getting rid of song name and popularity
Amsterdam_mean <- sapply(Amsterdam[,-c(1:2,4,9,16)], mean)
Delft_mean <- sapply(Delft[,-c(1:2,4,9)], mean)
Den_Haag <- sapply(Den_Haag[,-c(1:2,4,9)], mean)
Dordrecht_mean <- sapply(Dordrecht[,-c(1:2,4,9)], mean)
Rotterdam_mean <- sapply(Rotterdam[,-c(1:2,4,9)], mean)
Utrecht_mean <- sapply(Utrecht[,-c(1:2,4,9)], mean)
Vlaardingen_mean <- sapply(Vlaardingen[,-c(1:2,4,9)], mean)
Schiedam_mean <- sapply(Schiedam[,-c(1:2,4,9)], mean)
Capelle_mean <- sapply(Capelle[,-c(1:2,4,9)], mean)
Doelen_mean <- sapply(Doelen[,-c(1:2,4,9)], mean)

# Data Songkick
concerts <- read_csv("concerts.csv")
concerts$city <- as.factor(concerts$city)

concerts_amsterdam <- concerts[concerts$city == "Amsterdam",] # 3,151
concerts_delft <- concerts[concerts$city == "Delft",] # 45
concerts_dordrecht <- concerts[concerts$city == "Dordrecht",] # 187
concerts_gouda <- concerts[concerts$city == "Gouda",] # 143
concerts_rotterdam <- concerts[concerts$city == "Rotterdam",] # 1,112
concerts_hague <- concerts[concerts$city == "The Hague",] # 412
concerts_utrecht <- concerts[concerts$city == "Utrecht",] # 1,417
concerts_vlaardingen <- concerts[concerts$city == "Vlaardingen",] #1,105

View(concerts_amsterdam)

concerts_amsterdam_mean <- sapply(concerts_amsterdam[,-c(1:2,4,9,16:18)], mean)
concerts_delft_mean <- sapply(concerts_delft[,-c(1:2,4,9,16:18)], mean)
concerts_dordrecht_mean <- sapply(concerts_dordrecht[,-c(1:2,4,9,16:18)], mean)
concerts_gouda_mean <- sapply(concerts_gouda[,-c(1:2,4,9,16:18)], mean)
concerts_rotterdam_mean <- sapply(concerts_rotterdam[,-c(1:2,4,9,16:18)], mean)
concerts_hague_mean <- sapply(concerts_hague[,-c(1:2,4,9,16:18)], mean)
concerts_utrecht_mean <- sapply(concerts_utrecht[,-c(1:2,4,9,16:18)], mean)
concerts_vlaardingen_mean <- sapply(concerts_vlaardingen[,-c(1:2,4,9,16:18)], mean)


all_means <- data.frame()
all_means <- rbind(Amsterdam_mean,
                    Delft_mean,
                    Den_Haag,
                    Dordrecht_mean,
                    Rotterdam_mean,
                    Utrecht_mean,
                    Vlaardingen_mean,
                    Schiedam_mean,
                    Capelle_mean,
                    Doelen_mean,
                    concerts_amsterdam_mean,
                    concerts_delft_mean,
                    concerts_dordrecht_mean,
                    concerts_gouda_mean,
                    concerts_rotterdam_mean,
                    concerts_hague_mean,
                    concerts_utrecht_mean,
                    concerts_vlaardingen_mean)

all_means <- as.data.frame(all_means)
all_means <- rownames_to_column(all_means)

colnames(all_means)[1] <- "city"
all_means$city <- c("Amsterdam Spotify", "Delft Spotify", "Den_Haag Spotify", "Dordrecht Spotify",
                     "Rotterdam Spotify", "Utrecht Spotify", "VlaardingenSpotify",
                     "Schiedam Spotify", "Capelle Spotify", "Doelen", "Amsterdam Songkick", "Delft Songkick", "Dordrecht Songkick",
                     "Gouda Songkick", "Rotterdam Songkick", "Hague Songkick", "Utrecht Songkick", "Vlaardingen Songkick")



# 1. Danceabilty Plot
all_danceability <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-danceability) , y = danceability, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Danceability Feature",
       y="Danceability Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

all_danceability
# 2. Energy Plot
all_energy <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-energy) , y = energy, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Energy Feature",
       y="Energy Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

all_energy
# 3. Loudness Plot
all_loudness <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-loudness) , y = loudness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Loudness Feature",
       y="Loudness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

all_loudness

# 4. Speechiness Plot
all_speechiness <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-speechiness) , y = speechiness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Speechiness Feature",
       y="Speechiness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 5. Acousticness Plot
all_acousticness <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-acousticness) , y = acousticness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Acousticness Feature",
       y="Acousticness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 6. Instrumentalness Plot
all_instrumentalness <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-instrumentalness) , y = instrumentalness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Instrumentalness Feature",
       y="Instrumentalness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
# 7. Liveness Plot
all_liveness <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-liveness) , y = liveness, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Liveness Feature",
       y="Liveness Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
# 8. Valence Plot
all_valence <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-valence) , y = valence, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Valence Feature",
       y="Valence Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

all_valence

# 9. Tempo Feature
all_tempo <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-tempo) , y = tempo, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Tempo Feature",
       y="Tempo Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")

# 10. Popularity
all_popularity <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-popularity) , y = popularity, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Popularity Feature",
       y="Popularity Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
all_popularity


# 11. Key
all_key <- ggplot(data = all_means) +
  geom_bar(mapping = aes(x= reorder(city,-key) , y = key, fill=city),
           stat = "identity",
           position = "dodge")+coord_flip()+
  labs(title="Key Feature",
       y="Key Value", x="City")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position="none")
all_key



grid.arrange(all_danceability, all_energy, all_loudness, all_speechiness, all_acousticness, all_instrumentalness,
             all_liveness, all_valence, all_tempo)
grid.arrange(all_popularity, all_key)


###################################################################################################################################
# Genre
###################################################################################################################################

Amsterdam <- read_csv("Sound of Cities v1/Amsterdam_new.csv")
Delft <- read_csv("Sound of Cities v1/Delft.csv")
Den_Haag <- read_csv("Sound of Cities v1/Den_Haag.csv")
Dordrecht <- read_csv("Sound of Cities v1/Dordrecht.csv")
Rotterdam <- read_csv("Sound of Cities v1/Rotterdam.csv")
Utrecht <- read_csv("Sound of Cities v1/Utrecht.csv")
Vlaardingen <- read_csv("Sound of Cities v1/Vlaardingen.csv")
Schiedam <- read_csv("Sound of Cities v1/Schiedam.csv")
Capelle <- read_csv("Sound of Cities v1/Capelle.csv")
Doelen <- read_csv("de_doelen v1.csv")
View(Capelle)

# Spotify Data
Amsterdam_genre <- Amsterdam %>%
  select(genres)
Delft_genre <- Delft %>%
  select(genres)
Den_Haag_genre <- Den_Haag %>%
  select(genres)
Dordrecht_genre <- Dordrecht %>%
  select(genres)
Rotterdam_genre <- Rotterdam %>%
  select(genres)
Utrecht_genre <- Utrecht %>%
  select(genres)
Vlaardingen_genre <- Vlaardingen %>%
  select(genres)
Schiedam_genre <- Schiedam %>%
  select(genres)
Capelle_genre <- Capelle %>%
  select(genres)
Doelen_genre <- Doelen %>%
  select(genres)

names(Amsterdam_genre)[1] <- "Amsterdam_genre"
names(Delft_genre)[1] <- "Delft_genre"
names(Den_Haag_genre)[1] <- "Den_Haag_genre"
names(Dordrecht_genre)[1] <- "Dordrecht_genre"
names(Rotterdam_genre)[1] <- "Rotterdam_genre"
names(Utrecht_genre)[1] <- "Utrecht_genre"
names(Vlaardingen_genre)[1] <- "Vlaardingen_genre"
names(Schiedam_genre)[1] <- "Schiedam_genre"
names(Capelle_genre)[1] <- "Capelle_genre"
names(Doelen_genre)[1] <- "Doelen_genre"

Genre_All <- data.frame()
Genre_All <- cbind(Amsterdam_genre,
                   Delft_genre,
                   Den_Haag_genre,
                   Dordrecht_genre,
                   Rotterdam_genre,
                   Utrecht_genre,
                   Vlaardingen_genre,
                   Schiedam_genre)

View(Doelen_genre)
View(Genre_All)

######
# Text Analysis for Spotify Sound of Cities Data 
#####
Genre_All_edit <- gsub('b"|b\'|\\\\|\\"', "", Genre_All)
# Get rid of all punctuation except headline separators
Genre_All_edit <- gsub("([<>])|[[:punct:]]", "\\1", Genre_All_edit)
# transform all to lower case
Genre_All_edit<- tolower(Genre_All_edit)
View(Genre_All_edit)

library(tm)
textcorpus <- Corpus(VectorSource(Genre_All_edit))
textcorpus <- tm_map(textcorpus, removePunctuation)
dtm <- DocumentTermMatrix(textcorpus)

tdm<-TermDocumentMatrix(textcorpus,control=list(weighting=weightTf))
tdm.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.m)
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word,levels=unique(as.character(freq.df$word)))

ggplot(freq.df[1:20,], aes(x=word,y=frequency))+
  geom_bar(stat="identity",fill='darkred')+
  coord_flip()+theme_gdocs()+theme_minimal()+
  geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)+
  labs(y="Frequency", x="Word", title = "Figure X: Most Frequent Genre Words - Spotify Sound of Cities")


#####
# Doelen Genre
#####

Doelen_genre_edit<- gsub('b"|b\'|\\\\|\\"', "", Doelen_genre)
# Get rid of all punctuation except headline separators
Doelen_genre_edit <- gsub("([<>])|[[:punct:]]", "\\1", Doelen_genre_edit)
# transform all to lower case
Doelen_genre_edit<- tolower(Doelen_genre_edit)

textcorpus_doelen <- Corpus(VectorSource(Doelen_genre_edit))
textcorpus_doelen <- tm_map(textcorpus_doelen, removePunctuation)

dtm_doelen <- DocumentTermMatrix(textcorpus_doelen)
tdm_doelen<-TermDocumentMatrix(textcorpus_doelen,control=list(weighting=weightTf))
tdm.m_doelen<-as.matrix(tdm_doelen)
term.freq_doelen <-rowSums(tdm.m_doelen)
freq.df_doelen <-data.frame(word=names(term.freq_doelen),frequency=term.freq_doelen)
freq.df_doelen<-freq.df_doelen[order(freq.df_doelen[,2], decreasing=T),]
freq.df_doelen$word<-factor(freq.df_doelen$word,levels=unique(as.character(freq.df_doelen$word)))


ggplot(freq.df_doelen[1:20,], aes(x=word,y=frequency))+
  geom_bar(stat="identity")+
  coord_flip()+theme_gdocs()+ theme_minimal()+
  geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)+
  labs(y="Frequency", x="Word", title = "Figure X: Most Frequent Genre Words - Songkick Concerts")


#####
# Songkick Concerts Data
#####

concerts <- read_csv("concerts.csv")
View(concerts)

concerts_genre <- concerts %>%
  select(genres)

concerts_genre <- tolower(concerts_genre)
concerts_genre <- strsplit(concerts_genre, " ' ")
textcorpus_concerts <- Corpus(VectorSource(concerts_genre))
textcorpus_concerts <- tm_map(textcorpus_concerts, removePunctuation)
textcorpus_concerts <- tm_map(textcorpus_concerts, stripWhitespace)

dtm_concerts <- DocumentTermMatrix(textcorpus_concerts)
tdm_concerts<-TermDocumentMatrix(textcorpus_concerts,control=list(weighting=weightTf))
tdm.m_concerts<-as.matrix(tdm_concerts)
term.freq_concerts <-rowSums(tdm.m_concerts)
freq.df_concerts <-data.frame(word=names(term.freq_concerts),frequency=term.freq_concerts)
freq.df_concerts<-freq.df_concerts[order(freq.df_concerts[,2], decreasing=T),]
freq.df_concerts$word<-factor(freq.df_concerts$word,levels=unique(as.character(freq.df_concerts$word)))


ggplot(freq.df_concerts[1:20,], aes(x=word,y=frequency))+
  geom_bar(stat="identity")+
  coord_flip()+theme_gdocs()+ theme_minimal()+
  geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)+
  labs(y="Frequency", x="Word", title = "Figure X: Most Frequent Genre Words - Songkick Concerts")







###################################################################################################################################
# PCA 
###################################################################################################################################

library(tidyverse)
library(broom)

View(all_means)

all_means_reduced <- all_means %>%
  select(-city)

View(all_means_reduced)
features_PCA <- prcomp(all_means_reduced, center=TRUE, scale. = TRUE)

names(features_PCA)
summary(features_PCA)

features_PCA$rotation

std_dev <- features_PCA$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     type="b", main="Figure X: Variance Explained of Principal Components")



tidied_pca <- bind_cols(Tag=colnames(all_means_PCA),
                        tidy(features_PCA$rotation))%>%
  gather(PC, Contribution, PC1:PC11)

showPrincipalComponents <- function(PCNumber){
  tidied_pca%>%
    filter(PC==PCNumber)%>%
    top_n(20, abs(Contribution))%>%
    mutate(Tag=reorder(Tag, Contribution))%>%
    ggplot(aes(Tag, Contribution, fill=Tag))+
    geom_col(show.legend=FALSE, alpha=0.8)+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
          axis.ticks.x=element_blank())+
    labs(x="Mean Songs",
         y="Principal Component Importance")
}

pc1 <- showPrincipalComponents("PC1")
pc1
pc2 <- showPrincipalComponents("PC2")
pc2

grid.arrange(pc1, pc2)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(features_PCA)+
  theme(aspect.ratio=1)

ggbiplot(features_PCA, labels=rownames(all_means))+
  theme(aspect.ratio=1)

ggbiplot(features_PCA, labels=all_means$city)+
  theme(aspect.ratio=1)

features_origin <- c(rep("Spotify", 9), "Doelen", rep("Songkick", 8))

ggbiplot(features_PCA, ellipse = TRUE, labels=all_means$city, groups=features_origin,
         obs.scale = 1, var.scale = 1)+
  theme(aspect.ratio = 1)+
  ggtitle("Biplot of Audio Features PCA")+
  theme_minimal()+
  theme(legend.position="bottom")

ggbiplot(features_PCA, ellipse = TRUE, labels=all_means$city, groups=features_origin)+
  theme(aspect.ratio = 1)+
  ggtitle("Biplot of Audio Features PCA")+
  theme_minimal()+
  theme(legend.position="bottom")




















