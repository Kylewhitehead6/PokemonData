#Pokemon dataset to practice using R

#possible uses: 
#exploratory analysis of the data 
#cluster the data - type, hp, generation, bodystyle etc
#prediction: hp based on stats, type based on stats, gen based on something, hasmegaevolution etc 

library(tidyverse)
library(dplyr)
library(ggrepel)
library(cluster)    # clustering algorithms
library(factoextra)
library(clustertend)
library(FNN)
library(rpart)
library(rpart.plot)

pokemon = read.csv('~/Desktop/Pokemon/pokemon.csv')
#View(pokemon)
#total = sum of all base stats 

#Exploratory Analysis

#Histogram of HPs 
hp = hist(pokemon$HP, col = "green", ylim = c(0,250))    

#How many pokemon in each generation? 
gen = ggplot(data = pokemon, mapping = aes(Generation, colour = Generation)) + 
  geom_bar(width = 0.25)
gen

#how many pokemon of each type 1? 
levels(pokemon$Type_1)
type1 = ggplot(pokemon, aes(Type_1, colour = Type_1)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
type1

#how many pokemon of each type 2?
type2 = ggplot(pokemon, aes(Type_2, colour = Type_2)) + 
  geom_bar() + 
  theme(axis.text = element_text(angle = 90, hjust = 1))
type2

#type 1s per genertion 
#count
ggplot(pokemon) + 
  aes(x = Generation, fill = Type_1) + 
  geom_bar(position = "fill") + 
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(.5))
#or 
#percentage
gen_type1 = pokemon%>%
  group_by(Generation) %>% 
  count(Type_1) %>%
  mutate(ratio=scales::percent(n/sum(n)))
ggplot(pokemon) + 
  aes(x = Generation, fill = Type_1) + 
  geom_bar(position = "fill") + 
  geom_text(data = gen_type1, aes(y = n, label = ratio), position=position_fill(vjust=0.5), size = 2.5)
#percentage but match color with type
#outline box colour
ggplot(pokemon) + 
  aes(x = Generation, fill = Type_1) + 
  geom_bar(position = 'fill', color = 'blue') + # color in here outline each box with the colour - looks pretty cool actually
  geom_text(data = gen_type1, aes(y = n, label = ratio), position=position_fill(vjust=0.5), size = 2.5)
#fill box colour - not so useful but good to know
ggplot(pokemon) + 
  aes(x = Generation, fill = Type_1) + 
  geom_bar(position = 'fill', fill = 'blue') + # fill in here outline each box with the colour
  geom_text(data = gen_type1, aes(y = n, label = ratio), position=position_fill(vjust=0.5), size = 2.5)
#match type with colour - thats pretty cool
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(pokemon) + 
  aes(x = Generation, fill = Type_1) + 
  geom_bar(position = 'fill', color = 'gray40') +
  geom_text(data = gen_type1, aes(y = n, label = ratio), position=position_fill(vjust=0.5), size = 2.5) + 
  scale_fill_manual("legend", values = c('Bug' = 'yellowgreen', 'Dark' ='gray22', 'Dragon' = 'lightslateblue', 'Electric' = 'gold1', 'Fairy' = 'light pink', 'Fighting' = 'firebrick4', 'Fire' = 'chocolate1', 'Flying' = 'mediumpurple1','Ghost' = 'mediumpurple4', 'Grass' = 'chartreuse2', 'Ground' = 'gold3', 'Ice' = 'powderblue', 'Normal' = 'cornsilk3', 'Poison' = 'magenta4', 'Psychic' = 'violetred1', 'Rock' = 'goldenrod3', 'Steel' = 'lightsteelblue', 'Water' = 'steelblue2')) + 
  ggtitle("Proportion of Types per Pokemon Generation") + 
  xlab("Generation") + ylab("Proportions") 


#Defense vs Attack
#We've labbeled those with attack and defense greater than 125

ggplot(pokemon, aes(x=Attack, y=Defense)) +
  geom_point(color = 'blue', size = 1) + # Show dots
  geom_label(
    data=pokemon %>% 
      filter(Attack>125 & Defense>125), # Filter data first
    aes(label=Name,
        hjust = 0, vjust = 0), label.size = 0.01) 
#better way of doing it
attdef = ggplot(pokemon, aes(x=Attack, y=Defense)) +
  geom_point(color = 'blue', size = 1)  # Show dots
attdef + 
  geom_label_repel(data = pokemon %>%
                     filter(Attack > 125 & Defense > 125), 
                   aes(label = Name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50', 
                   size = 2) +
  theme_classic()

#Sanity Check
pokemon[pokemon$Name == "Metagross",]
pokemon[pokemon$Name == "Gigalith",]


#Clustering 
# which pokemon names are alike? 
#based on their stats, 


#Hopkins statistic. 
#It is a way of measuring the cluster tendency of a data set. 
#A value close to 1 tends to indicate the data is highly clustered, 
#random data will tend to result in values around 0.5, 
#and uniformly distributed data will tend to result in values close to 0.

# Compute Hopkins statistic 

#the data can only be numeric data 
pokemonClust = pokemon[,6:11]
res <- get_clust_tendency(pokemonClust, nrow(pokemonClust)-1, graph = FALSE)
res$hopkins_stat

#kmeans clustering 
clust2 <- kmeans(pokemonClust, 2)
clust2_plot <- fviz_cluster(list(data=pokemonClust, cluster=clust2$cluster), ellipse.type="convex", geom="point", stand=FALSE, palette="Dark2", ggtheme=theme_minimal()) + labs(title = "K-means")+theme(legend.position="bottom") 
clust2_plot

#optimal number of clusters
fviz_nbclust(pokemonClust,FUNcluster=kmeans,method = "s")

#its 2: but lets try a couple more 
clust4 <- kmeans(pokemonClust, 4)
clust4_plot <- fviz_cluster(list(data=pokemonClust, cluster=clust4$cluster), ellipse.type="convex", geom="point", stand=FALSE, palette="Dark2", ggtheme=theme_minimal()) + labs(title = "K-means")+theme(legend.position="bottom") 
clust4_plot

clust6 <- kmeans(pokemonClust, 6)
clust6_plot <- fviz_cluster(list(data=pokemonClust, cluster=clust6$cluster), ellipse.type="convex", geom="point", stand=FALSE, palette="Dark2", ggtheme=theme_minimal()) + labs(title = "K-means")+theme(legend.position="bottom") 
clust6_plot

#fviz_cluster conducts a Principal component Analysis on the variables and projects the first two principal componets onto the diagram
#Dim1 reprensets one PC that accounts for 92% of the variation in the clusters, while Dim 2 is another PC that accounts foer 2.6% of the variation in the clusters
#PCA is kind of like finding the distance between values in a variable to see how close they are to one another 
##so that you can then see which variable are creating the most differences 

#Predictive Analysis


#predicting hp based on stats 

#set up the model
hp_lm = lm(HP ~ Attack + Defense + Sp_Atk + Sp_Def + Speed + Generation + Height_m + Weight_kg, data = pokemon)
#See which independent variables are most associated with predicting HP - pretty much everything is strong except generation
summary(hp_lm)

#create a train and testing sample
#we train one sample with the model and we use the model on the test data to make sure it hasnt been created to only work on one set of data
set.seed(1)
ind <- sample(c(TRUE, FALSE), nrow(pokemon), replace=TRUE, prob=c(0.5, 0.5))
pokemon_train =  pokemon[ind, ]
pokemon_test = pokemon[!ind, ]

#set up the model
hp_lm = lm(HP ~ Attack + Defense + Sp_Atk + Sp_Def + Speed + Generation + Height_m + Weight_kg, data = pokemon_train)
#predict HP in the test data based on the model in the train dataset
pokemon_test$HP_Pred = predict(hp_lm, newdata = pokemon_test, type = 'response')

#whats the mean squared error?
mse_lm = sum((pokemon_test$HP_Pred - pokemon_test$HP)^2)/sum((pokemon_test$HP -mean(pokemon_test$HP))^2)
mse_lm
#We want this to be as close to 0 as possible so its not the best model

#turns out Sp_Atk, Speed and Generation were not strong in predicting HP
summary(hp_lm)

#Heres 20 predicted HPs we had 
head(pokemon_test$HP_Pred, 20)
#Heres there actualy HP
head(pokemon_test$HP, 20)


## Nearest Neighbours 
### Need numeric variables 


mse_knn <- numeric(20) ## make a container to hold the mean squared error ratios
for (kk in c(1:20)){ ## loop through the different neighbourhood sizes
  hp_knn <- knn.reg(train=as.data.frame(pokemon_train[,c("Attack", "Defense", "Sp_Atk", "Sp_Def", "Speed", "Generation", "Weight_kg", "Height_m")]),
                    test=as.data.frame(pokemon_test[,c("Attack", "Defense", "Sp_Atk", "Sp_Def", "Speed", "Generation", "Weight_kg", "Height_m")]),
                    y=pokemon_train[,"HP"],k=kk,algorithm="brute") ## run the model
  mse_knn[kk] <- sum((hp_knn$pred - pokemon_train[,"HP"])^2)/
    sum((pokemon_test[,"HP"] - mean(pokemon_test[,"HP"]))^2) ## compute mse ratio
}
### Mse for knn
mse_knn
#linear regression better

#predict type with stats and other variables 
#try a tree 
type_tree = rpart(Type_1 ~ Attack + Defense + Sp_Atk + Sp_Def + Speed + Generation + Height_m + Weight_kg + Color, data = pokemon_train, method = 'class', control = rpart.control(cp = 0.01))
rpart.plot(type_tree, extra =2, box.palette = 0)
levels(pokemon$Type_1)
#basicially it means that color is the best predictor of type as its the first varaible thats split at the top
#and after that we see some splits of weight, sp_atk speed and defense which helps sort the types

#ADding a random line so i can commit a new file to github
