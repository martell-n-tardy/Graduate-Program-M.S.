#Packages
library(stats)
library(caret)
library(arules)
library(rpart)
library(rpart.plot)
library(plyr)
library(curl)
library(arulesViz)
library(datasets)
library(car)
library(e1071)
library(broom)
library(devtools)
library(psych)
library(PerformanceAnalytics)
library(neuralnet)
library(aod)
library(klaR)
library(MASS)
library(lattice)
library(ggplot2)
library(TTR)

### Getting the Data in R ###
library(readr)
read.csv(Desktop/CompleteDataset.csv)
View(CompleteDataset)

### Preprocessing of the Data ###
#CompleteDataset overview
dim(CompleteDataset)

### Preprocessing 1: Removing NAs ###
CompleteDataset[! complete.cases(CompleteDataset),]

### Preprocessing 2: Transforming data frame for clustering ###
#remove game titles as a column before clustering, instead make into row.names
Complete <- data.frame(CompleteDataset[,-1], row.names = CompleteDataset$GameTitle)
View(Complete)

#change all character <chr> columns to factor <fctr>
library(dplyr)
Complete2<-mutate_if(Complete,is.character,as.factor)
glimpse(Complete2)
Complete3<-mutate_if(Complete2,is.integer,as.double)
glimpse(Complete3)
          
#now add back the rownames, since dplyr:: removes them
rownames(Complete3)<-CompleteDataset$GameTitle
View(Complete3)
glimpse(Complete3) #only <dbl>, <fctr>, <lgl>

### Clustering Model (using PAM) ###
#Calculating distance using Gower’s Dissimilarity Distance
library(cluster)
gd=daisy(Complete3, metric = c("gower"))
distance = dist(gd)
summary(gd)

#Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  + pam_fit <- pam(gd,
                   + diss = TRUE,
                   + k = i)
  + sil_width[i] <- pam_fit$silinfo$avg.width}
plot(1:10, sil_width,
       + xlab = "Number of Clusters",
       + ylab = "Silhouette Width")
lines(1:10, sil_width)

### Cluster Interpretation ###
library(dplyr)
pam_fit<-pam(gd,diss = TRUE, k = 5)
results <- Complete3 %>%
  +mutate(cluster = pam_fit$clustering)%>%
  +group_by(cluster)%>%
  +do(the_summary = summary(.))
results$the_summary

### Correlation Model: sales and game players ###
#test correlation sales and total players, award money, tournuments, users and critics score
pairs(data = CompleteDataset, ~UserScore+CriticScore+Global_Sales+ #printplots os pairs
        TournamentMoneyAwarded+TournamentTotalPlayers+TotalTournaments)

cor<-select(CompleteDataset,UserScore,CriticScore,Global_Sales,
            TournamentMoneyAwarded,TournamentTotalPlayers,TotalTournaments)
cor_test<-corr.test(cor, 
                   use    = "pairwise",
                   method = "pearson",
                   adjust = "none")

print(cor_test, short = FALSE)
#overlay pairs plot with correlation and significance
chart.Correlation(cor, 
                  method="pearson",
                  histogram=TRUE,
                  pch=16)

### Possion regression ###
continous_logical_data<-CompleteDataset[,-c(1,2,3,4,5,6,7,30,32)]

lm_sales<-glm(continous_logical_data$TournamentTotalPlayers ~.,
               data = continous_logical_data,family=poisson(link='log'))

par(mfrow = c(2, 2))
summary(lm_sales)#Poisson Regression Summary TextOutput

### CART Tree Sales by players by Tournuments by Genre ###
tree_data<-CompleteDataset[,-c(1,2,3,4,6,30)]

tree_sales<-rpart(tree_data$Global_Sales~ ., 
                               data = tree_data)
rpart.plot(tree_sales, type = 4) #needed library(rpart.plot) to run this line of code
plotcp(tree_sales)
summary(tree_sales)

#second visualization provided using number of tournament by critic and user score
tree_data2<-CompleteDataset[,c(3,4,5,7,30,31,32)]

tree_sales2<-rpart(tree_data2$Global_Sales~ ., method = "anova",
                   data = tree_data2)

plotcp(tree_sales2)
rpart.plot(tree_sales2, type = 4)
summary(tree_sales2)

### ANOVA sales by genre ###
anova_data<-CompleteDataset[,-c(1,2,3,4,5,6,30,31,32)]
anova_sales_genre<-aov(anova_data$Global_Sales~., data = anova_data, na.action =  na.omit)
summary(anova_sales_genre) #Figure 16 ----