library(tidyr)
library(caret)
library(corrplot)
library(car)
library(ggplot2)
library(mice)
library(dplyr)
library(lubridate)


##YANG's Code##
#Packages
install.packages('fastDummies')
library('fastDummies')
library(ggplot2)
library(dplyr)
library(tidyr)

#Importing the datasets
housing = read.csv("/Users/Desktop/housing.csv")
arrest = read.csv("/Users/Desktop/arrest data.csv")

#Dropping irrelevant columns in each dataset
housing=subset(housing, select = -c(Building.ID, Number, BBL, BIN, Census.Tract, NTA...Neighborhood.Tabulation.Area,Latitude, Longitude, Longitude..Internal.,coordinate, Latitude..Internal.))
arrest=subset(arrest, select = -c(PD_CD, PD_DESC,KY_CD, ARREST_PRECINCT,LAW_CODE,JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD, Latitude, Longitude, Latitude.Longitude,City.Council.Districts, Police.Precincts, Community.Districts, Borough.Boundaries))
View(housing)
View(arrest)
names(housing)                 
names(arrest)

#Dropping missing values
sum(is.na(arrest))
arrest <- na.omit(arrest)
sum(is.na(housing))
housing<- na.omit(housing)

#Mutating arrest dataset borough name to full name 

arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'Q', 'Queens')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'M', 'Manhattan')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'B', 'Bronx')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'S', 'State Island')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'K', 'Brooklyn')
View(arrest)

#Dropping irrelevant crime types
arrest <- arrest[!(arrest$OFNS_DESC == "ABORTION" | arrest$OFNS_DESC == "ADMINISTRATIVE CODE" | 
                     arrest$OFNS_DESC == "ADMINISTRATIVE CODES"| arrest$OFNS_DESC == "AGRICULTURE & MRKTS LAW-UNCLASSIFIED" | 
                     arrest$OFNS_DESC == "OTHER STATE LAWS" | arrest$OFNS_DESC == "OTHER STATE LAWS (NON PENAL LA" |
                     arrest$OFNS_DESC == "F.C.A. P.I.N.O.S." | arrest$OFNS_DESC == "NEW YORK CITY HEALTH CODE" | 
                     arrest$OFNS_DESC == "VEHICLE AND TRAFFIC LAWS"), ]
table(arrest$OFNS_DESC)

#Dummy coding the categorical variables
arrest <- dummy_cols(arrest, select_column = c("OFNS_DESC", "LAW_CAT_CD", "ARREST_BORO", "AGE_GROUP", "PERP_SEX", "PERP_RACE"))
housing <- dummy_cols(housing, select_columns = c("Borough", "Reporting.Construction.Type",
                                                  "Extended.Affordability.Only", "Prevailing.Wage.Status"))
View(arrest)
View(housing)

#Create visualization bar charts
ggplot(housing, aes(x=Borough))+ geom_bar(width=0.7, fill="steelblue")+ geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") + 
  print(labs(y = "Count of building project", x = "Borough")) +
  print(ggtitle("Building project visualization by Borough"))

ggplot(arrest, aes(x=ARREST_BORO))+ geom_bar(width=0.7, fill="steelblue")+ geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") + 
  print(labs(y = "Count of arrest", x = "Borough")) +
  print(ggtitle("NYPD Arrest visualization by borough"))


#Export the cleaned datasets
write.table(arrest, file = "arrest_cleaned.csv", sep = ",")
write.table(housing, file = "housing_cleaned.csv", sep = ",")



##Jenny's Workspace###
Arrest <- read.csv("~/Documents/arrest_cleaned.csv", stringsAsFactors = TRUE)
head(Arrest)

library(caret)
library(mice)
library(dplyr)
library(cluster)

#Prep arrest data for clustering (First Attempt)
Arrest2 = subset(Arrest, select = -c(ARREST_KEY, ARREST_DATE, OFNS_DESC, LAW_CAT_CD, ARREST_BORO,
                                     AGE_GROUP, PERP_SEX, PERP_RACE, year,month, Arrest_Race_Unknow,
                                     LAW_CAT_CD,LAW_CAT_CD_F,LAW_CAT_CD_I,LAW_CAT_CD_M,LAW_CAT_CD_V,
                                     OFNS_DESC_UNAUTHORIZED.USE.OF.A.VEHICLE.3..UUV.,OFNS_DESC_THEFT.FRAUD,
                                     OFNS_DESC_SEX.CRIMES,OFNS_DESC_POSSESSION.OF.STOLEN.PROPERTY.5,
                                     OFNS_DESC_OTHER.OFFENSES.RELATED.TO.THEFT,OFNS_DESC_OTHER.OFFENSES.RELATED.TO.THEF,
                                     OFNS_DESC_OFFENSES.AGAINST.THE.PERSON,OFNS_DESC_OFFENSES.AGAINST.MARRIAGE.UNCLASSIFIED,
                                     OFNS_DESC_,OFNS_DESC_ANTICIPATORY.OFFENSES,OFNS_DESC_BURGLAR.S.TOOLS,
                                     OFNS_DESC_CRIMINAL.MISCHIEF...RELATED.OFFENSES,OFNS_DESC_DISRUPTION.OF.A.RELIGIOUS.SERVICE,
                                     OFNS_DESC_ESCAPE.3,OFNS_DESC_FELONY.SEX.CRIMES,OFNS_DESC_FOR.OTHER.AUTHORITIES,
                                     OFNS_DESC_FORCIBLE.TOUCHING,OFNS_DESC_FORGERY,OFNS_DESC_FORTUNE.TELLING,
                                     OFNS_DESC_FRAUDS,OFNS_DESC_FRAUDULENT.ACCOSTING,OFNS_DESC_GAMBLING,OFNS_DESC_GRAND.LARCENY.OF.MOTOR.VEHICLE,
                                     OFNS_DESC_HARRASSMENT.2,OFNS_DESC_HOMICIDE.NEGLIGENT.UNCLASSIFIE,
                                     OFNS_DESC_INTOXICATED.IMPAIRED.DRIVING,OFNS_DESC_KIDNAPPING.AND.RELATED.OFFENSES,
                                     OFNS_DESC_LOITERING,OFNS_DESC_UNLAWFUL.POSS..WEAP..ON.SCHOOL.GROUNDS,
                                     OFNS_DESC_NYS.LAWS.UNCLASSIFIED.VIOLATION))

head(Arrest2)
summary(Arrest2)

Arrest2_sample <- sample_n(Arrest2, 50000)#random sample for 100K data pts
glimpse(Arrest2_sample)

x <- sum(is.na(Arrest2_sample)); x #No missing values
set.seed(1706)
split = createDataPartition(Arrest2_sample$OFNS_DESC_ARSON,p = 0.7, list = F, groups = 100)
train = Arrest2_sample[split,]
test = Arrest2_sample[-split,]
distances = dist(train, method="binary")
length(distances)
clust = hclust(distances, method = "complete", members = NULL)
clust2 = hclust(distances,method = "ward.D2")

clust
plot(clust)
rect.hclust(tree=clust,k = 4)
glimpse(train)


clusters = cutree(clust,k = 4)
data2 = cbind(train,clusters)
glimpse(data2)
ggplot(data=data2,aes(x=Arrest_Sex_Female,y=OFNS_DESC_BURGLARY,color=factor(clusters)))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F,color=F)+
  geom_text(aes(label=rownames(data2)),hjust=-1.5,vjust=0.5)

#Clustering with Categorical Variables 
Arrest3 = subset(Arrest, select = c(OFNS_DESC,ARREST_BORO,PERP_SEX,PERP_RACE))
glimpse(Arrest3)
Arrest3 <- Arrest3 %>% filter(OFNS_DESC != "") %>% droplevels
levels(Arrest3$OFNS_DESC)
install.packages("klaR")
library(klaR)
cluster3 <- kmodes(Arrest3_sample, 5, iter.max = 20, weighted = FALSE, fast = TRUE)
cluster3

library(dendextend)
library(ggplot2)

data2 = cbind(Arrest3_sample,kmc=cluster3$cluster)
plot(data2)


library(cluster)
install.packages("fpc")
library(fpc)



Arrest3_sample <- sample_n(Arrest3, 50000)#random sample
Arrest3_sample$OFNS_DESC <- levels(droplevels(Arrest3_sample$OFNS_DESC))

split = createDataPartition(Arrest3_sample$OFNS_DESC,p = 0.7, list = F, groups = 100)
train3 = Arrest3_sample[split,]
test3 = Arrest3_sample[-split,]

train3$OFNS_DESC<- factor(train3$OFNS_DESC)
levels(train3$OFNS_DESC)

x <- sum(is.na(Arrest3_sample)); x #No missing values
set.seed(1706)
split = createDataPartition(Arrest3_sample$OFNS_DESC,p = 0.7, list = F, groups = 100)
train3 = Arrest3_sample[split,]
test3 = Arrest3_sample[-split,]


distances3 = dist(train3, method="binary")
length(distances)
clust = hclust(distances, method = "complete", members = NULL)
clust2 = hclust(distances,method = "ward.D2")


#Random Forest ATTEMPT 1 - FAIL
library(randomForest)

set.seed(3000)

train3$OFNS_DESC<- factor(train3$OFNS_DESC)

glimpse(train3)
levels(train3$OFNS_DESC)
sum_off <- summary(train3$OFNS_DESC) 
sum_off <- sort(sum_off,decreasing = FALSE)
barchart(sum_off[56:76])
x2 <- sum(is.na(train3)); x2

class(train3$OFNS_DESC)
train3$OFNS_DESC <- droplevels(train3$OFNS_DESC)
train3$OFNS_DESC<- factor(train3$OFNS_DESC)
forest = randomForest(OFNS_DESC~., data = train3, ntree = 80)

varImpPlot(forest)
glimpse(train3)

#Random Forest with Dummy

train4 <- dummy_cols(train3, select_column = c("ARREST_BORO", "PERP_SEX"))
train4 <- subset(train4, select = -c(PERP_SEX, ARREST_BORO, ARREST_BORO_))
train4$OFNS_DESC <- droplevels(train4$OFNS_DESC)
train4$OFNS_DESC<- factor(train4$OFNS_DESC)
forest2 = randomForest(OFNS_DESC~., data = train4, ntree = 80)
varImpPlot(forest2)
summary(forest2)

glimpse(train4)

###Clustering and Forest on Combined##
Combined <- read.csv("~/Documents/combined.csv", stringsAsFactors = TRUE)
glimpse(Combined)
sum(is.na(Combined))


#Cluster Plot
Combined2 <- subset(Combined, select = -c(month,year,Arrest_Race_Other))
distances = round(dist(Combined2,method = "euclidean"),2)
clust = hclust(distances,method = "ward.D2")
plot(clust)
res.hc <- eclust(Combined2, "hclust")
fviz_dend(res.hc, rect = TRUE)
Combined2 <- na.omit(Combined2)

#Kmeans
set.seed(1000)
km = kmeans(Combined2, centers = 4,iter.max = 1000)
km
within_ss = sapply(X = 1:9, 
                   FUN = function(x) kmeans(Combined2,centers = x,iter.max = 100)$tot.withinss)

ratio_ss = sapply(X = 1:9, 
                  FUN = function(x) {
                    km = kmeans(Combined2,centers = x,iter.max = 100)
                    ratio = km$betweenss/km$totss
                    return(ratio)
                  })
dat = data.frame(clusters=1:9,within_ss, ratio_ss)
ggplot(dat,aes(x=clusters,y=within_ss))+
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
  geom_vline(xintercept=4)

km$centers
km$cluster

km2 = kmeans(Combined2, centers = 2,iter.max = 1000)
km2 <- na.omit(km2)

k_segments = km2$cluster
library(psych)
temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(Combined2,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(Combined2,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

data4 = cbind(Combined2,k_segments)
data4 %>%
  select(Housing_Bronx:Arrest_Sex_Female,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Housing_Bronx:Arrest_Sex_Female)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()


#Random Forest Model Final 
split = createDataPartition(Combined$Housing_Bronx,p = 0.7, list = F, groups = 100)
train = Combined[split,]
test = Combined[-split,]
X_data <- subset(train, select = -c(month:Housing_Staten_Island))
# Bronx
forest_Bronx = randomForest(train$Housing_Bronx~., data = X_data, ntree = 20, mtry = 30)
print(forest_Bronx)
varImpPlot(forest_Bronx)
pred_1 = predict(forest,X_data)
train$forecasted = pred_1
# mean absolute relative error
mean(abs(train$Housing_Bronx-train$forecasted))
# compare the means of predicted versus forecasted
mean(train$Housing_Bronx)
mean(train$forecasted)
pred_test = predict(forest,test)
test$forecasted = pred_test
mean(abs(test$Housing_Bronx-test$forecasted))
mean(test$Housing_Bronx)
mean(test$forecasted)

# Brooklyn

forest_Brooklyn = randomForest(train$Housing_Brooklyn~., data = X_data, ntree = 20, mtry = 30)
print(forest_Brooklyn)
varImpPlot(forest_Brooklyn)
pred_brook_train = predict(forest_brooklyn,X_data)
train$forecasted_brook = pred_brook_train
# mean absolute relative error
mean(abs(train$Housing_Brooklyn-train$forecasted_brook))
# compare the means of predicted versus forecasted
mean(train$Housing_Brooklyn)
mean(train$forecasted_brook)
pred_test_brook = predict(forest_brooklyn,test)
test$forecasted_brook = pred_test_brook
mean(abs(test$Housing_Brooklyn-test$forecasted_brook))
mean(test$Housing_Brook)
mean(test$forecasted_brook)

# Manhattan

forest_Manhattan = randomForest(train$Housing_Manhattan~., data = X_data, ntree = 20, mtry = 30)
print(forest_Manhattan)
varImpPlot(forest_Manhattan)
pred_Manhattan_train = predict(forest_Manhattan,X_data)
train$forecasted_Manhattan = pred_Manhattan_train
# mean absolute relative error
mean(abs(train$Housing_Manhattan-train$forecasted_Manhattan))
# compare the means of predicted versus forecasted
mean(train$Housing_Manhattan)
mean(train$forecasted_Manhattan)
pred_test_Manhattan = predict(forest_Manhattan,test)
test$forecasted_Manhattan = pred_test_Manhattan
mean(abs(test$Housing_Manhattan-test$forecasted_Manhattan))
mean(test$Housing_Manhattan)
mean(test$forecasted_Manhattan)

# Queens

forest_Queens = randomForest(train$Housing_Queens~., data = X_data, ntree = 20, mtry = 30)
print(forest_Queens)
varImpPlot(forest_Queens)
pred_Queens_train = predict(forest_Queens,X_data)
train$forecasted_Queens = pred_Queens_train
# mean absolute relative error
mean(abs(train$Housing_Queens-train$forecasted_Queens))
# compare the means of predicted versus forecasted
mean(train$Housing_Queens)
mean(train$forecasted_Queens)
pred_test_Queens = predict(forest_Queens,test)
test$forecasted_Queens = pred_test_Queens
mean(abs(test$Housing_Queens-test$forecasted_Queens))
mean(test$Housing_Queens)
mean(test$forecasted_Queens)

# Staten Island

forest_Staten_Island = randomForest(train$Housing_Staten_Island~., data = X_data, ntree = 20, mtry = 30)
print(forest_Staten_Island)
varImpPlot(forest_Staten_Island)
pred_Staten_Island_train = predict(forest_Staten_Island,X_data)
train$forecasted_Staten_Island = pred_Staten_Island_train
# mean absolute relative error
mean(abs(train$Housing_Staten_Island-train$forecasted_Staten_Island))
# compare the means of predicted versus forecasted
mean(train$Housing_Staten_Island)
mean(train$forecasted_Staten_Island)
pred_test_Staten_Island = predict(forest_Staten_Island,test)
test$forecasted_Staten_Island = pred_test_Staten_Island
mean(abs(test$Housing_Staten_Island-test$forecasted_Staten_Island))
mean(test$Housing_Staten_Island)
mean(test$forecasted_Staten_Island)
