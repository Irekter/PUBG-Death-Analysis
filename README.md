# PUBG-Death-Analysis
Machine Learning in PUBG kills and death data

Softwares used:
1. R
2. Tableau

The repo contains:
1. R code files
2. Saved ML Models
3. The pdf that has a summary of all the models
4. A ppt that is a short presentation of my analysis


Code:
---
title: "R Notebook"
output:
  html_document:
    df_print: paged
  word_document: default
---

```{r}
library(dplyr)
library(caret)
library(RWeka)
library(rpart)
library(rpart.plot)
library(class)
library(gmodels)
library(ROCR)
library(AUC)
library(gmodels)
library(e1071)
library(factoextra)
library(party)
library(RColorBrewer)
library(plyr)
library(tidyverse)
library(cluster)
library(dbscan)

library(fpc)
```

Reading all pubg data files
<!-- ```{r} -->
<!-- PUBGData <- read.csv("kill_match_stats_final_0.csv", fileEncoding="UTF-8-BOM") -->
<!-- PUBGData1 <- read.csv("kill_match_stats_final_1.csv", fileEncoding="UTF-8-BOM") -->
<!-- PUBGData2 <- read.csv("kill_match_stats_final_0.csv", fileEncoding="UTF-8-BOM") -->
<!-- PUBGData3 <- read.csv("kill_match_stats_final_1.csv", fileEncoding="UTF-8-BOM") -->
<!-- PUBGData4 <- read.csv("kill_match_stats_final_0.csv", fileEncoding="UTF-8-BOM") -->
<!-- ``` -->

<!-- ```{r} -->
<!-- head(PUBGData) -->
<!-- head(PUBGData1) -->
<!-- head(PUBGData2) -->
<!-- head(PUBGData3) -->
<!-- head(PUBGData4) -->
<!-- ``` -->

Checking if all data frames have column names in the correct order 
<!-- ```{r} -->
<!-- colnames(PUBGData) -->
<!-- colnames(PUBGData1) -->
<!-- colnames(PUBGData2) -->
<!-- colnames(PUBGData3) -->
<!-- colnames(PUBGData4) -->
<!-- ``` -->

Combining all data frames to one 
<!-- ```{r} -->
<!-- PUBG <- bind_rows(PUBGData,PUBGData1,PUBGData2,PUBGData3,PUBGData4) -->
<!-- str(PUBG) -->
<!-- ``` -->


Removing deaths that occured by dying in the blue zone, falling or down and out 
<!-- ```{r} -->
<!-- PUBGData <- subset(PUBGData, killed_by!="Bluezone" ) -->
<!-- PUBGData <- subset(PUBGData, killed_by!="Falling" ) -->
<!-- PUBGData <- subset(PUBGData, killed_by!="Down and Out" ) -->

<!-- PUBGData1 <- subset(PUBGData1, killed_by!="Bluezone" ) -->
<!-- PUBGData1 <- subset(PUBGData1, killed_by!="Falling" ) -->
<!-- PUBGData1 <- subset(PUBGData1, killed_by!="Down and Out" ) -->

<!-- PUBGData2 <- subset(PUBGData2, killed_by!="Bluezone" ) -->
<!-- PUBGData2 <- subset(PUBGData2, killed_by!="Falling" ) -->
<!-- PUBGData2 <- subset(PUBGData2, killed_by!="Down and Out" ) -->

<!-- PUBGData3 <- subset(PUBGData3, killed_by!="Bluezone" ) -->
<!-- PUBGData3 <- subset(PUBGData3, killed_by!="Falling" ) -->
<!-- PUBGData3 <- subset(PUBGData3, killed_by!="Down and Out" ) -->

<!-- PUBGData4 <- subset(PUBGData4, killed_by!="Bluezone" ) -->
<!-- PUBGData4 <- subset(PUBGData4, killed_by!="Falling" ) -->
<!-- PUBGData4 <- subset(PUBGData4, killed_by!="Down and Out" ) -->
<!-- ``` -->


separating data based on maps 
<!-- ```{r} -->
<!-- PUBGDataMiramar <- subset(PUBGData, map!= "ERANGEL") -->
<!-- PUBGDataErangel <- subset(PUBGData, map!= "MIRAMAR") -->

<!-- PUBGDataMiramar1 <- subset(PUBGData1, map!= "ERANGEL") -->
<!-- PUBGDataErangel1 <- subset(PUBGData1, map!= "MIRAMAR") -->

<!-- PUBGDataMiramar2 <- subset(PUBGData2, map!= "ERANGEL") -->
<!-- PUBGDataErangel2 <- subset(PUBGData2, map!= "MIRAMAR") -->

<!-- PUBGDataMiramar3 <- subset(PUBGData3, map!= "ERANGEL") -->
<!-- PUBGDataErangel3 <- subset(PUBGData3, map!= "MIRAMAR") -->

<!-- PUBGDataMiramar4 <- subset(PUBGData4, map!= "ERANGEL") -->
<!-- PUBGDataErangel4 <- subset(PUBGData4, map!= "MIRAMAR") -->
<!-- ``` -->


Combining all the 4 data sets to one 
<!-- ```{r} -->
<!-- PUBGErangel <- bind_rows(PUBGDataErangel,PUBGDataErangel1,PUBGDataErangel2,PUBGDataErangel3, PUBGDataErangel4) -->
<!-- PUBGMiramar <- bind_rows(PUBGDataMiramar,PUBGDataMiramar1,PUBGDataMiramar2,PUBGDataMiramar3, PUBGDataMiramar4) -->
<!-- ``` -->


<!-- ```{r} -->
<!-- write.csv(PUBGErangel, "pubgerangeldata.csv") -->
<!-- ``` -->

writing the whole erangel dataset to .csv file
# ```{r}
# bupg<-read.csv("pubgerangeldata.csv", na.strings = c("NA"), stringsAsFactors = FALSE)
# ```



*Data cleaning* 
# ```{r}
# killdata <- subset(bupg, select = c(killer_position_x, killer_position_y, victim_position_x, victim_position_y, killed_by, time))
# ```


See different unique values in the "killed by" column. These will give us the modes in which the victim was killed 
# ```{r}
# table(killdata$killed_by)
# ```
As we can see that different guns, vehicles and explosives were used for killing the victim


We remove N/A values, positional values which have 0.0 and weapons we cannot identify  
# ```{r}
# #table(killdata$killed_by)
# #which(is.na(killdata))
# killdata <- subset(killdata, killer_position_x != "NA" & killer_position_y != "NA" & victim_position_x != "NA" & victim_position_y != "NA" & killer_position_x != 0.0 & killer_position_y != 0.0 & victim_position_x != 0.0 & victim_position_y != 0.0 & killed_by != "RedZone" & killed_by != "Drown" & killed_by != "death.RedZoneBomb_C" & killed_by != "death.Buff_FireDOT_C" & killed_by != "death.None" & killed_by != "death.PlayerMale_A_C" )
# which(is.na(killdata))
```


We write a new .csv file which contains the cleaned data 
# ```{r}
# write.csv(killdata, "cleanerangel.csv")
# ```


Reading the cleaned data
```{r}
cleandata <- read.csv("cleanerangel.csv", header = TRUE, stringsAsFactors = FALSE)
```


Data Manupilation process
```{r}
#feature engineering: deriving a feature Here we will derive four features
cleandata$GunType <- 0
cleandata$killerelevation <- 0
cleandata$victimelevation <- 0
cleandata$killdistance <- 0


mysample <- cleandata #Creating a new data frame to keep the original intact 


#Creating vectors that hold the names of the guns. These are derived from the "killed_by" column
Rifle <- c("Groza", "M16A4", "Mk14", "SCAR-L", "M416", "Mini 14", "AKM", "AUG")
MG <- c("M249", "DP-28")
Sniper <- c("AWM", "Kar98k", "M24", "Crossbow", "SKS")
Explosives <- c("Grenade", "death.ProjMolotov_C", "death.ProjMolotov_DamageField_C")
Vehicles <- c("Boat", "Buggy", "Hit by Car", "Motorbike", "Motorbike (SideCar)", "Dacia", "Aquarail", "Uaz")
SMG <- c("Micro UZI", "UMP9", "Tommy Gun", "Vector", "VSS")
Melee <- c("Crowbar", "Machete", "Pan", "Sickle", "Punch", "death.WeapSawnoff_C")
Pistols <- c("P18C", "P1911", "P92", "R1895", "R45")
Shotgun <- c("S12K", "S1897", "S686")

#Classifiying guns according to their types
mysample <- within(mysample, GunType[killed_by %in% Rifle] <- ("Rifle"))
mysample <- within(mysample, GunType[killed_by %in% MG] <- ("Machine Gun"))
mysample <- within(mysample, GunType[killed_by %in% Sniper] <- ("Sniper"))
mysample <- within(mysample, GunType[killed_by %in% Explosives] <- ("Explosives"))
mysample <- within(mysample, GunType[killed_by %in% Vehicles] <- ("Vehicles"))
mysample <- within(mysample, GunType[killed_by %in% Melee] <- ("Melee"))
mysample <- within(mysample, GunType[killed_by %in% Shotgun] <- ("Shotgun"))
mysample <- within(mysample, GunType[killed_by %in% Pistols] <- ("Pistol"))
mysample <- within(mysample, GunType[killed_by %in% SMG] <- ("Sub Machine Gun"))


#Analysing the pubg map and tagging areas of the map which are hills and mountains and plateaus as "Elevated" or "Normal" for victims and killers
mysample <- within(mysample, killerelevation[killer_position_x > 462500 & killer_position_x < 500000  & killer_position_y > 110000 & killer_position_x < 131500] <- ("Elevated"))
mysample <- within(mysample, killerelevation[killer_position_x > 337500 & killer_position_x < 581500  & killer_position_y > 187500 & killer_position_x < 237500] <- ("Elevated"))
mysample <- within(mysample, killerelevation[killer_position_x > 562500 & killer_position_x < 662500  & killer_position_y > 350000 & killer_position_x < 450000] <- ("Elevated"))
mysample <- within(mysample, killerelevation[killer_position_x > 181200 & killer_position_x < 262500  & killer_position_y > 450000 & killer_position_x < 525000] <- ("Elevated"))
mysample <- within(mysample, killerelevation[killer_position_x > 462500 & killer_position_x < 631500  & killer_position_y > 600000 & killer_position_x < 750000] <- ("Elevated"))
mysample <- within(mysample, killerelevation[killer_position_x > 100000 & killer_position_x < 150000  & killer_position_y > 650000 & killer_position_x < 700000] <- ("Elevated"))
mysample <- within(mysample, killerelevation[killer_position_x > 875000 & killer_position_x < 250000  & killer_position_y > 225000 & killer_position_x < 381250] <- ("Elevated"))

mysample <- within(mysample, killerelevation[killerelevation == 0] <- ("Normal"))

mysample <- within(mysample, victimelevation[victim_position_x > 462500 & victim_position_x < 500000  & victim_position_y > 110000 & victim_position_x < 131500] <- ("Elevated"))
mysample <- within(mysample, victimelevation[victim_position_x > 337500 & victim_position_x < 581500  & victim_position_y > 187500 & victim_position_x < 237500] <- ("Elevated"))
mysample <- within(mysample, victimelevation[victim_position_x > 562500 & victim_position_x < 662500  & victim_position_y > 350000 & victim_position_x < 450000] <- ("Elevated"))
mysample <- within(mysample, victimelevation[victim_position_x > 181200 & victim_position_x < 262500  & victim_position_y > 450000 & victim_position_x < 525000] <- ("Elevated"))
mysample <- within(mysample, victimelevation[victim_position_x > 462500 & victim_position_x < 631500  & victim_position_y > 600000 & victim_position_x < 750000] <- ("Elevated"))
mysample <- within(mysample, victimelevation[victim_position_x > 100000 & victim_position_x < 150000  & victim_position_y > 650000 & victim_position_x < 700000] <- ("Elevated"))
mysample <- within(mysample, victimelevation[victim_position_x > 875000 & victim_position_x < 250000  & victim_position_y > 225000 & victim_position_x < 381250] <- ("Elevated"))

mysample <- within(mysample, victimelevation[victimelevation == 0] <- ("Normal"))




#Calculating the distance between victima nd killer
mysample <- within(mysample, killdistance[killdistance == 0] <- sqrt(((killer_position_x - victim_position_x)^2) + ((killer_position_y - victim_position_y)^2)))

```


Converting the derived features to factors for analysis
```{r}
mysample$killed_by <- as.factor(mysample$killed_by)
mysample$GunType <- as.factor(mysample$GunType)
mysample$killerelevation <- as.factor(mysample$killerelevation)
mysample$victimelevation <- as.factor(mysample$victimelevation)
```

Removing the column which contains row numbers
```{r}

mysample <- mysample[,-1]
str(mysample)
```

Creating 3 samples of 300000 each from the data set since my coputer doesn't handle so much data. We will run analysis and compare the 3 answers
```{r}
set.seed(2334)
mysample1 <- mysample[sample(1:nrow(cleandata), 30000,replace=FALSE),] 

set.seed(553)
mysample2 <- mysample[sample(1:nrow(cleandata), 30000,replace=FALSE),] 

set.seed(9876)
mysample3 <- mysample[sample(1:nrow(cleandata), 30000,replace=FALSE),] 

```


Creating another data set samples the same way but with scaled data
```{r}
set.seed(2334)
smysample1 <- mysample[sample(1:nrow(cleandata), 30000,replace=FALSE),] 

set.seed(553)
smysample2 <- mysample[sample(1:nrow(cleandata), 30000,replace=FALSE),] 

set.seed(9876)
smysample3 <- mysample[sample(1:nrow(cleandata), 30000,replace=FALSE),] 


preProcValues <- preProcess(x = smysample1,method = c("center", "scale"))
smysample1 <- predict(preProcValues, smysample1)
```


Creating histogram charts and check the frequency of the class of guns used and also check if terrain is elevated or normal
```{r}
histo_charts <- function(df, da)
{
  ggplot(df, aes(x = da)) +
    geom_bar(stat = "count") +
    labs(x = deparse((substitute(da))), y = "Count")+
    labs(fill = "survived")
}
```
Histograms for sample 1
```{r}
histo_charts(mysample1, mysample1$GunType)
histo_charts(mysample1, mysample1$killerelevation)
histo_charts(mysample1, mysample1$victimelevation)
```
Histograms for sample 2
```{r}
histo_charts(mysample2, mysample2$GunType)
histo_charts(mysample2, mysample2$killerelevation)
histo_charts(mysample2, mysample2$victimelevation)
```
Histogram for sample 3
```{r}
histo_charts(mysample3, mysample3$GunType)
histo_charts(mysample3, mysample3$killerelevation)
histo_charts(mysample3, mysample3$victimelevation)
```


Creating function for scatterplot diagram
```{r}
scatterplot_func <- function(dataa, xaxis, yaxis)
{
  ggplot(dataa, aes(x=xaxis, y=yaxis)) + geom_point()
  ggplot(dataa, aes(x=xaxis, y=yaxis)) + 
  geom_point()+
  geom_smooth(method=lm) +
  labs( y = deparse((substitute(yaxis))), x =deparse((substitute(xaxis))))
}
```

Creating scatterplots
```{r}
scatterplot_func(mysample1, mysample1$time, mysample1$GunType)
scatterplot_func(mysample1, mysample1$time, mysample1$killerelevation)
```

```{r}
scatterplot_func(mysample2, mysample2$time, mysample2$GunType)
scatterplot_func(mysample2, mysample2$time, mysample2$killerelevation)
```

```{r}
scatterplot_func(mysample3, mysample3$time, mysample3$GunType)
scatterplot_func(mysample3, mysample3$time, mysample3$killerelevation)
```

Creating one hot encoding for gun type and elevation 
```{r}

temp<- mysample1
dmy <- dummyVars("~GunType", data = temp)
onehots <- data.frame(predict(dmy, newdata = temp))
temp <- cbind(temp, onehots)

dmy1 <- dummyVars("~killerelevation", data = temp)
onehots <- data.frame(predict(dmy1, newdata = temp))
temp <- cbind(temp, onehots)

dmy2 <- dummyVars("~victimelevation", data = temp)
onehots <- data.frame(predict(dmy2, newdata = temp))
temp <- cbind(temp, onehots)



head(temp)
temp <- temp[,-5]
temp <- temp[,-6]
temp <- temp[,-6]
temp <- temp[,-6]

corval <- cor(temp) #creating a correalation matrix
round(corval,3)
```


Splitting data into test set and training set

```{r}
set.seed(250)
index1 <- createDataPartition(mysample1$GunType, p=0.75, list = FALSE)  
mysample1_train<-mysample1[index1,]
mysample1_test<-mysample1[-index1,]

set.seed(250)
index2 <- createDataPartition(mysample2$GunType, p=0.75, list = FALSE)  
mysample2_train<-mysample2[index2,]
mysample2_test<-mysample2[-index2,]

set.seed(250)
index3 <- createDataPartition(mysample3$GunType, p=0.75, list = FALSE)  
mysample3_train<-mysample3[index3,]
mysample3_test<-mysample3[-index3,]

set.seed(250)
sindex1 <- createDataPartition(smysample1$GunType, p=0.75, list = FALSE)  
smysample1_train<-mysample1[sindex1,]
smysample1_test<-mysample1[-sindex1,]

```


kNN on non scaled data
```{r}
set.seed(400)
training_control <- trainControl(method = "repeatedcv", number=10, repeats = 3)
#knn <- readRDS("./knn_salesnormalized_model.rds")
knn1 <- train(GunType ~. - killed_by, data = mysample1_train, method = "knn", metric = "Kappa",  trControl = training_control) #,

knn_pred1 <- predict(knn1, newdata = mysample1_test)

confusionMatrix(knn_pred1,mysample1_test$GunType)
saveRDS(knn, "./knn1.rds")

set.seed(777)

#knn <- readRDS("./knn_salesnormalized_model.rds")
knn2 <- train(GunType ~.  - killed_by, data = mysample2_train, method = "knn", metric = "Kappa",  trControl = training_control) #,

knn_pred2 <- predict(knn2, newdata = mysample2_test)

confusionMatrix(knn_pred2,mysample2_test$GunType)
saveRDS(knn, "./knn2.rds")

set.seed(111)

#knn <- readRDS("./knn_salesnormalized_model.rds")
knn3 <- train(GunType ~.  - killed_by, data = mysample3_train, method = "knn", metric = "Kappa",  trControl = training_control) #,

knn_pred3 <- predict(knn3, newdata = mysample3_test)

confusionMatrix(knn_pred3,mysample3_test$GunType)
saveRDS(knn, "./knn3.rds")
```

kNN on scaled data
```{r}

straining_control <- trainControl(method = "repeatedcv", number=10, repeats = 3)
#knn <- readRDS("./knn_salesnormalized_model.rds")
sknn1 <- train(GunType ~., data = smysample1_train, method = "knn", metric = "Kappa",  trControl = straining_control) #,

sknn_pred1 <- predict(sknn1, newdata = smysample1_test)

confusionMatrix(sknn_pred1,smysample1_test$GunType)


```

Rule learning on non scaled data
```{r}
set.seed(333)
JRips1 <- JRip(GunType ~ . - killed_by, data = mysample1_train)
predict_Jrips1<-predict(JRips1,mysample1_test)
summary(predict_Jrips1)
confusionMatrix(predict_Jrips1,mysample1_test$GunType)
saveRDS(JRips1, "./Jrips1.rds")


set.seed(222)
JRips2 <- JRip(GunType ~ . - killed_by, data = mysample2_train)
predict_Jrips2<-predict(JRips2,mysample2_test)
summary(predict_Jrips2)
confusionMatrix(predict_Jrips2,mysample2_test$GunType)
saveRDS(JRips2, "./Jrips2.rds")


set.seed(444)
JRips3 <- JRip(GunType ~ . - killed_by, data = mysample3_train)
predict_Jrips3<-predict(JRips3,mysample3_test)
summary(predict_Jrips3)
confusionMatrix(predict_Jrips3,mysample3_test$GunType)
saveRDS(JRips3, "./Jrips3.rds")
```

Rule learning on scaled data
```{r}
set.seed(333)
sJRips1 <- JRip(GunType ~ . - killed_by, data = smysample1_train)
spredict_Jrips1<-predict(sJRips1,smysample1_test)
summary(spredict_Jrips1)
confusionMatrix(spredict_Jrips1,smysample1_test$GunType)


sJRips1
```

Rparts on non scaled data
```{r}
set.seed(101)
r_ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 3)

rparts1 <- train(GunType ~ .  - killed_by, data = mysample1_train, method = "rpart", trControl = r_ctrl, tuneLength = 5, metric = "Kappa")
rparts_pred1 <- predict(rparts1, newdata = mysample1_test)
confusionMatrix(rparts_pred1, mysample1_test$GunType)
saveRDS(rparts1, "./rparts1")

set.seed(9811)
rparts2 <- train(GunType ~ .  - killed_by, data = mysample2_train, method = "rpart", trControl = r_ctrl, tuneLength = 5, metric = "Kappa")
rparts_pred2 <- predict(rparts2, newdata = mysample2_test)
confusionMatrix(rparts_pred2, mysample2_test$GunType)
saveRDS(rparts2, "./rparts2")

set.seed(5975)
rparts3 <- train(GunType ~ .  - killed_by, data = mysample3_train, method = "rpart", trControl = r_ctrl, tuneLength = 5, metric = "Kappa")
rparts_pred3 <- predict(rparts3, newdata = mysample3_test)
confusionMatrix(rparts_pred3, mysample3_test$GunType)
saveRDS(rparts3, "./rparts3")
```
R parts on scaled data
```{r}
set.seed(101)
sr_ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 3)

srparts1 <- train(GunType ~ .  - killed_by, data = smysample1_train, method = "rpart", trControl = sr_ctrl, tuneLength = 5, metric = "Kappa")
srparts_pred1 <- predict(srparts1, newdata = smysample1_test)
confusionMatrix(srparts_pred1, smysample1_test$GunType)
#saveRDS(rparts1, "./rparts1")

rpart.plot(srparts1$finalModel)
```


SVM on scaled data
```{r}
set.seed(102)
svm1<- svm(GunType~.  - killed_by, data = mysample1_train, scale=TRUE, kernel = "linear", cross = 3)

svm_predict1<-predict(svm1,mysample1_test)
confusionMatrix(svm_predict1,mysample1_test$GunType)
saveRDS(svm1, "./svm1.rds")

set.seed(1231)
svm2<- svm(GunType~.  - killed_by, data = mysample2_train, scale=TRUE, kernel = "radial", cross = 3)

svm_predict2<-predict(svm2,mysample2_test)
confusionMatrix(svm_predict2,mysample2_test$GunType)
saveRDS(svm2, "./svm2.rds")

set.seed(7000)
svm3<- svm(GunType~.  - killed_by, data = mysample3_train, scale=TRUE, kernel = "radial", cross = 3)

svm_predict3<-predict(svm3,mysample3_test)
confusionMatrix(svm_predict3,mysample3_test$GunType)
saveRDS(svm3, "./svm3.rds")
```
