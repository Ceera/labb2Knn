rm(list=ls())
graphics.off()
library(class)
library(mlbench)
data(Glass)

#set.seed(987)

print(Glass)

#Delar upp datat i två delar, man brukar arbeta med ca 70% till träning och 30% sparat till testet
ind50 <- sample(2, nrow(Glass), replace=TRUE, prob=c(1/2, 1/2))
ind66 <- sample(2, nrow(Glass), replace=TRUE, prob=c(2/3, 1/3))
ind75 <- sample(2, nrow(Glass), replace=TRUE, prob=c(3/4, 1/4))


# Kollar så fördelningen blev rätt
length(ind50[ind50==1]) #training data 
length(ind50[ind50==2]) #test data
length(ind66[ind66==1]) #training data 
length(ind66[ind66==2]) #test data
length(ind75[ind75==1]) #training data 
length(ind75[ind75==2]) #test data
nrow(Glass)

## Skapar variabler som håller de olika datasetten
## 50%
Glass.training50 <- Glass[ind50==1, 1:9]
Glass.test50 <- Glass[ind50==2, 1:9] 

## 60%
Glass.training66 <- Glass[ind66==1, 1:9]
Glass.test66 <- Glass[ind66==2, 1:9] 

## 75%
Glass.training75 <- Glass[ind75==1, 1:9]
Glass.test75 <- Glass[ind75==2, 1:9] 

## Skapar upp klasser (namnen på typerna)
# 50%
Glass.trainLabels50 <- Glass[ind50==1, 10]
Glass.testLabels50 <- Glass[ind50==2, 10]

# 66%
Glass.trainLabels66 <- Glass[ind66==1, 10]
Glass.testLabels66 <- Glass[ind66==2, 10]

# 75%
Glass.trainLabels75 <- Glass[ind75==1, 10]
Glass.testLabels75 <- Glass[ind75==2, 10]

#################KNN##############################################################################

#Sätter gränsen för närmaste grannen reglen till 3
classifiedData50 <- knn(train = Glass.training50, test = Glass.test50, cl = Glass.trainLabels50, k=3)

classifiedData66 <- knn(train = Glass.training66, test = Glass.test66, cl = Glass.trainLabels66, k=3)

classifiedData75 <- knn(train = Glass.training75, test = Glass.test75, cl = Glass.trainLabels75, k=3)

#################   Kör knn 3 med de olika % satserna:  #############################################  
#Visar i en tabel hur det gick för programmet att gissa. 
confusion50 <- table(Target = Glass.testLabels50, Predicted = classifiedData50)
confusion50
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy50 <- (sum(diag(confusion50))/sum(confusion50) )* 100
accuracy50

#Visar i en tabel hur det gick för programmet att gissa. 
confusion66 <- table(Target = Glass.testLabels66, Predicted = classifiedData66)
confusion66
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy66 <- (sum(diag(confusion66))/sum(confusion66) )* 100
accuracy66

#Visar i en tabel hur det gick för programmet att gissa. 
confusion75 <- table(Target = Glass.testLabels75, Predicted = classifiedData75)
confusion75
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy75 <- (sum(diag(confusion75))/sum(confusion75) )* 100
accuracy75

#################KNN##############################################################################

#Sätter gränsen för närmaste grannen reglen till 5
classifiedData50 <- knn(train = Glass.training50, test = Glass.test50, cl = Glass.trainLabels50, k=5)

classifiedData66 <- knn(train = Glass.training66, test = Glass.test66, cl = Glass.trainLabels66, k=5)

classifiedData75 <- knn(train = Glass.training75, test = Glass.test75, cl = Glass.trainLabels75, k=5)

#################   Kör knn 5 med de olika % satserna:  #############################################


confusion50 <- table(Target = Glass.testLabels50, Predicted = classifiedData50)
confusion50
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy50 <- (sum(diag(confusion50))/sum(confusion50) )* 100
accuracy50

#Visar i en tabel hur det gick för programmet att gissa. 
confusion66 <- table(Target = Glass.testLabels66, Predicted = classifiedData66)
confusion66
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy66 <- (sum(diag(confusion66))/sum(confusion66) )* 100
accuracy66

#Visar i en tabel hur det gick för programmet att gissa. 
confusion75 <- table(Target = Glass.testLabels75, Predicted = classifiedData75)
confusion75
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy75 <- (sum(diag(confusion75))/sum(confusion75) )* 100
accuracy75


#################KNN##############################################################################

#Sätter gränsen för närmaste grannen reglen till 5
classifiedData50 <- knn(train = Glass.training50, test = Glass.test50, cl = Glass.trainLabels50, k=7)

classifiedData66 <- knn(train = Glass.training66, test = Glass.test66, cl = Glass.trainLabels66, k=7)

classifiedData75 <- knn(train = Glass.training75, test = Glass.test75, cl = Glass.trainLabels75, k=7)

#################   Kör knn 7 med de olika % satserna:  #############################################


confusion50 <- table(Target = Glass.testLabels50, Predicted = classifiedData50)
confusion50
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy50 <- (sum(diag(confusion50))/sum(confusion50) )* 100
accuracy50

#Visar i en tabel hur det gick för programmet att gissa. 
confusion66 <- table(Target = Glass.testLabels66, Predicted = classifiedData66)
confusion66
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy66 <- (sum(diag(confusion66))/sum(confusion66) )* 100
accuracy66

#Visar i en tabel hur det gick för programmet att gissa. 
confusion75 <- table(Target = Glass.testLabels75, Predicted = classifiedData75)
confusion75
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracy75 <- (sum(diag(confusion75))/sum(confusion75) )* 100
accuracy75

################################################################################################
#####SLUT PÅ DEL 1 Körning av Knn (utan normalizering eller standardisering) med olika % #######

###############################################################################################


#### DEL 2 Standardisera glass data och kör med de lika % och med olika kNN 3-5-7####

# Gör om allt data i glass till standardiserat data
standard.features <- scale(Glass[,9:1])
stdData <- cbind(standard.features,Glass[10])

#kollar att värderna kom med
anyNA(stdData)
head(stdData)

#Delar upp i de olika % fördelningarna
standard50 <- sample(1:nrow(stdData),as.integer(0.50*nrow(stdData)))
standard66 <- sample(1:nrow(stdData),as.integer(0.66*nrow(stdData)))
standard75 <- sample(1:nrow(stdData),as.integer(0.75*nrow(stdData)))


#Lägger in de olika datasetten i respektiva variabel: 

#Train50%, Test 50%
stdTrain50 <- stdData[standard50,]
stdTest50 <- stdData[-standard50,]

#Train 66%, Test 34%
stdTrain66 <- stdData[standard66,]
stdTest66 <- stdData[-standard66,]

#Train 75%, Test 25%
stdTrain75 <- stdData[standard75,]
stdTest75 <- stdData[-standard75,]

#Kör de olika kNN delarna respektive % sats

#K = 3 
###50% 
pred <- knn(stdTrain50[,-10],stdTest50[,-10],stdTrain50[,10],k=3)
confusionStd50 <- print(table(Target = pred,Prediction = stdTest50[,10]))
accuracyStand50 <- (sum(diag(confusionStd50))/sum(confusionStd50))*100
accuracyStand50

###66% 
pred <- knn(stdTrain66[,-10],stdTest66[,-10],stdTrain66[,10],k=3)
confusionStd66 <- print(table(Target = pred,Prediction = stdTest66[,10]))
accuracyStand66 <- (sum(diag(confusionStd66))/sum(confusionStd66))*100
accuracyStand66

###75% 
pred <- knn(stdTrain75[,-10],stdTest75[,-10],stdTrain75[,10],k=3)
confusionStd75 <- print(table(Target = pred,Prediction = stdTest75[,10]))
accuracyStand75 <- (sum(diag(confusionStd75))/sum(confusionStd75))*100
accuracyStand75


#K = 5 

# 50% Train
pred <- knn(stdTrain50[,-10],stdTest50[,-10],stdTrain50[,10],k=5)
confusionStd50 <- print(table(Target = pred,Prediction = stdTest50[,10]))
accuracyStand50 <- (sum(diag(confusionStd50))/sum(confusionStd50))*100
accuracyStand50

# 66% Train
pred <- knn(stdTrain66[,-10],stdTest66[,-10],stdTrain66[,10],k=5)
confusionStd66 <- print(table(Target = pred,Prediction = stdTest66[,10]))
accuracyStand66 <- (sum(diag(confusionStd66))/sum(confusionStd66))*100
accuracyStand66

# 75% Train
pred <- knn(stdTrain75[,-10],stdTest75[,-10],stdTrain75[,10],k=5)
confusionStd75 <- print(table(Target = pred,Prediction = stdTest75[,10]))
accuracyStand75 <- (sum(diag(confusionStd75))/sum(confusionStd75))*100
accuracyStand75


#K = 7 

# 50%
pred <- knn(stdTrain50[,-10],stdTest50[,-10],stdTrain50[,10],k=7)
confusionStd50 <- print(table(Target = pred,Prediction = stdTest50[,10]))
accuracyStand50 <- (sum(diag(confusionStd50))/sum(confusionStd50))*100
accuracyStand50

# 66%
pred <- knn(stdTrain66[,-10],stdTest66[,-10],stdTrain66[,10],k=7)
confusionStd66<- print(table(Target = pred,Prediction = stdTest66[,10]))
accuracyStand66 <- (sum(diag(confusionStd66))/sum(confusionStd66))*100
accuracyStand66

# 75%
pred <- knn(stdTrain75[,-10],stdTest75[,-10],stdTrain75[,10],k=7)
confusionStd75 <- print(table(Target = pred,Prediction = stdTest75[,10]))
accuracyStand75 <- (sum(diag(confusionStd75))/sum(confusionStd75))*100
accuracyStand75

###########  SLUT DEL 2   Standalisering ########################


########## DEL 3 Normalisera data ##############################

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normaliserar glass data 
#Jag vill bara normalisera features inte typerna därav 1:9
Glass_nrm <- as.data.frame(lapply(Glass[1:9], normalize))
#Visar upp det normaliserade datat 
summary(Glass_nrm)

ind50 <- sample(2, nrow(Glass_nrm), replace=TRUE, prob=c(1/2, 1/2))
ind66 <- sample(2, nrow(Glass_nrm), replace=TRUE, prob=c(2/3, 1/3))
ind75 <- sample(2, nrow(Glass_nrm), replace=TRUE, prob=c(3/4, 1/4))


# Kollar så fördelningen blev rätt
length(ind50[ind50==1]) #training data 
length(ind50[ind50==2]) #test data
length(ind66[ind66==1]) #training data 
length(ind66[ind66==2]) #test data
length(ind75[ind75==1]) #training data 
length(ind75[ind75==2]) #test data
nrow(Glass)

## Skapar variabler som håller de olika datasetten
## 50%
Glass.Nrmtraining50 <- Glass_nrm[ind50==1, 1:9]
Glass.Nrmtest50 <- Glass_nrm[ind50==2, 1:9] 

## 60%
Glass.Nrmtraining66 <- Glass_nrm[ind66==1, 1:9]
Glass.Nrmtest66 <- Glass_nrm[ind66==2, 1:9] 

## 75%
Glass.Nrmtraining75 <- Glass_nrm[ind75==1, 1:9]
Glass.Nrmtest75 <- Glass_nrm[ind75==2, 1:9] 

## Skapar upp klasser (namnen på typerna)
# 50%
Glass.NrmtrainLabels50 <- Glass[ind50==1, 10]
Glass.NrmtestLabels50 <- Glass[ind50==2, 10]

# 66%
Glass.NrmtrainLabels66 <- Glass[ind66==1, 10]
Glass.NrmtestLabels66 <- Glass[ind66==2, 10]

# 75%
Glass.NrmtrainLabels75 <- Glass[ind75==1, 10]
Glass.NrmtestLabels75 <- Glass[ind75==2, 10]

#################KNN##############################################################################

#Sätter gränsen för närmaste grannen reglen till 3
classifiedData50 <- knn(train = Glass.Nrmtraining50, test = Glass.Nrmtest50, cl = Glass.NrmtrainLabels50, k=3)

classifiedData66 <- knn(train = Glass.Nrmtraining66, test = Glass.Nrmtest66, cl = Glass.NrmtrainLabels66, k=3)

classifiedData75 <- knn(train = Glass.Nrmtraining75, test = Glass.Nrmtest75, cl = Glass.NrmtrainLabels75, k=3)

#################   Kör knn 3 med de olika % satserna:  #############################################  
#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm50 <- table(Target = Glass.NrmtestLabels50, Predicted = classifiedData50)
confusionNrm50
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm50 <- (sum(diag(confusionNrm50))/sum(confusionNrm50) )* 100
accuracyNrm50

#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm66 <- table(Target = Glass.NrmtestLabels66, Predicted = classifiedData66)
confusionNrm66
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm66 <- (sum(diag(confusionNrm66))/sum(confusionNrm66) )* 100
accuracyNrm66

#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm75 <- table(Target = Glass.NrmtestLabels75, Predicted = classifiedData75)
confusionNrm75
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm75 <- (sum(diag(confusionNrm75))/sum(confusionNrm75) )* 100
accuracyNrm75

#################KNN##############################################################################

#Sätter gränsen för närmaste grannen reglen till 5
classifiedData50 <- knn(train = Glass.Nrmtraining50, test = Glass.Nrmtest50, cl = Glass.NrmtrainLabels50, k=5)

classifiedData66 <- knn(train = Glass.Nrmtraining66, test = Glass.Nrmtest66, cl = Glass.NrmtrainLabels66, k=5)

classifiedData75 <- knn(train = Glass.Nrmtraining75, test = Glass.Nrmtest75, cl = Glass.NrmtrainLabels75, k=5)

#################   Kör knn 5 med de olika % satserna:  #############################################

#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm50 <- table(Target = Glass.NrmtestLabels50, Predicted = classifiedData50)
confusionNrm50
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm50 <- (sum(diag(confusionNrm50))/sum(confusionNrm50) )* 100
accuracyNrm50

#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm66 <- table(Target = Glass.NrmtestLabels66, Predicted = classifiedData66)
confusionNrm66
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm66 <- (sum(diag(confusionNrm66))/sum(confusionNrm66) )* 100
accuracyNrm66

#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm75 <- table(Target = Glass.NrmtestLabels75, Predicted = classifiedData75)
confusionNrm75
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm75 <- (sum(diag(confusionNrm75))/sum(confusionNrm75) )* 100
accuracyNrm75


#################KNN##############################################################################

#Sätter gränsen för närmaste grannen reglen till 5
classifiedData50 <- knn(train = Glass.Nrmtraining50, test = Glass.Nrmtest50, cl = Glass.NrmtrainLabels50, k=7)

classifiedData66 <- knn(train = Glass.Nrmtraining66, test = Glass.Nrmtest66, cl = Glass.NrmtrainLabels66, k=7)

classifiedData75 <- knn(train = Glass.Nrmtraining75, test = Glass.Nrmtest75, cl = Glass.NrmtrainLabels75, k=7)

#################   Kör knn 7 med de olika % satserna:  #############################################


#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm50 <- table(Target = Glass.NrmtestLabels50, Predicted = classifiedData50)
confusionNrm50
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm50 <- (sum(diag(confusionNrm50))/sum(confusionNrm50) )* 100
accuracyNrm50

#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm66 <- table(Target = Glass.NrmtestLabels66, Predicted = classifiedData66)
confusionNrm66
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm66 <- (sum(diag(confusionNrm66))/sum(confusionNrm66) )* 100
accuracyNrm66

#Visar i en tabel hur det gick för programmet att gissa. 
confusionNrm75 <- table(Target = Glass.NrmtestLabels75, Predicted = classifiedData75)
confusionNrm75
# Visar upp i % hur många gånger programet kunde gissa rätt
accuracyNrm75 <- (sum(diag(confusionNrm75))/sum(confusionNrm75) )* 100
accuracyNrm75
