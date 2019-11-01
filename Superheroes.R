#libraries
library(arules)
library(arulesViz)
library(cluster)
library(EMCluster)
library(randomForest)
library(caret)
library(e1071)
library(FSelector)

#Reading in the data sets
superPowers <- read.csv("C:/Users/pbonnin/Desktop/Local DataScience@Syracuse/IST565 Project/super_hero_powers.csv",stringsAsFactors = FALSE)
superDemo <- read.csv("C:/Users/pbonnin/Desktop/Local DataScience@Syracuse/IST565 Project/heroes_information.csv",stringsAsFactors = FALSE)

SparsePowers <- superPowers
SparsePowers[SparsePowers=="True"] <- 1
SparsePowers[SparsePowers=="False"] <- 0
SparsePowers[,2:ncol(SparsePowers)] <- as.data.frame(apply(SparsePowers[,2:ncol(SparsePowers)],2,as.numeric))

############## Pre-procesing superDemo ############## 
superDemo <- superDemo[,-1]

#Looking at the numerical variables for discretization
#hist(superDemo$Weight)
#hist(superDemo$Height)

#only complete data switch
superDemoC <- superDemo
superDemoC[superDemoC=="-"] <- NA
superDemoC[superDemoC=="-99"] <- NA
dim(superDemoC[complete.cases(superDemoC),])-dim(superDemoC)

#cleaning superDemos
superDemoF <- superDemo

#keeping only complete cases, numeric height/weight, replacing "-" with neutral for alignment and with "flesh" for skin color

superDemoF <- superDemoF[complete.cases(superDemoF),]
superDemoF$Height <- as.numeric(superDemoF$Height)
superDemoF$HeightNum <- superDemoF$Height
superDemoF$Weight <- as.numeric(superDemoF$Weight)
superDemoF$WeightNum <- superDemoF$Weight
superDemoF$Alignment <- as.factor(gsub("-","neutral",superDemoF$Alignment))
superDemoF$Skin.color <- as.factor(gsub("-","flesh",superDemoF$Skin.color))
superDemoF <- superDemoF[!duplicated(superDemoF$name),]
superDemoF[,c("Gender","Eye.color","Race","Hair.color","Publisher")] <- as.data.frame(apply(superDemoF[,c("Gender","Eye.color","Race","Hair.color","Publisher")],2,as.factor))

#discretizing weight and height

superDemoF$Weight <- cut(superDemoF$Weight, breaks = c(-Inf,-90,50,200,400,Inf), labels = c("-","Ultra.Light","Normal","Heavy","Ultra.Heavy"))
superDemoF$Weight <- ordered(superDemoF$Weight,c("-","Ultra.Light","Normal","Heavy","Ultra.Heavy"))
superDemoF$Height <- cut(superDemoF$Height, breaks = c(-Inf,-90,35,150,200,250,Inf), labels = c("-","Tiny","Short","Normal","Tall","Huge"))
superDemoF$Height <- ordered(superDemoF$Height,c("-","Tiny","Short","Normal","Tall","Huge"))

summary(superDemoF)
#unique(superDemoF[superDemoF$Hair.color=="-","name"])

#getting rid of duplicate names in super powers (hint: there were none)
superPowers <- superPowers[!duplicated(superPowers$hero_names),]

#merging the two dataframes. What if we kept only complete data?
superDF <- merge.data.frame(superDemoF,superPowers, by.y = "hero_names" , by.x = "name", all.y = TRUE)
superDFTest <-superDF
superDFTest[superDFTest=="-"] <- NA
dim(superDFTest[complete.cases(superDFTest),])-dim(superDFTest)
# too many drops (333)

#a superDF with powers in binary instead of strings
superDemoF[superDemoF==-99] <- 0
superDF2 <- merge.data.frame(superDemoF,SparsePowers, by.y = "hero_names" , by.x = "name", all.y = TRUE)
superDF2 <- superDF2[complete.cases(superDF2),]

#################### kmeans #######################

#use kmeans to avoid the attributes with too many incomplete examples ("-")

#trying out different k's
data <- superDF2[,11:ncol(superDF)]

set.seed(50)
wss <- sapply(15:30,function(k){kmeans(data, k, iter.max = 30 )$tot.withinss})

#elbow plot for kmeans
set.seed(50)
plot(15:30, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(50)
superKMean <- kmeans(superDF2[,11:ncol(superDF)], 24)

#clusters into the data frame
superDF3 <- data.frame("Clusters"=as.factor(superKMean$cluster),superDF2)
clusplot(superDF2[,11:ncol(superDF)],superKMean$cluster,color = TRUE,lines = 0, main = "SuperPower Clusters")


############## AR Mining ##############

#dropping incomplete values from before to exclude from the "baskets" 
superDF2AR <- superDF2
superDF2AR[superDF2AR=="-"] <- NA
superDF2AR[superDF2AR=="flesh"] <- NA
superDFTran <- as(cbind(superDF2AR[,2:10],superDF2[,13:ncol(superDF2)]==1), "transactions")

TranPubAlign <- as(cbind(superDF2[,c("Publisher","Alignment")],superDF2[,13:ncol(superDF2)]==1), "transactions")

itemFreq <- sort(itemFrequency(superDFTran),decreasing = TRUE)
itemFreq[1:10]

superRules <- apriori(superDFTran, parameter = list(supp = 0.05, conf = 1, maxlen = 10))
superRules <- sort(superRules, by = "support", decreasing = TRUE)
inspect(superRules[1:5])


superRules2 <- apriori(TranPubAlign, parameter = list(supp = 0.05, conf = 0.9, maxlen = 8))
superRules2 <- sort(superRules2, by = "confidence", decreasing = TRUE)

#Lower support rules for good and bad alignment
superRules4 <- apriori(superDFTran, parameter = list(supp = 0.05, conf = 0.9, maxlen = 10),appearance = list(default="lhs",rhs="Alignment=good"), control = list(verbose = F))
superRules4 <- sort(superRules4, by = "support", decreasing = TRUE)
inspect(superRules4[!is.redundant(superRules4)][1:10])

superRules5 <- apriori(superDFTran, parameter = list(supp = 0.01, conf = 0.9, maxlen = 8),appearance = list(default="lhs",rhs="Alignment=bad"), control = list(verbose = F))
superRules5 <- sort(superRules5, by = "lift", decreasing = TRUE)
inspect(superRules5[!is.redundant(superRules5)])


############## AR with Only powers ######################

#AR Mining, what powers go together?
superPowersAR <- read.csv("C:/Users/pbonnin/Desktop/Local DataScience@Syracuse/IST565 Project/super_hero_powers.csv")
superPowerTran <- as(superPowersAR == "True", "transactions")
superRules <- apriori(superPowerTran, parameter = list(supp = 0.05, conf = 0.9, maxlen = 5))
superRules <- sort(superRules, by = "confidence", decreasing = TRUE)
superPlot <- plot(superRules[!is.redundant(superRules)], interactive = TRUE)
inspect(superRules[!is.redundant(superRules)])

#explore the item sets
View(sort(itemFrequency(superPowerTran),decreasing = TRUE))

#This could be used as a function if a list of powers can be provided
superRules2 <- apriori(superPowerTran, parameter = list(supp = 0.05, conf = 0.9, maxlen = 5),appearance = list(default="lhs",rhs="Super.Speed"), control = list(verbose = F))
superRules2 <- sort(superRules2, by = "confidence", decreasing = TRUE)
inspect(superRules2)

#a Super.Strength only one
superRules3 <- apriori(superPowerTran, parameter = list(supp = 0.05, conf = 0.9, maxlen = 5),appearance = list(default="lhs",rhs="Super.Strength"), control = list(verbose = F))
superRules3 <- sort(superRules3, by = "confidence", decreasing = TRUE)
plot(superRules3, interactive = TRUE)


############## Sampling superDF3 ############################


#separating good and bad for stratified samples

goodDF <- superDF3[superDF3$Alignment=="good",]
badDF <- superDF3[superDF3$Alignment=="bad",]
neutralDF <- superDF3[superDF3$Alignment=="neutral",]

set.seed(10)
goodIndexes <- sample(1:nrow(superDF3[superDF3$Alignment=="good",]))
badIndexes <- sample(1:nrow(superDF3[superDF3$Alignment=="bad",]))

#training and testing good DF
#length(goodIndexes)
goodTest <- goodDF[goodIndexes[1:130],]
goodTrain <- goodDF[goodIndexes[131:length(goodIndexes)],]

#training and testing bad DF
#length(badIndexes)
badTest <- badDF[badIndexes[1:60],]
badTrain <- badDF[badIndexes[61:length(badIndexes)],]

#Merging back together both samples
superTest <- rbind(goodTest,badTest)
superTrain <- rbind(goodTrain,badTrain)

superTrain$Alignment <- factor(superTrain$Alignment)
superTest$Alignment <- factor(superTest$Alignment)

#dropping variables with too many factors (using clusters instead) = names, race, eye
superTest <- superTest[,-c(2,5,12,13)]
superTrain <- superTrain[,-c(2,5,12,13)]
superDF4 <- superDF3[,-c(2,5,12,13)]

#What if the binary were factors?

superTrainF <- superTrain
superTrainF$REF <- rep("Train",nrow(superTrainF))
superTrainF[,10:ncol(superTrainF)] <- as.data.frame(apply(superTrainF[,10:ncol(superTrainF)],2,as.factor))

superTestF <- superTest
superTestF$REF <- rep("Test",nrow(superTestF))

factorizer <- rbind(superTrainF,superTestF)
factorizer[,10:ncol(factorizer)] <- as.data.frame(apply(factorizer[,10:ncol(factorizer)],2,as.factor))

superTestF <- factorizer[factorizer$REF=="Test",]
superTestF <- subset(superTestF, select=-c(REF))

superTrainF <- factorizer[factorizer$REF=="Train",]
superTrainF <- subset(superTrainF, select=-c(REF))



#Random forests
set.seed(100)
superRF_F <- randomForest(Alignment~.,superTrainF)
importance <- as.data.frame(superRF_F$importance)
superPredict <- predict(superRF,superTest[,-8])

wordcloud(rownames(importance),importance$MeanDecreaseGini)


RF_Accuracy <- confusionMatrix(superTest$Alignment,superPredict)$overall[1]
RF_AccuracyG <- confusionMatrix(superTest$Alignment,superPredict,mode = "prec_recall", positive = "good")$byClass[5:6]
RF_AccuracyB <- confusionMatrix(superTest$Alignment,superPredict,mode = "prec_recall", positive = "bad")$byClass[5:6]
RF_Results <- cbind("Accuracy"=RF_Accuracy[1],"Precision (good)"=RF_AccuracyG[1],"Recall (good)"=RF_AccuracyG[2],"Precision (bad)"=RF_AccuracyB[1],"Recall (bad)"=RF_AccuracyB[2])
rownames(RF_Results) <- "Random Forest"

#Naive Bayes
set.seed(100)
superNB_DefaultR <- naiveBayes(Alignment~.,superTrainF)
BayesNB_Train <- predict(superNB_DefaultR,superTrainF[,-8])
confusionMatrix(BayesNB_Train,superTrainF$Alignment,mode = "prec_recall", positive = "good")

BayesNB_Test <- predict(superNB_DefaultR,superTestF[,-8])
confusionMatrix(BayesNB_Test,superTestF$Alignment,mode = "prec_recall", positive = "good")

Bayes_Accuracy <- confusionMatrix(superTest$Alignment,BayesNB_Test)$overall[1]
Bayes_AccuracyG <- confusionMatrix(superTest$Alignment,BayesNB_Test,mode = "prec_recall", positive = "good")$byClass[5:6]
Bayes_AccuracyB <- confusionMatrix(superTest$Alignment,BayesNB_Test,mode = "prec_recall", positive = "bad")$byClass[5:6]
Bayes_Results <- cbind("Accuracy"=Bayes_Accuracy[1],"Precision (good)"=Bayes_AccuracyG[1],"Recall (good)"=Bayes_AccuracyG[2],"Precision (bad)"=Bayes_AccuracyB[1],"Recall (bad)"=Bayes_AccuracyB[2])
rownames(Bayes_Results) <- "Naive Bayes"

#SVM
set.seed(100)
superRadialSVM <- svm(Alignment~.,superTrain, kernel = "radial", cost = 500 ,na.action = na.omit)

superSVM_TEST <- predict(superRadialSVM,superTest[,-8])

confusionMatrix(predict(superLinearSVM,superTrain[,-8]),superTrain[,8], positive = "good")
confusionMatrix(predict(superLinearSVM,superTest[,-8]),superTest[,8], positive = "good")

SVM_Accuracy <- confusionMatrix(superTest$Alignment,superSVM_TEST)$overall[1]
SVM_AccuracyG <- confusionMatrix(superTest$Alignment,superSVM_TEST,mode = "prec_recall", positive = "good")$byClass[5:6]
SVM_AccuracyB <- confusionMatrix(superTest$Alignment,superSVM_TEST,mode = "prec_recall", positive = "bad")$byClass[5:6]
SVM_Results <- cbind("Accuracy"=SVM_Accuracy[1],"Precision (good)"=SVM_AccuracyG[1],"Recall (good)"=SVM_AccuracyG[2],"Precision (bad)"=SVM_AccuracyB[1],"Recall (bad)"=SVM_AccuracyB[2])
rownames(SVM_Results) <- "Radial SVM"


######################### Variable Importance ########################

CHI2 <- chi.squared(Alignment~.,superTrainF)
CHI2 <- CHI2[order(-CHI2$attr_importance), , drop = FALSE]
head(CHI2,10)
wordcloud(rownames(CHI2),CHI2$attr_importance,colors = TRUE,max.words=20)

GR <- gain.ratio(Alignment~.,superTrainF)
GR <- GR[order(-GR$attr_importance), , drop = FALSE]
head(GR,10)
wordcloud(rownames(GR),GR$attr_importance,colors = TRUE,max.words=20)

#what about only powers?
superTest2 <- superTest[,-c(1:7,9)]
superTrain2 <- superTrain[,-c(1:7,9)]
superRF2 <- randomForest(Alignment~.,superTrain2)
importance2 <- as.data.frame(superRF2$importance)
wordcloud(rownames(importance2),importance2$MeanDecreaseGini)

##################### ideas for next steps #####################
# Use kmeans to decide how to discretize the Races based on their other demographics <- show how there were a lot of races
# bin the weight and height
# names as characters
# "-" to NA <- how many complete cases?
# hair and eye color could be boiled down <- probably do it based on the number of cases they each have
# do some AR mining to see how powers go together <- maybe add the alignment into these to force a RHS?
# after all the variables are discretized run random forest feature selection to understand what variables help in classifying, maybe even CHI2 and info gain through FSelector
# separate into training and testing sets
# use NB, SVM, RF, KNN and DT's to see which one helps most
# possibly create a shiny app that puts out a buzzfeed style quizz
#### AR mining could help you select the next super power (pick 3)
#### select your demo's
#### "you are a bad guy" <- you are superman? <-- knn to match to the actual hero?

