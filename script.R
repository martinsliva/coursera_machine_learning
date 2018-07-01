library(caret)


### download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
### download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")



training<-read.csv("pml-training.csv")
testing<-read.csv("pml-testing.csv")


#### Data preparation

pokus <- function(x) sum(is.na(x))
col.na <- apply(training, 2, pokus) ### collumns full of NA - not used in training, omited in testing
training2 <- (training[,(!col.na==19216)])
testing2<- (testing[,(!col.na==19216)])

pokus2 <- function(x)sum(x=="#DIV/0!")
col.div0 <- apply(training2, 2, pokus2) ### collumns wit DIV0 text of empty only - not used in training, omited in testing
training3 <- training2[,(col.div0==0)]
testing3 <- testing2[,(col.div0==0)]

training4 <- training3[,-c(1,2,3,4,5,6,7)] ### Collumns with order and time stamp, name of user, number of time window - not used in analysis , omited in testing
testing4 <- testing3[,-c(1,2,3,4,5,6,7)]

### Finding highly correlated columns and using only one representant of highly correlated groups of columns
cortab <- abs(cor(training4[, -53]))
diag(cortab) <- 0
which(cortab>0.95,arr.ind = T)
training5 <- training4[,-c(4,8,10,32)]
testing5 <- testing4[,-c(4,8,10,32)]




#### Preparing training and Validation set
set.seed(123)
TrainingIndex <- createDataPartition(training5$classe, p=.8, list = FALSE, times = 1)
training_final<- training5[TrainingIndex,]
validation <- training5[-TrainingIndex,]

#### Developement - limitation of data for development code



#      TrainingIndex <- createDataPartition(training_final$classe, p=.03, list = FALSE, times = 1)
#      training_final<- training_final[TrainingIndex,]


#### Trainning

fit_rf <- train(classe~., data=training_final, method="rf") ### Random Forest
fit_lda <- train(classe~., data=training_final, method="lda") ### Linear Discriminant Analysis

### Validation

pred_rf <- predict(fit_rf, newdata = validation)
pred_lda <- predict(fit_lda, newdata = validation)

#### Comparing method
confusionMatrix(pred_rf,validation$classe)
confusionMatrix(pred_lda,validation$classe)

### Testing

print(pred_test <- predict(fit_rf, newdata=testing5))

write.csv(file="result.csv", pred_test)
