input <- read.csv(file="E:\\Kuliah\\Semester 6\\Pengantar Data Mining\\archive-house prediction\\data.csv")
data_HousePrediction <- data.frame(input)
data_HousePrediction
str(data_HousePrediction)
View(data_HousePrediction)
dim(data_HousePrediction)

set.seed(123456)
sample_HousePrediction <- sample(2, nrow(data_HousePrediction), replace=TRUE, prob=c(0.8, 0.2))
train_HP<-data_HousePrediction[sample_HousePrediction==1,]
test_HP<-data_HousePrediction[sample_HousePrediction==2,]
dim(train_HP)
dim(test_HP)
View(train_HP)
View(test_HP)

library(party)
myFormula <- price ~ bedrooms + bathrooms + sqft_basement
HP_ctree <- ctree(myFormula, data=train_HP)
# mengecek hasil prediksi
table(predict(HP_ctree), train_HP$price)

print(HP_ctree)
plot(HP_ctree)
plot(HP_ctree, type="simple")
testprediksi<-predict(HP_ctree, newdata=test_HP)
table(testprediksi, test_HP$price)

#hitung akurasi prediksi
Prediksi1 <- predict(HP_ctree, data=test_HP[,-18])
Hasilprediksi <- table(test_HP[,18], predict(HP_ctree, test_HP[,-18]))
Hasilprediksi
accuracy <- (sum(diag(Hasilprediksi)))/sum(Hasilprediksi)
accuracy


#####Evaluasi kinerja
library(rpart)
myFormula <- price ~ bedrooms + bathrooms + sqft_basement
HP_rpart <- rpart(myFormula, data = train_HP, control = rpart.control(minsplit = 18))
attributes(HP_rpart)

plot(HP_rpart)
text(HP_rpart, use.n=T)

opt <- which.min(HP_rpart$cptable[,"xerror"])
cp <- HP_rpart$cptable[opt, "CP"]
HP_prune <- prune(HP_rpart, cp = cp)
print(HP_prune)
plot(HP_prune)
text(HP_prune, use.n=T)

Price_pred<-predict(HP_prune, newdata = test_HP)
xlim<-range(data_HousePrediction$price)
plot(Price_pred~price,data = test_HP,xlab = "Observed",ylab = "Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)
