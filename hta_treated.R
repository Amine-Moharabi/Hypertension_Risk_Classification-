hta=HTA_treated
#Exploritory data analysis
table(is.na(hta))
summary(is.na(hta))
summary(table(is.na(hta)))
#summary of the data
summary(hta)
#convertig the target varibale as factor
hta$P_Risk= as.factor(hta$P_Risk)
#correlation matrix between data
cor(hta)
#randoom forest library
library(randomForest)
#spilitinf the data frame into test and train
set.seed(123) #randoom sampling
indices <- sample(1:nrow(hta), 0.7 * nrow(hta))
train_data <- hta[indices, ]
test_data <- hta[-indices, ]
table(train_data$P_Risk)
summary(test_data$P_Risk)
# Re-run the randomForest model
rf_model <- randomForest(P_Risk ~ ., data = train_data, ntree = 100, importance = TRUE)

# Afficher un r�sum� du mod�le
print(rf_model)

# Faire des pr�dictions sur l'ensemble de test
predictions <- predict(rf_model, test_data)

# �valuer la performance du mod�le (exemple : matrice de confusion pour une classification)
library(caret)
confusionMatrix(predictions, test_data$P_Risk)
print(rf_model)
