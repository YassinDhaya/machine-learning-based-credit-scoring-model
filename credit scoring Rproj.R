#IMPORT DATA
library(dplyr)
library(readxl)
library( magrittr)
credit_data <- read_excel("C:/Users/yassi/Downloads/credit-data.xlsx")

View(credit_data)
#COLUMN REDUCTION
threshold <- 0.05 #for a 5% cut-off

df <- credit_data %>% select(where(~mean(is.na(.)) < threshold))
View(df)
colnames(df)
df1 <- df[,-c(1,2)]
View(df1)
sum(is.na(df1))
#CLASS DIVISION
df1$class <- ifelse(df1$CLASSE122019 < 1 | df1$CLASSE122020 < 1| df1$CLASSE122021 < 1 , 0, 1)
table(df1$class)
barplot(table(df1$class),
        ylab = "Frequency",
        xlab = "class")
df1$class <- factor(df1$class)
table(df1$class)
View(df1)
df1 <- df1[,-c(1:3)]
table(df1$class)
str(df1)
#MICE
library(mice)
md.pattern(df1)
p <- md.pairs(df1)
p

library(VIM)
marginplot(df1[, c("ROA", "ROE")], col = mdc(1:2), cex = 1.2,
           cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

imp <- mice(df1, method = "cart")
md.pattern(df1)
print(imp)
complete(imp)
stripplot(imp, pch = 20, cex = 1.2)
View(complete(imp))
sum(is.na(complete(imp)))
df2 <- complete(imp)
View(df2)
table(df2$class)
str(df2)
#remove correlated features
library(ggcorrplot)
df2$class <- as.numeric(df2$class)
cor_matrix <- data.frame(cor(df2))
cor_matrix
ggcorrplot(cor_matrix)
library(caret)
findCorrelation(cor(df2), cutoff=0.75)
cor_matrix_rm <- cor_matrix                  # Modify correlation matrix
cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
diag(cor_matrix_rm) <- 0
cor_matrix_rm
data_new <- df2[ , !apply(cor_matrix_rm,    # Remove highly correlated variables
                          2,
                          function(x) any(abs(x > 0.75)))]
ggcorrplot(cor_matrix_rm)
ggcorrplot(cor(data_new[,-20]))
data_new$class <- data_new$class - 1
data_new$class <- factor(data_new$class)
View(data_new)
#normality test
library("dplyr")
library("ggpubr")
apply(data_new[,-20],2,shapiro.test)
#skewness test
library(parameters)
apply(data_new[,-20],2,skewness)
#lof
library(DescTools)
library(dbscan)
class_scale <- as.data.frame(scale(data_new[,-20]))
View(class_scale)
class_lof <- lof(class_scale, minPts = 5)

head(class_scale)
data_newlof <- data_new
data_newlof$lof <- class_lof

head(data_newlof)
View(data_newlof)
library(FactoMineR)
library(factoextra)

class_pca <- PCA(class_scale, scale.unit = F, ncp = 6, graph = F)

summary(class_pca)
fviz_eig(class_pca, ncp = 6, addlabels = T, main = "Variance explained by each dimensions")
class_a <- data.frame(class_pca$ind$coord[,1:3])
class_b <- cbind(class_a, class = data_newlof$class, lof_score = data_newlof$lof)
install.packages("hrbrthemes")
library(hrbrthemes)
fraud_lof_visual <- ggplot(class_b, aes(x=Dim.1 ,y=Dim.2, color=class)) + 
  geom_point(aes(size=lof_score)) +
  ggtitle("LOF Score Distribution")+
  theme_ipsum()

fraud_lof_visual
class_b %>%
  filter(lof_score <= 1.75) %>% 
  ggplot( aes(x=lof_score)) +
  geom_density( color="#e9ecef", fill = "#c90076", alpha=0.7) +
  scale_fill_manual(values="#8fce00") +
  xlab("LOF Score")+
  ggtitle("LOF Score Distribution")+
  theme_ipsum() +
  labs(fill="")
quantile(class_b$lof_score, probs = c(0, 0.5))
class_b <- class_b %>% 
  mutate(outlier = ifelse(lof_score > 1.1281767, 1, 0))
class_lof_visual_b <- ggplot(class_b, aes(x=Dim.1 ,y=Dim.2, color=outlier)) + 
  geom_point() +
  ggtitle("LOF Score Distribution")+
  theme_ipsum()

class_lof_visual_b
data_new1 <- data_newlof[data_newlof$lof <= 1.1281767 ,]
View(data_new1)
no_outliers <- data_new1[,-21]
no_outliers2 <- no_outliers
table(no_outliers$class)
View(no_outliers)
barplot(table(no_outliers$class),
        ylab = "Frequency",
        xlab = "class")
#STANDARDIZATION
no_outliers[,-20] <- no_outliers[,-20] %>% mutate_all(~(scale(.) %>% as.vector))
View(no_outliers)
#split data
set.seed(111)
ind <- sample(2, nrow(no_outliers),
              replace = TRUE,
              prob = c(0.7, 0.3))
train <- no_outliers[ind==1,]
test <- no_outliers[ind==2,]
prop.table(table(no_outliers$class))
#smote
library(smotefamily)
table(no_outliers$class)
train <- SMOTE(train[,-20],train[,20],K=10)
View(train$data)
table(train$data$class)
train <- train$data
train$class <- factor(train$class)
barplot(table(train$class),
        ylab = "Frequency",
        xlab = "class")
#confusion matrix draw
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
#FEATURE SELECTION (USING BORUTA TECHNIQUE)
#BORUTA
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

set.seed(111)
boruta <- Boruta(class ~ ., data = train, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

getNonRejectedFormula(boruta)
rfboruta <- randomForest(class ~ COMMITMENT122021 + WCR + NET_TREASURY + COVERAGE_RATIO + 
                           SELF_FUNDING + TOTAL_DEBT_RATIO + SHAREHOLDER_CAP_PR_SHARE_CAP + 
                           CURRENT_RATIO + ROE + ROA + OPERATING_PROFITABLILITY + EXEPTIONAL_RESULT + 
                           GEARING + LEVERAGE_RATIO + TOTAL_ASSETS + R6_DETTES_CT_ACTIF_CIRCULANTS + 
                           R7_DETTES_LT_PROV_CAP_FINANC + TOTAL_EQUITY + R14_FDR_BFR, data = train)
print(rfboruta)
p <- predict(rfboruta, test)
cm2 <- confusionMatrix(p, test$class)
cm2
varImpPlot(rfboruta,
           sort = T,
           n.var = 16,
           main = "Variable Importance")
draw_confusion_matrix(cm2)
getConfirmedFormula(boruta)

#RANDOM FOREST
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtree <- 16
mtry <- sqrt(ncol(train[,-20]))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(class~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(7)
metric <- "Accuracy"
mtry <- sqrt(ncol(trainrf[,-17]))
rf_random <- train(class~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
#grid
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(class~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#SMV
# Create a SVM model with default hyperparameters
svm <- train(class ~ ., data = train, method = "svmRadial")
print(svm)
summary(svm)
# Create a hyperparameter grid
hyper_grid <- expand.grid(sigma = c(0.01, 0.1, 1), C = c(1, 10, 100))

# Perform hyperparameter tuning using 10-fold cross-validation
svm.tuned <- train(class ~ ., data = train, method = "svmRadial",
                   trControl = trainControl(method = "cv", number = 10),
                   metric = "Accuracy",
                   tuneGrid = hyper_grid)

# Print the best hyperparameters
print(svm.tuned$bestTune)
svm.tuned
# Make predictions on the test dataset
predictions <- predict(svm.tuned, test)

# Calculate the accuracy
accuracy <- mean(predictions == test$class)
plot(smv.tuned,train)
# Print the accuracy
print(accuracy)
confusionMatrix(predictions,test$class)
library(e1071)
classifier = svm(formula = class ~ .,
                 data = train,
                 cost = 10,
                 gamma = 1,
                 type = 'C-classification',
                 kernel = 'radial')

classifier$gamma
View(classifier)
y_pr = predict(classifier, newdata = test[-20])
cmv = table(test[, 20], y_pr)
cmv
ROC2 <- roc(as.numeric(test$class), as.numeric(y_pr))
par(pty = "s")
plot(ROC2, col = "blue", main = "ROC For random forest", legacy.axes = T)

# NEURAL NETWORK
index <- createDataPartition(no_outliers$class, p=0.7, list=FALSE)

final.training <- no_outliers[index,]
final.test <- no_outliers[-index,]
library(dplyr)
X_train <- train %>% 
  select(-class) %>% 
  scale()

y_train <- to_categorical(train$class)
X_test <- test %>% 
  select(-class) %>% 
  scale()

y_test <- to_categorical(test$class)
# Hyperparameter tuning
runs <- tuning_run("experiment.R",
                   flags = list(dense_units1 = c(32, 64),
                                dense_units2 = c(16,32),
                                dropout1 = c(0.1,0.2),
                                dropout2 = c(0.1,0.2),
                                batch_size = c(32,64)))

head(runs)
res <- runs[,c(3,5:10)]
model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(
  X_train, y_train, 
  epochs = 50, 
  batch_size = 64,
  validation_split = 0.3
)
summary(model)
model %>% evaluate(X_test, y_test)
predictions <- model %>% predict(X_test) %>% `>`(0.5) %>% k_cast("int32")
#XGBOOST
# Define the hyperparameter grid
hyper_params <- expand.grid(
  nrounds = 100,
  max_depth = 5,
  eta = 0.01,
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 0.7
)

# Perform hyperparameter tuning using cross-validation
cv_results <- train(
  x = train[, -20],
  y = train$class,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_params,
  verbose = FALSE
)

# Get the best hyperparameters
best_params <- cv_results$bestTune

# Train the final model using the best hyperparameters
model <- xgboost(
  data = train[, -1],
  label = train$Class,
  nrounds = best_params$nrounds,
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  gamma = best_params$gamma,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample
)

# Make predictions on the test set
predictions <- predict(model, test[, -1])

# Evaluate the model
accuracy <- mean(predictions == test$Class)

# Print the accuracy
print(accuracy)

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

set.seed(50)
# Customsing the tuning grid
gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = (1:10)*50,    # number of trees
                        # default values below
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)

# training a XGboost Regression tree model while tuning parameters
model = train(class~., data = train, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)

# summarising the results
print(model)
best_params <- gbmGrid$bestTune
View(gbmGrid)
gbmGrid
t <- as.matrix(train)
model <- xgboost(data = t,                    # the data   
                 max.depth=3,                            # max depth 
                 nrounds=50)
