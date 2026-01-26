library(MASS)       
library(class)     
library(nnet)       
library(xgboost)    
library(HDclassif)  
library(tidyverse)  

load("task1.Rdata")

#S1 PCA
pca_s1 <- prcomp(train.data.s1, center = TRUE, scale = FALSE)
var_s1 <- cumsum(pca_s1$sdev^2) / sum(pca_s1$sdev^2)
no_comp_s1 <- which(var_s1 >= 0.9)[1]  #number of components which explain 90% variance
cat("S1: Number of PCs ", no_comp_s1, "\n")

#S2 PCA
pca_s2 <- prcomp(train.data.s2, center = TRUE, scale = FALSE)
var_s2 <- cumsum(pca_s2$sdev^2) / sum(pca_s2$sdev^2)
no_comp_s2 <- which(var_s2 >= 0.9)[1]
cat("S2: Number of PCs ", no_comp_s2, "\n")

plot(var_s1, type = "b", pch = 15,
     xlab = "No Principal Components",
     ylab = "Cum Prop of Variance",
     main = "Cum Variance - S1")
abline(h = 0.9, col = "blue", lty = 2)  # 90% threshold

plot(var_s2, type = "b", pch = 15,
     xlab = "No Principal Components",
     ylab = "Cum Prop of Variance",
     main = "Cum Variance - S2")
abline(h = 0.9, col = "blue", lty = 2)  # 90% threshold

# Project training and test data onto selected PCs
train.pc.s1 <- pca_s1$x[, 1:no_comp_s1]
train.pc.s2 <- pca_s2$x[, 1:no_comp_s2]

test.pc.s1 <- scale(test.data, center = pca_s1$center, scale = FALSE) %*% pca_s1$rotation[, 1:no_comp_s1]
test.pc.s2 <- scale(test.data, center = pca_s2$center, scale = FALSE) %*% pca_s2$rotation[, 1:no_comp_s2]

#Wilks' Lambda for S1
wilk_s1_pc <- manova(as.matrix(train.pc.s1) ~ train.target.s1)
summary(wilk_s1_pc, test = "Wilks")

#Wilks' Lambda for S2 
wilk_s2_pc <- manova(as.matrix(train.pc.s2) ~ train.target.s2)
summary(wilk_s2_pc, test = "Wilks")

#Wilks’ Lambda (0.03) which is very close to 0 indicating strong differences between group centroids as expected
# p-value small enough to reject the null
# hence, LDA and QDA are meaningful due to real separation in data.

# LDA
# S1: LDA
lda_s1 <- lda(train.pc.s1, grouping = train.target.s1)
print(lda_s1)

#all classes are equally likely -- balanced set
#LD1 contributes most to separation as it helps in explaining most of the trace(between-class variance)

lda_trn_pred_s1 <- predict(lda_s1)$class
#Confusion matrix 
table(lda_trn_pred_s1, train.target.s1)
#Most errors between visually similar letters D to O and G tp Q
# Posterior probabilities
lda_trn_post_s1 <- predict(lda_s1)$posterior

#most rows showcase high confidence predictions meaning LDA works decently
#most low confidence preds between visually similar letters (row 135 D and Q)

# training error
lda_trn_err_s1 <- mean(lda_trn_pred_s1 != train.target.s1)

# Predictons
lda_tst_pred_s1 <- predict(lda_s1, newdata = test.pc.s1)$class
lda_tst_err_s1 <- mean(lda_tst_pred_s1 != test.target)

table(lda_tst_pred_s1, test.target)

cat("S1 - LDA Training Error ", lda_trn_err_s1, "\n")
cat("S1 - LDA Test Error ", lda_tst_err_s1, "\n")

# S2: LDA
lda_s2 <- lda(train.pc.s2, grouping = train.target.s2)
print(lda_s2)

lda_trn_pred_s2 <- predict(lda_s2)$class
table(lda_trn_pred_s2, train.target.s2)
lda_trn_err_s2 <- mean(lda_trn_pred_s2 != train.target.s2)

lda_tst_pred_s2 <- predict(lda_s2, newdata = test.pc.s2)$class
lda_tst_err_s2 <- mean(lda_tst_pred_s2 != test.target)

cat("S2 - LDA Training Error:", lda_trn_err_s2, "\n")
cat("S2 - LDA Test Error:", lda_tst_err_s2, "\n")

#training error around 8 to 9% — reasonably good fit
#test error higher at around 11% for S1 and 13% for S2
#mild overfitting, particularly in S2 due to smaller training set
#linear separation works fairly well but cannot capture complex boundaries

# QDA
qda_s1 <- qda(train.pc.s1, grouping = train.target.s1)
print(qda_s1)
#group means identical to LDA since they work in similar ways but cater for group covariances unlike LDA

qda_trn_pred_s1 <- predict(qda_s1)$class
table(qda_trn_pred_s1, train.target.s1)
qda_trn_err_s1 <- mean(qda_trn_pred_s1 != train.target.s1)

#Most errors between D and O and between G and Q, but does better than LDA with quadratic decision boundary
qda_tst_pred_s1 <- predict(qda_s1, newdata = test.pc.s1)$class
qda_tst_err_s1 <- mean(qda_tst_pred_s1 != test.target)

cat("S1 - QDA Training Error", qda_trn_err_s1, "\n")
cat("S1 - QDA Test Error", qda_tst_err_s1, "\n")

# S2: QDA
qda_s2 <- qda(train.pc.s2, grouping = train.target.s2)
print(qda_s2)

qda_trn_pred_s2 <- predict(qda_s2)$class
table(qda_trn_pred_s2, train.target.s2)
qda_trn_err_s2 <- mean(qda_trn_pred_s2 != train.target.s2)

qda_tst_pred_s2 <- predict(qda_s2, newdata = test.pc.s2)$class
qda_tst_err_s2 <- mean(qda_tst_pred_s2 != test.target)

cat("S2 - QDA Training Error ", qda_trn_err_s2, "\n")
cat("S2 - QDA Test Error ", qda_tst_err_s2, "\n")

#low training error - much lower than LDA around 4.6% for S1 and 2.6% for S2
#lower test error than LDA as well at 5.8% for S1 and 6.8% for S2
#QDA fits the data with more flexibility thanks to the quadratic decision boundaries and generalizes better 
#QDA outperforms LDA 

#KNN

# Range of k 
k_vals <- seq(1, 50)  
no_k <- length(k_vals)

train_err <- numeric(no_k)
test_err  <- numeric(no_k)

# Loop over k
for (i in seq_along(k_vals)) {
  k <- k_vals[i]
  
  train_pred <- knn(train = train.pc.s1, test = train.pc.s1,
                    cl = train.target.s1, k = k)
  train_err[i] <- mean(train_pred != train.target.s1)
  
  test_pred <- knn(train = train.pc.s1, test = test.pc.s1,
                   cl = train.target.s1, k = k)
  test_err[i] <- mean(test_pred != test.target)
}

plot(k_vals, train_err, type="b", col="blue", pch=19, ylim=c(0, max(train_err, test_err)),
     xlab="k value", ylab="Error Rate", main="KNN Errors vs k")
lines(k_vals, test_err, type="b", col="red", pch=19)
legend("topright", legend=c("Train","Test"), col=c("blue","red"), pch=10)

# Train and test predictions using best k
train_pred_s1 <- knn(train = train.pc.s1, test = train.pc.s1,
                     cl = train.target.s1, k = 3)
test_pred_s1 <- knn(train = train.pc.s1, test = test.pc.s1,
                    cl = train.target.s1, k = 3)

knn_trn_err_s1 <- mean(train_pred_s1 != train.target.s1)
knn_tst_err_s1  <- mean(test_pred_s1 != test.target)

cat("S1 - Train Error:", knn_trn_err_s1, "\n")
cat("S1 - Test Error:", knn_tst_err_s1, "\n")

# Scenario 2
k_vals <- seq(1, 50)  
no_k <- length(k_vals)

train_err <- numeric(no_k)
test_err  <- numeric(no_k)

# Loop over k
for (i in seq_along(k_vals)) {
  k <- k_vals[i]
  
  train_pred <- knn(train = train.pc.s2, test = train.pc.s2,
                    cl = train.target.s2, k = k)
  train_err[i] <- mean(train_pred != train.target.s2)
  
  test_pred <- knn(train = train.pc.s2, test = test.pc.s2,
                   cl = train.target.s2, k = k)
  test_err[i] <- mean(test_pred != test.target)
}

plot(k_vals, train_err, type="b", col="blue", pch=19, ylim=c(0, max(train_err, test_err)),
     xlab="k value", ylab="Error Rate", main="KNN Errors vs k")
lines(k_vals, test_err, type="b", col="red", pch=19)
legend("topright", legend=c("Train","Test"), col=c("blue","red"), pch=10)


train_pred_s2 <- knn(train = train.pc.s2, test = train.pc.s2,
                     cl = train.target.s2, k = 3)
test_pred_s2 <- knn(train = train.pc.s2, test = test.pc.s2,
                    cl = train.target.s2, k = 3)

knn_trn_err_s2 <- mean(train_pred_s2 != train.target.s2)
knn_tst_err_s2  <- mean(test_pred_s2 != test.target)

cat("S2 - Train Error:", knn_trn_err_s2, "\n")
cat("S2 - Test Error:", knn_tst_err_s2, "\n")

#With larger training set, KNN has 4% training error and test error of 7.6% indicating over-fitting
# with k=1, train error is 0 but that is a highly unstable model
#With smaller training set, KNN has 6.4% training error and 12.7% test error indicating worse fit
#KNN needs larger datasets 

#Multinomial Regression
mul_s1 <- multinom(train.target.s1 ~ ., data = as.data.frame(train.pc.s1),family=multinomial,maxit=1000,hess=TRUE)
coef_s1 <- coef(mul_s1)

trn_pred_s1 <- predict(mul_s1, newdata = as.data.frame(train.pc.s1))
tst_pred_s1 <- predict(mul_s1, newdata = as.data.frame(test.pc.s1))

mlr_trn_err_s1 <- mean(trn_pred_s1 != train.target.s1)
mlr_tst_err_s1 <- mean(tst_pred_s1 != test.target)

# S2
mul_s2 <- multinom(train.target.s2 ~ ., data = as.data.frame(train.pc.s2),family=multinomial,maxit=1000,hess=TRUE)
trn_pred_s2 <- predict(mul_s2, newdata = as.data.frame(train.pc.s2))
tst_pred_s2 <- predict(mul_s2, newdata = as.data.frame(test.pc.s2))

mlr_trn_err_s2 <- mean(trn_pred_s2 != train.target.s2)
mlr_tst_err_s2 <- mean(tst_pred_s2 != test.target)

#training error around 8% and test error around 9.2% - worse than QDA (5.8%).
#maybe the relationship btw principal components and labels is non-linear,and MLR is linear in the log-odds.

#with a smaller training set, training error decreases to around 5% but test error increases to 12%
#probable under-fitting making the model too simple for the complex decision boundaries

#similar train and test errors show low over-fitting,but the low scores indicate high bias 

#Squaring terms
# Scenario 1
trn_pc_sq_s1 <- as.data.frame(train.pc.s1)
tst_pc_sq_s1 <- as.data.frame(test.pc.s1)

# Squared PCs
sqr_trn_s1 <- trn_pc_sq_s1^2
colnames(sqr_trn_s1) <- paste0(colnames(trn_pc_sq_s1), "_sq")

sqr_tst_s1 <- tst_pc_sq_s1^2
colnames(sqr_tst_s1) <- paste0(colnames(tst_pc_sq_s1), "_sq")

# Combine original + squared PCs
trn_pc_sq_s1 <- cbind(trn_pc_sq_s1, sqr_trn_s1)
tst_pc_sq_s1 <- cbind(tst_pc_sq_s1, sqr_tst_s1)

# Scenario 2
trn_pc_sq_s2 <- as.data.frame(train.pc.s2)
tst_pc_sq_s2 <- as.data.frame(test.pc.s2)

# Squared PCs
sqr_trn_s2 <- trn_pc_sq_s2^2
colnames(sqr_trn_s2) <- paste0(colnames(trn_pc_sq_s2), "_sq")

sqr_tst_s2 <- tst_pc_sq_s2^2
colnames(sqr_tst_s2) <- paste0(colnames(tst_pc_sq_s2), "_sq")

# Combine
trn_pc_sq_s2 <- cbind(trn_pc_sq_s2, sqr_trn_s2)
tst_pc_sq_s2 <- cbind(tst_pc_sq_s2, sqr_tst_s2)

library(nnet)

# Scenario 1
mul_sq_s1 <- multinom(train.target.s1 ~ ., data = trn_pc_sq_s1,family=multinomial,maxit=1000,hess=TRUE)
trn_prd_sq_s1 <- predict(mul_sq_s1, newdata = trn_pc_sq_s1)
tst_prd_sq_s1 <- predict(mul_sq_s1, newdata = tst_pc_sq_s1)

mlr_sq_trn_err_s1 <- mean(trn_prd_sq_s1 != train.target.s1)
mlr_sq_tst_err_s1 <- mean(tst_prd_sq_s1 != test.target)

# Scenario 2
mul_sq_s2 <- multinom(train.target.s2 ~ ., data = trn_pc_sq_s2,family=multinomial,maxit=1000,hess=TRUE)
trn_prd_sq_s2 <- predict(mul_sq_s2, newdata = trn_pc_sq_s2)
tst_prd_sq_s2 <- predict(mul_sq_s2, newdata = tst_pc_sq_s2)

mlr_sq_trn_err_s2 <- mean(trn_prd_sq_s2 != train.target.s2)
mlr_sq_tst_err_s2 <- mean(tst_prd_sq_s2 != test.target)

para_s1 <- coef(mul_sq_s1)   #K-1 row, p+1 col
para_s2 <- coef(mul_sq_s2)
n_sam_s1 <- length(train.target.s1)
n_sam_s2 <- length(train.target.s2)

cat("S1: K-1 classes =", nrow(para_s1),
    " Vars =", ncol(para_s1),
    " Parameters =", length(para_s1),
    " Training samples =", n_sam_s1, "\n")

cat("S2: K-1 classes =", nrow(para_s2),
    " Vars =", ncol(para_s2),
    " Parameters =", length(para_s2),
    " Training samples =", n_sam_s2, "\n")

cat("S1  Train Error:", mlr_trn_err_s1, "\n")
cat("S1  Test Error:", mlr_tst_err_s1, "\n")
cat("S2  Train Error:", mlr_trn_err_s2, "\n")
cat("S2  Test Error:", mlr_tst_err_s2, "\n")

cat("S1 (PC + PC^2) Train Error:", mlr_sq_trn_err_s1, "\n")
cat("S1 (PC + PC^2) Test Error:", mlr_sq_tst_err_s1, "\n")
cat("S2 (PC + PC^2) - Train Error:", mlr_sq_trn_err_s2, "\n")
cat("S2 (PC + PC^2) - Test Error:", mlr_sq_tst_err_s2, "\n")

#MLR on PCs alone had:
#  S1 → Train 8%, Test 9%   vs 5% and 8% Squared
#  S2 → Train 5%, Test 12% vs 0% and 16% Squared

#Adding squared PCs increases test errors for smaller sample maybe because of over-parameterization?
#S1-417 variables for only 9600 samples
#S2-393 variables for 1600 samples 
#slight over-fitting and poor generalization due to too many correlated predictors/ unstable 
#smaller training set suffers more
---------------------------------------------------------------------------------------------------------
#XGB
library(xgboost)

#labels to 0,1,2,3 
y_train_s1 <- as.numeric(train.target.s1) - 1
y_train_s2 <- as.numeric(train.target.s2) - 1
y_test     <- as.numeric(test.target) - 1

# S1
#convert to DMat for XGB
dtrain_s1 <- xgb.DMatrix(data = as.matrix(train.pc.s1), label = y_train_s1)
dtest_s1  <- xgb.DMatrix(data = as.matrix(test.pc.s1),  label = y_test)

#choose optimum number of rounds to train
set.seed(1000)
cv_s1 <- xgb.cv(
  data = dtrain_s1,
  nrounds = 300,
  nfold = 5,
  objective = "multi:softmax",
  num_class = length(unique(y_train_s1)),
  max_depth = 10,
  eta = 0.1,
  subsample = 1,
  colsample_bytree = 0.8,
  verbose = 0,
  early_stopping_rounds = 20
)

best_nrounds_s1 <- cv_s1$best_iteration

# Train final model
xgb_s1 <- xgb.train(
  data = dtrain_s1,
  nrounds = 100,
  objective = "multi:softmax",
  num_class = length(unique(y_train_s1)),
  max_depth = 10,
  eta =0.5,
  subsample = 1,
  colsample_bytree = 1
)

# Predictions
pred_train_s1 <- predict(xgb_s1, dtrain_s1)
pred_test_s1  <- predict(xgb_s1, dtest_s1)

# Error rates
xgbun_trn_err_s1 <- mean(pred_train_s1 != y_train_s1)
xgbun_tst_err_s1  <- mean(pred_test_s1 != y_test)

cat("S1 - Train Error:", xgbun_trn_err_s1, "\n")
cat("S1 - Test Error:",  xgbun_tst_err_s1,  "\n")

# S2

dtrain_s2 <- xgb.DMatrix(data = as.matrix(train.pc.s2), label = y_train_s2)
dtest_s2  <- xgb.DMatrix(data = as.matrix(test.pc.s2),  label = y_test)

# Cross-validation
set.seed(1000)
cv_s2 <- xgb.cv(
  data = dtrain_s2,
  nrounds = 00,
  nfold = 5,
  objective = "multi:softmax",
  num_class = 4,
  max_depth = 10,
  eta = 0.1,
  subsample = 1,
  colsample_bytree = 0.8,
  verbose = 0,
  early_stopping_rounds = 20
)

best_nrounds_s2 <- cv_s2$best_iteration

# Final model
xgb_s2 <- xgb.train(
  data = dtrain_s2,
  nrounds = 100,
  objective = "multi:softmax",
  num_class = length(unique(y_train_s2)),
  max_depth = 10,
  eta = 0.5,
  subsample = 1,
  colsample_bytree = 1
)

# Predictions
pred_train_s2 <- predict(xgb_s2, dtrain_s2)
pred_test_s2  <- predict(xgb_s2, dtest_s2)

# Error rates
xgbun_trn_err_s2 <- mean(pred_train_s2 != y_train_s2)
xgbun_tst_err_s2  <- mean(pred_test_s2 != y_test)

cat("S2 - Train Error:", xgbun_trn_err_s2, "\n")
cat("S2 - Test Error:",  xgbun_tst_err_s2,  "\n")

#Training error 0 % - capture complex patterns
#however, high test error 6.5% - over-fitting, common for boosting
#better than LDA and comparable to KNN but worse than QDA(5.8%) needs tuning
#For S2, Training error = 0 which may be over-fitting on the smaller data set (1600 samples).
#Test error around 11% indicating reduced generalization due to fewer training samples.
#better than MLR (~16.4%) and comparable to LDA (~12.7%) in S2
#Over-fitting is more pronounced in S2 = needs careful tuning 

#Tuning
#choose optimum number of rounds to train
set.seed(1000)
cv_tune_s1 <- xgb.cv(
  data = dtrain_s1,
  nrounds = 300,
  nfold = 5,
  objective = "multi:softmax",
  num_class = length(unique(y_train_s1)),
  max_depth = 6,
  eta = 0.2,
  subsample = 0.8,
  colsample_bytree = 1,
  verbose = 0,
  early_stopping_rounds = 20
)

best_nrounds_tune_s1 <- cv_tune_s1$best_iteration

# Train final model
xgb_tune_s1 <- xgb.train(
  data = dtrain_s1,
  nrounds = best_nrounds_tune_s1,
  objective = "multi:softmax",
  num_class = length(unique(y_train_s1)),
  max_depth = 6,
  eta = 0.2,
  subsample = 0.8,
  colsample_bytree = 1,
  verbose = 0
)

# Predictions
pred_trn_tune_s1 <- predict(xgb_tune_s1, dtrain_s1)
pred_tst_tune_s1  <- predict(xgb_tune_s1, dtest_s1)

# Error rates
xgb_trn_err_s1 <- mean(pred_trn_tune_s1 != y_train_s1)
xgb_tst_err_s1  <- mean(pred_tst_tune_s1 != y_test)

cat("S1 - Train Error:", xgb_trn_err_s1, "\n")
cat("S1 - Test Error:",  xgb_tst_err_s1,  "\n")


#choose optimum number of rounds to train
set.seed(1000)
cv_tune_s2 <- xgb.cv(
  data = dtrain_s2,
  nrounds = 300,
  nfold = 5,
  objective = "multi:softmax",
  num_class = length(unique(y_train_s2)),
  max_depth = 4,
  eta = 0.2,
  subsample = 0.8,
  colsample_bytree = 0.9,
  verbose = 0,
  early_stopping_rounds = 20
)

best_nrounds_tune_s2 <- cv_tune_s2$best_iteration

# Train final model
xgb_tune_s2 <- xgb.train(
  data = dtrain_s2,
  nrounds = best_nrounds_tune_s2,
  objective = "multi:softmax",
  num_class = length(unique(y_train_s2)),
  max_depth = 4,
  eta = 0.2,
  subsample = 0.8,
  colsample_bytree = 0.9,
  verbose = 0
)

# Predictions
pred_trn_tune_s2 <- predict(xgb_tune_s2, dtrain_s2)
pred_tst_tune_s2  <- predict(xgb_tune_s2, dtest_s2)

# Error rates
xgb_trn_err_s2 <- mean(pred_trn_tune_s2 != y_train_s2)
xgb_tst_err_s2  <- mean(pred_tst_tune_s2 != y_test)

cat("S2 - Train Error:", xgb_trn_err_s2, "\n")
cat("S2 - Test Error:",  xgb_tst_err_s2,  "\n")

#on setting number of trees to 6 from 10 and colsample_bytree to 1, test error S1 reduces to 6%
#on setting number of trees to 4 from 10 and colsample_bytree to 0.8, eta=0.2 test error S2 reduces to 9.7%
#n_rounds_s1 = 172 which is > n_rounds_s2 (116) -- helps prevent overfitting
#these may be the best parameters since the number of trees being less for less samples makes sense 
#can help prevent overfitting

#HDDA
#using raw data since we HDDA uses dim red
hdda_s1 <- hdda(train.data.s1, train.target.s1, model = "AKJBKQKD", threshold = 0.05)
hdda_trn_pred_s1 <- predict(hdda_s1, data = train.data.s1)$class
hdda_trn_err_s1 <- mean(hdda_trn_pred_s1 != train.target.s1)
hdda_tst_pred_s1 <- predict(hdda_s1, data = test.data)$class
hdda_tst_err_s1 <- mean(hdda_tst_pred_s1 != test.target)

hdda_s2 <- hdda(train.data.s2, train.target.s2, model = "AKJBKQKD", threshold = 0.05)
hdda_trn_pred_s2 <- predict(hdda_s2, data = train.data.s2)$class
hdda_trn_err_s2 <- mean(hdda_trn_pred_s2 != train.target.s2)
hdda_tst_pred_s2 <- predict(hdda_s2, data = test.data)$class
hdda_tst_err_s2 <- mean(hdda_tst_pred_s2 != test.target)

#Training Error of around 6.7% for S1 and 3.9% for S2 indicating good fit to training data.
#avoids overfitting by using dim reduction at higher dims
#Test Error around 8.3% for S1 and 8.6% for S2 which shows very good generalization.
#HDDA still generalizes well showing robustness with small training sets.

model_names <- rep(
  c(
    "LDA",
    "QDA",
    "KNN",
    "Multinomial LogReg",
    "Multinomial LogReg (Squared)",
    "XGB Untuned",    
    "XGB Tuned",
    "HDDA"
  ),
  each = 2
)

train_errors <- round(c(lda_trn_err_s1, lda_trn_err_s2,
                        qda_trn_err_s1, qda_trn_err_s2,
                        knn_trn_err_s1, knn_trn_err_s2,
                        mlr_trn_err_s1, mlr_trn_err_s2,
                        mlr_sq_trn_err_s1, mlr_sq_trn_err_s2,
                        xgbun_trn_err_s1, xgbun_trn_err_s2,
                        xgb_trn_err_s1,xgb_trn_err_s2,
                        hdda_trn_err_s1, hdda_trn_err_s2), 3)

test_errors <- round(c(lda_tst_err_s1, lda_tst_err_s2,
                       qda_tst_err_s1, qda_tst_err_s2,
                       knn_tst_err_s1, knn_tst_err_s2,
                       mlr_tst_err_s1, mlr_tst_err_s2,
                       mlr_sq_tst_err_s1, mlr_sq_tst_err_s2,
                       xgbun_tst_err_s1, xgbun_tst_err_s2,
                       xgb_tst_err_s1,xgb_tst_err_s2,
                       hdda_tst_err_s1, hdda_tst_err_s2), 3)

results <- data.frame(
  Model = model_names,
  Scenario = rep(c("Scenario 1", "Scenario 2"), each = 1),
  Train_Error = train_errors,
  Test_Error = test_errors
)

print(results)

# plotting the error rates

results_long <-results %>%
  pivot_longer(
    cols = c("Train_Error", "Test_Error"),
    names_to = "Error_Type",
    values_to = "Error_Value"
  )

results_long$Error_Type <- factor(results_long$Error_Type, 
                                  levels = c("Train_Error", "Test_Error"))

ggplot(results_long, aes(x = Model, y = Error_Value, fill = Scenario)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~Error_Type) +
  labs(
    title = "Overview of model performance",
    y = "Error Rate",
    x = "Method"
  ) +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        legend.position = "top") +
  scale_fill_manual(name = "", values = c("#52BDEC", "#00407A"))

#S1
#Best performing based on test error
#QDA 
#XGBoost 
#KNN  

#Moderate performance: HDDA

#Poor performers:
#MR & MR squared 
#LDA

#Non-linear classifiers do better due to their curved decision boundaries 

#S2
#Top performers
#QDA 
#HDDA
#XGBoost

#Poor performers
#MR & MR squared 
#LDA
#KNN

#With fewer samples, flexible models like XGBoost and KNN overfit more easily.
#HDDA remains stable, showing robustness to small sample size in high dimensions.
#QDA is still best due to nonlinear decision boundaries and small data is sufficient for a 4-class problem.


