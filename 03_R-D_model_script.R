library(readr)
library(dplyr)
library(randomForest)
library(MASS)
library(psych)
library(caret)
library(ggplot2)
library(ROCR)
library(RColorBrewer)
library(e1071)
library(naivebayes)
library(rminer)

#**********************************************************************************************************************************************************************************************
# Code for: (1) Build prediction models and visualisation of variable importance
#           (2) Model comparison and visualisation of ROCs and AUCs
#**********************************************************************************************************************************************************************************************

#===STEP 1 (Build prediction models and variable importance vis) ==============================================================================================================================
data <- read_csv("data_step2.csv")
data$ABN <- as.character(data$ABN)

#coerce to factors:
cols <- c("trading_status", "company_status", "country_of_incorporation", "headquarters_city", "headquarters_state", "headquarters_country",
          "primary_industry", "GICS_sector", "GICS_industry_group", "GICS_industry", "has_RD_exp")
data[cols] <- lapply(data[cols], factor)
data <- data[,c(1:36,38:50,37)]

set.seed(1234) #for reproducibility

#---Random Forest Model 1: drop incomplete cases
data_ready <- data[,-c(1:4,8,12,15)] #remove metadata amd factors with extreme no. levels

#split the data and build training and testing datasets:
sample_size <- floor(0.75 * nrow(data_ready)) #size of training dataset (75% of data)
train_indices <- sample(nrow(data_ready), size = sample_size) #get list of indices for training dataset
train_data <- data_ready[train_indices, ] #build training dataset
test_data <- data_ready[-train_indices, ] #build testing dataset

#build a random forest predictive model:
rf_model1 <- randomForest::randomForest(has_RD_exp ~., data = train_data, ntree = 5000) #build a random forest predictive model
rf_model1

#calculate accuracy with testing data:
pred <- predict(rf_model1, newdata = test_data[,!(names(test_data) == "has_RD_exp")]) #predict labels in test data using model built
cm <- table(test_data$has_RD_exp, pred) #confusion matrix using true outcome vs predicted outcome
rf_model1_acc <- sum(diag(cm)) / (sum(cm)) * 100 #the cases predicted correctly, divided by total cases

#Visualise variable importance:
df_varimp <- caret::varImp(rf_model1)
df_varimp$Variable <- factor(row.names(df_varimp), levels = row.names(df_varimp)[order(df_varimp$Overall)])

ggplot(df_varimp, aes(x=Variable, y=Overall)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 4, color = "orange") +
  scale_y_continuous(limits = c(0,max(df_varimp$Overall) + 5), expand = c(0, 0)) +
  coord_flip() +
  theme_minimal()
#---end


#---Logistic regression Model 1:
  #(using the train and test dataset used for random forest 1)

# Build a stepwise logistic regression model:
lr_model1 <- glm(has_RD_exp ~., data = train_data, family = "binomial") %>%
  MASS::stepAIC(trace = FALSE) #this performs stepwise variable selection

summary(lr_model1)

#calculate accuracy with testing data:
pred2 <- predict(lr_model1, newdata = test_data[,!(names(test_data) == "has_RD_exp")]) #predict labels in test data using model built
pred_classes2 <- ifelse(pred2 > 0.5, "yes", "no") #turn the predictions from continuous [0,1] to dichotomous, as they should be
cm2 <- table(test_data$has_RD_exp, pred_classes2) #confusion matrix using true outcome vs predicted outcome
lr_model1_acc <- sum(diag(cm2)) / (sum(cm2)) * 100 #the cases predicted correctly, divided by total cases

#Visualise variable importance:
df_varimp <- caret::varImp(lr_model1)
df_varimp$Variable <- factor(row.names(df_varimp), levels = row.names(df_varimp)[order(df_varimp$Overall)])

ggplot(df_varimp, aes(x=Variable, y=Overall)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 4, color = "orange") +
  scale_y_continuous(limits = c(0,max(df_varimp$Overall) + 5), expand = c(0, 0)) +
  coord_flip() +
  theme_minimal()
#---end


#---Naive Bayes Model 1:
#(using the train and test dataset used for random forest 1)
nb_model1 <- caret::train(x = as.data.frame(train_data[,!(names(train_data) == "has_RD_exp")]),
                          y = train_data$has_RD_exp,
                          method = "naive_bayes",
                          preProcess=c("scale","center"),
                          trControl = trainControl(method = 'cv', number = 10))
nb_model1
  #tried with features selected through stepwise reg (for logistic reg above), and performance is poorer = 79% acc
# nb_model1 <- caret::train(x = as.data.frame(train_data[,c(7,19,6,15,9,29,14,17,39,36,31,20,41,2)]),
#                           y = train_data$has_RD_exp,
#                           method = "naive_bayes",
#                           preProcess=c("scale","center"),
#                           trControl = trainControl(method = 'cv', number = 10))
# nb_model1
#calculate accuracy with testing data:
pred3 <- predict(nb_model1, newdata = test_data[,!(names(test_data) == "has_RD_exp")]) #predict labels in test data using model built
cm3 <- table(test_data$has_RD_exp, pred3) #confusion matrix using true outcome vs predicted outcome
nb_model1_acc <- sum(diag(cm3)) / (sum(cm3)) * 100 #the cases predicted correctly, divided by total cases

#Visualise variable importance:
df_varimp <- caret::varImp(nb_model1)
df_varimp <- as.data.frame(df_varimp$importance)
colnames(df_varimp)[1] <- "Overall"
df_varimp$Variable <- factor(row.names(df_varimp), levels = row.names(df_varimp)[order(df_varimp$Overall)])
df_varimp <- df_varimp[,c(3,1)]

ggplot(df_varimp, aes(x=Variable, y=Overall)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 4, color = "orange") +
  scale_y_continuous(limits = c(0,max(df_varimp$Overall) + 5), expand = c(0, 0)) +
  coord_flip() +
  theme_minimal()
#---end

#===End STEP 1 ===============================================================================================================================================================================




#===STEP 2 (Model comparison and visualisation of ROCs and AUCs) =============================================================================================================================
palette <- brewer.pal(n = 5, name = "Set2")
pred_rf1 <- predict(rf_model1, newdata = test_data[,!(names(test_data) == "has_RD_exp")], type = "prob") #predict labels in test data using model built
pred_ROC <- ROCR::prediction(pred_rf1[,2], test_data$has_RD_exp)
perf_ROC <- ROCR::performance(pred_ROC, "tpr", "fpr")
auc_rf1 <- ROCR::performance(pred_ROC, measure = "auc")
auc_rf1 <- round(auc_rf1@y.values[[1]], 2)
plot(perf_ROC, col = palette[1], lwd = 2, main = "ROC curves of the different classifiers", xaxs = "i", yaxs = "i")

pred_lr1 <- predict(lr_model1, newdata = test_data[,!(names(test_data) == "has_RD_exp")], type = "response") #predict labels in test data using model built
pred_ROC_2 <- ROCR::prediction(pred_lr1, test_data$has_RD_exp)
perf_ROC_2 <- ROCR::performance(pred_ROC_2, "tpr", "fpr")
auc_lr1 <- ROCR::performance(pred_ROC_2, measure = "auc")
auc_lr1 <- round(auc_lr1@y.values[[1]], 2)
plot(perf_ROC_2, col = palette[2], lwd = 2, add = T, xaxs = "i", yaxs = "i")

pred_nb1 <- predict(nb_model1, newdata = test_data[,!(names(test_data) == "has_RD_exp")], type = "prob") #predict labels in test data using model built
pred_ROC_3 <- ROCR::prediction(pred_nb1[,2], test_data$has_RD_exp)
perf_ROC_3 <- ROCR::performance(pred_ROC_3, "tpr", "fpr")
auc_nb1 <- ROCR::performance(pred_ROC_3, measure = "auc")
auc_nb1 <- round(auc_nb1@y.values[[1]], 2)
plot(perf_ROC_3, col = palette[3], lwd = 2, add = T, xaxs = "i", yaxs = "i")

abline(a=0, b= 1, col = "grey75", lwd = 2)
legend(0.4, 0.3, c(paste0("Random forest", " | AUC: ", auc_rf1), 
                   paste0("Logistic regression", " | AUC: ", auc_lr1),
                   paste0("Naive Bayes", " | AUC: ", auc_nb1)
                   ), palette[1:3], border = "white", 
       bty = "n", text.col = "grey30", x.intersp = .1)
box(col = "grey60", lwd = 1)
#===End STEP 2 ===============================================================================================================================================================================





