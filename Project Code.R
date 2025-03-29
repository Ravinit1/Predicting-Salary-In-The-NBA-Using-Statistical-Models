set.seed(100)

#Load Data
NBA <- read.csv(file.choose())
NBA

#Adjust Salary For In Millions
NBA$Salary_In_Millions <- NBA$Salary / 1e6

#Split Dataset (80% train, 20% test)
train_indices <- sample(1:nrow(NBA), size = 0.8 * nrow(NBA))  
train_data <- NBA[train_indices, ]
test_data <- NBA[-train_indices, ]

#EDA 

#Heatmap
install.packages("corrplot")
install.packages("ggplot")
install.packages("ggplot2")
library(ggplot2)
#Correlation Matrix
cor_matrix <- cor(NBA[, c("PTS", "TRB", "AST", "STL", "BLK", 
                          "Salary_In_Millions")], use = "pairwise.complete.obs")
#Create the correlation plot
cor_data <- as.data.frame(as.table(cor_matrix))
#Plot the correlation plot 
ggplot(cor_data, aes(Var1, Var2, fill = Freq)) + 
  geom_tile() + 
  geom_text(aes(label = round(Freq, 2)), color = "white", size = 4) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation", 
       title = "Heatmap of the Correlation Matrix")

#Scatterplot
plot(NBA$Salary, NBA$FG., col = 'darkgreen', pch = 19, 
     main = "Scatterplot of Salary vs. Field Goals", 
     xlab = "Salary", ylab = "Field Goals")
plot(NBA$Salary, NBA$Age, col = 'darkred', pch = 19,
     main = "Scatterplot of Salary vs. Age", 
     xlab = "Salary", ylab = "Age")

#----------------------------------------------------------------------------------------------------
#Now Analysis 
#Linear Reg
model_lin <- lm(Salary_In_Millions ~ PTS + TRB + AST + STL + BLK, data = NBA)
model_lin
predictions <- predict(model_lin, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#48.7839778920473

#Linear Reg Poly
#2 degree 
model_squared <- lm(Salary_In_Millions ~ PTS + I(PTS^2) + TRB + I(TRB^2) + AST + 
                      I(AST^2) + STL + I(STL^2) + BLK + I(BLK^2), data = NBA)
predictions <- predict(model_squared, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#46.3943279026368

#3 degree
model_cubed <- lm(Salary_In_Millions ~ PTS + I(PTS^2) + I(PTS^3) + TRB + 
                    I(TRB^2) + I(TRB^3) + AST + I(AST^2) + I(AST) + STL + I(STL^2) 
                  + I(STL^3) + BLK + I(BLK^2) + I(BLK^3), data = NBA)
predictions <- predict(model_cubed, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#46.63

#4 degree 
model_fourth <- lm(Salary_In_Millions ~ 
                     PTS + I(PTS^2) + I(PTS^3) + I(PTS^4) + 
                     TRB + I(TRB^2) + I(TRB^3) + I(TRB^4) + 
                     AST + I(AST^2) + I(AST^3) + I(AST^4) + 
                     STL + I(STL^2) + I(STL^3) + I(STL^4) + 
                     BLK + I(BLK^2) + I(BLK^3) + I(BLK^4), 
                   data = NBA)
predictions <- predict(model_fourth, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#48.1216713324094

#5 degree
model_fifth <- lm(Salary_In_Millions ~ 
                    PTS + I(PTS^2) + I(PTS^3) + I(PTS^4) + I(PTS^5) + 
                    TRB + I(TRB^2) + I(TRB^3) + I(TRB^4) + I(TRB^5) + 
                    AST + I(AST^2) + I(AST^3) + I(AST^4) + I(AST^5) + 
                    STL + I(STL^2) + I(STL^3) + I(STL^4) + I(STL^5) + 
                    BLK + I(BLK^2) + I(BLK^3) + I(BLK^4) + I(BLK^5), 
                  data = NBA)
predictions <- predict(model_fifth, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#47.1025466648859

#Linear Reg Stepwise
#2nd degree
set.seed(100)
NBA$cut_pts <- cut(NBA$PTS, breaks = 2)
NBA$cut_trb <- cut(NBA$TRB, breaks = 2)
NBA$cut_ast <- cut(NBA$AST, breaks = 2)
NBA$cut_stl <- cut(NBA$STL, breaks = 2)
NBA$cut_blk <- cut(NBA$BLK, breaks = 2)

train_indices <- sample(1:nrow(NBA), size = 0.8 * nrow(NBA))  # 80% for training
train_data <- NBA[train_indices, ]
test_data <- NBA[-train_indices, ]

model_2 <- lm(Salary_In_Millions ~ cut_pts + cut_trb + cut_ast 
              + cut_stl + cut_blk, data = NBA)
predictions <- predict(model_2, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#63.96

#3rd degree 
set.seed(100)
NBA$cut_pts <- cut(NBA$PTS, breaks = 3)
NBA$cut_trb <- cut(NBA$TRB, breaks = 3)
NBA$cut_ast <- cut(NBA$AST, breaks = 3)
NBA$cut_stl <- cut(NBA$STL, breaks = 3)
NBA$cut_blk <- cut(NBA$BLK, breaks = 3)

train_indices <- sample(1:nrow(NBA), size = 0.8 * nrow(NBA))  # 80% for training
train_data <- NBA[train_indices, ]
test_data <- NBA[-train_indices, ]

model_3 <- lm(Salary_In_Millions ~ cut_pts + cut_trb + cut_ast 
              + cut_stl + cut_blk, data = NBA)
predictions <- predict(model_3, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#55.618

#4th degree
set.seed(100)
NBA$cut_pts <- cut(NBA$PTS, breaks = 4)
NBA$cut_trb <- cut(NBA$TRB, breaks = 4)
NBA$cut_ast <- cut(NBA$AST, breaks = 4)
NBA$cut_stl <- cut(NBA$STL, breaks = 4)
NBA$cut_blk <- cut(NBA$BLK, breaks = 4)

train_indices <- sample(1:nrow(NBA), size = 0.8 * nrow(NBA))  # 80% for training
train_data <- NBA[train_indices, ]
test_data <- NBA[-train_indices, ]

model_4 <- lm(Salary_In_Millions ~ cut_pts + cut_trb + 
                cut_ast + cut_stl + cut_blk, data = NBA)
predictions <- predict(model_4, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#56.7826951146235

#5th degree
set.seed(100)
NBA$cut_pts <- cut(NBA$PTS, breaks = 5)
NBA$cut_trb <- cut(NBA$TRB, breaks = 5)
NBA$cut_ast <- cut(NBA$AST, breaks = 5)
NBA$cut_stl <- cut(NBA$STL, breaks = 5)
NBA$cut_blk <- cut(NBA$BLK, breaks = 5)

train_indices <- sample(1:nrow(NBA), size = 0.8 * nrow(NBA))  # 80% for training
train_data <- NBA[train_indices, ]
test_data <- NBA[-train_indices, ]

model_5 <- lm(Salary_In_Millions ~ cut_pts + cut_trb + cut_ast + 
                cut_stl + cut_blk, data = NBA)
predictions <- predict(model_5, newdata = test_data)
residuals <- test_data$Salary_In_Millions - predictions
mse <- mean(residuals^2)
print(paste("Test MSE: ", mse))
#52.8747391814419

#Lasso
install.packages("glmnet")
library(glmnet)
x_train <- as.matrix(train_data[, c("PTS", "TRB", "AST", "STL", "BLK")]) 
y_train <- train_data$Salary_In_Millions 
x_test <- as.matrix(test_data[, c("PTS", "TRB", "AST", "STL", "BLK")])    
y_test <- test_data$Salary_In_Millions 
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5) #alpha=1 = Lasso 
lasso_cv
# Best lambda from cross-validation
best_lambda <- lasso_cv$lambda.min
best_lambda

#Fit model with best lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

#Get MSE 
predictions_lasso <- predict(lasso_model, s = best_lambda, newx = x_test)
residuals <- predictions_lasso - y_test
mse <- mean(residuals^2)

#Print MSE
print(paste("Test MSE: ", mse))
#51.1223041352134

#Regression Tree
library(tree)

#Fit the regression tree model
tree_model <- tree(Salary_In_Millions ~ PTS + TRB + AST + STL + BLK, 
                   data = train_data)

#Plot the tree
plot(tree_model)
text(tree_model, pretty = 0)

#Filter data for a specific section because it seems like an outlier
library(dplyr)

filtered_data <- NBA %>%
  filter(PTS < 13.2, AST >= 5, AST <= 6.12)
print(filtered_data)

test_predictions <- predict(tree_model, newdata = test_data)

#Get MSE
test_residuals <- test_data$Salary_In_Millions - test_predictions
test_mse <- mean(test_residuals^2)

#Print the test MSE
print(paste("Test MSE: ", test_mse))
#68.8909458580188

#---------------------------------------------------------------------------------
#Now For Checking Part On Median/Mean Data
#check using mean and median prediction
ast_mean <- mean(NBA$AST)
trb_mean <- mean(NBA$TRB)
pts_mean <- mean(NBA$PTS)
stl_mean <- mean(NBA$STL)
blk_mean <- mean(NBA$BLK)

ast_median <- median(NBA$AST)
trb_median <- median(NBA$TRB)
pts_median <- median(NBA$PTS)
stl_median <- median(NBA$STL)
blk_median <- median(NBA$BLK)

NBA$AST2 <- NBA$AST^2
NBA$TRB2 <- NBA$TRB^2
NBA$PTS2 <- NBA$PTS^2
NBA$STL2 <- NBA$STL^2
NBA$BLK2 <- NBA$BLK^2

#Get Actual Data
model_squared <- lm(Salary_In_Millions ~ PTS + I(PTS^2) + TRB + I(TRB^2) 
                    + AST + I(AST^2) + STL + I(STL^2) + BLK + I(BLK^2), 
                    data = NBA)

#Get Mean Values And Get Average Salary With Player Having Mean Stats
mean_values_poly <- data.frame(
  PTS = pts_mean, TRB = trb_mean, AST = ast_mean, STL = stl_mean, 
  BLK = blk_mean, PTS2 = pts_mean^2, TRB2 = trb_mean^2, AST2 = ast_mean^2, 
  STL2 = stl_mean^2, BLK2 = blk_mean^2
)
mean_values_poly
predicted_salary_poly <- predict(model_squared, newdata = mean_values_poly)
print(predicted_salary_poly)

#Get Median Values And Get Average Salary With Player Having Median Stats
median_values_poly <- data.frame(
  PTS = pts_median, TRB = trb_median, AST = ast_median, STL = stl_median, 
  BLK = blk_median, PTS2 = pts_median^2, TRB2 = trb_median^2, 
  AST2 = ast_median^2, STL2 = stl_median^2, BLK2 = blk_median^2
)
median_values_poly
predicted_salary_poly_median <- predict(model_squared, 
                                        newdata = median_values_poly)
print(predicted_salary_poly_median)

#Actual Mean And Median Value In Dataset
mean(NBA$Salary_In_Millions)
median(NBA$Salary_In_Millions)

#Plot
library(ggplot2)

# Create a histogram of Salary_In_Millions with specific bins
ggplot(NBA, aes(x = Salary_In_Millions)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of NBA Salaries in Millions",
       x = "Salary in Millions",
       y = "Frequency") +
  theme_minimal()

#More analysis
library(dplyr)
# Filter the dataset for players with a salary less than 10 million
players_under_10mil <- NBA %>% filter(Salary_In_Millions < 10)

#Count the number of players with salary under 10 million
num_players_under_10mil <- nrow(players_under_10mil)

#Print the result
print(num_players_under_10mil)

mean(NBA$Salary_In_Millions)
sum(NBA$Salary_In_Millions > 25)


#Boxplot
boxplot(NBA$Salary_In_Millions,
        horizontal = TRUE, 
        main = "NBA Salary Distribution",
        xlab = "Salary (in Millions)")


#Summary Statistics 
summary(NBA)

#Scree Plot
# Prepare the predictor variables (excluding Salary_In_Millions, which is the response)
predictors <- NBA[, c("PTS", "TRB", "AST", "STL", "BLK")]

# Scale the data (important for PCA)
predictors_scaled <- scale(predictors)

# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(predictors_scaled, center = TRUE, scale. = TRUE)

# Plot the Scree Plot to show the proportion of variance explained
screeplot(pca_result, main = "PCA Screen Plot", ol = "green", type = "lines", pch = 19)

# Optionally, if you'd like to view the proportion of variance explained by each component:
summary(pca_result)


