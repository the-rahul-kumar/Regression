library(leaps)
library(glmnet)

set.seed(1992)

beta_0 <- 3
beta_1 <- -1.5
beta_2 <- 4.5
beta_3 <- 0.75
X <- rnorm(100, 2, 2)
epsilon <- rnorm(100, 0, 1)
Y <- beta_0 + beta_1*X + beta_2*X^2 + beta_3*X^3 + epsilon

# Part 1
df <- data.frame(Y, X)
regfit.full <- regsubsets(Y ~ poly(X, 10, raw = TRUE), data = df, nvmax = 10)
reg.summary <- summary(regfit.full)

data.frame(
  Adj.R2 = which.max(reg.summary$adjr2),
  CP = which.min(reg.summary$cp),
  BIC = which.min(reg.summary$bic)
)

coef(regfit.full,which.min(reg.summary$bic))

# Part 2
regfit.full.forward <- regsubsets(Y ~ poly(X, 10, raw = TRUE), data = df, nvmax = 10, method="forward")
reg.summary.forward <- summary(regfit.full.forward)

coef(regfit.full.forward,which.min(reg.summary.forward$bic))

regfit.full.backward <- regsubsets(Y ~ poly(X, 10, raw = TRUE), data = df, nvmax = 10, method="backward")
reg.summary.backward <- summary(regfit.full.backward)

coef(regfit.full.backward,which.min(reg.summary.backward$bic))

# Part 3&4
X_matrix <- model.matrix(Y ~ poly(X, 10, raw = TRUE), data = df)[, -1]
set.seed(1)
lasso_cv <- cv.glmnet(X_matrix, Y, alpha=1)
bestlam <- lasso_cv$lambda.min
bestlam

out=glmnet(X_matrix,Y,alpha=1)
predict(out, type="coefficients",s=bestlam)

set.seed(1992)
beta_7 <- 6.7
Y_2 <- beta_0 + beta_7*X^7 + epsilon
df_2 <- data.frame(Y_2, X)

# Part 5&6&7
regfit.full.2 <- regsubsets(Y_2 ~ poly(X, 10, raw = TRUE), data = df_2, nvmax = 10)
reg.summary.2 <- summary(regfit.full.2)

data.frame(
  Adj.R2 = which.max(reg.summary.2$adjr2),
  CP = which.min(reg.summary.2$cp),
  BIC = which.min(reg.summary.2$bic))
  

X_matrix_2 <- model.matrix(Y_2 ~ poly(X, 10, raw = TRUE), data = df_2)[, -1]
set.seed(1)
lasso_cv.2 <- cv.glmnet(X_matrix_2, Y_2, alpha=1)
bestlam.2 <- lasso_cv.2$lambda.min
bestlam.2
  
out.2=glmnet(X_matrix_2,Y_2,alpha=1)
predict(out.2, type="coefficients",s=bestlam.2)