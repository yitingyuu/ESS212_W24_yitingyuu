# Ess212 hw4
# Yiting

## Problem 1
t <- seq(1:10)
y <- c(-2.73,-2.71,-2.65,-0.87,-3.10,-1.03,0.63,1.46,5.90,8.38)

n <- length(y)
k <- 3  # the number of parameters 

# the design matrix
A <- cbind(1,t,t^2)

# estimate parameters
beta <- solve(t(A)%*%A) %*% t(A)%*%y

# the error vector
residuals <- y - A%*%beta

# estimate residual variance
sigma2 <- sum(residuals^2)/(n-k)

# covariance matrix
cov_matrix <- solve(t(A)%*%A)*sigma2

# standard errors of the estimates
std_errors <- sqrt(diag(cov_matrix))

# prediction for y(t = 12)
A_new <- cbind(1, 12, 12^2)
y_pred <- A_new %*% beta

# standard error of the prediction
pred_var <- A_new %*% cov_matrix %*% t(A_new)
pred_std_error <- sqrt(diag(pred_var))

# results
cat("Estimated parameters: ", beta, "\n")
cat("Standard errors: ", std_errors, "\n")
cat("Predicted y for x = 12: ", y_pred, "\n")
cat("Standard error of the prediction: ", pred_std_error, "\n")

## Problem 2
x <- c(0,1,2,3,4)
y <- c(0.0434,1.0343,-0.2588,3.68622,4.3188)
err_known <- c(0.1, 0.1) 

# estimate the error of the last three measurements
var_known <- mean(err_known^2)
weights <- c(1/err_known^2, rep(1/var_known,3))

# WLS
W <- diag(weights)  

# the design matrix
X <- cbind(1,x)

# estimate parameters
beta <- solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%y

# estimate residuals and residual variance
residuals <- y - X%*%beta
sigma2 <- sum(weights*residuals^2)/(length(y)-ncol(X))

# covariance matrix 
cov_matrix <- sigma2*solve(t(X)%*%W%*%X)

# standard errors of the coefficients
std_errors <- sqrt(diag(cov_matrix))

# results
cat("Coefficients:\n")
print(beta)
cat("\nStandard Errors:\n")
print(std_errors)

## Problem 3
y <- c(3.75, 0.93, 0.38, 0.05, 0.04,
       0.36, 0.32, 0.11, 0.15, 0.03,
       0.58, 0.67, 0.12, 0.05, 0.08,
       2.06, 1.01, 0.60, 0.11, 0.06)
log_y <- log(y)

# the design matrix X 
t <- rep(c(1,2,3,4,5), times=4)  # repeated for each y_ij
X <- cbind(1,t)  

# estimate parameters
beta <- solve(t(X)%*%X) %*% t(X)%*%log_y

# residuals and estimate sigma
n <- 20
k <- 2
residuals <- log_y - X%*%beta
sigma_est <- sqrt(sum(residuals^2)/(n-k))

# covariance matrix
cov_beta <- solve(t(X)%*%X)*sigma_est^2

# standard errors for beta
se_beta <- sqrt(diag(cov_beta))

# estimates and standard errors for mu and b
mu_est <- beta[1]
b_est <- -beta[2]  # change the sign 
mu_se <- se_beta[1]
b_se <- se_beta[2]

# results
cat("mu estimate:", mu_est, "with SE:", mu_se, "\n")
cat("b estimate:", b_est, "with SE:", b_se, "\n")
cat("sigma estimate:", sigma_est, "\n")