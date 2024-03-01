# ess212 hw3
# yiting

library(MASS) 
library(numDeriv) 
library(httr)
library(ggplot2)
library(readr)

# Problem 1
A <- matrix(c(16, -4, 1, 0, 0, 1, 16, 4, 1), 3, byrow = TRUE)
d <- c(30, 2, 6)

beta <- solve(A, d)
#beta <- solve(t(A) %*% A) %*% t(A) %*% d

d_hat <- A %*% beta
e <- d - d_hat

# result
beta
e
#-------------------------------------
fn <- function(beta, x, y) {
  A <- beta[1]
  B <- beta[2]
  C <- beta[3]
  
  F <- numeric(length(y))
  
  for(i in 1:length(y)) {
    F[i] <- A*(x[i]-B)*(x[i]-C) - y[i]
  }
  return(F)
}

# A, B, C for initial iterate
beta <- c(A=1, B=2, C=3)

x <- c(-4, 0, 4) 
y <- c(30, 2, 6) 

# Newton's method 
n <- 100
for (iter in 1:n) {
  F <- fn(beta, x, y)
  J <- jacobian(func = function(b) fn(b, x, y), x = beta)
  delta <- solve(J) %*% F
  beta <- beta - delta
  if (max(abs(delta)) <  1e-6) {
    cat("Convergence achieved after", iter, "iterations.\n")
    break
  }
}

# result
cat("Estimates for A, B, C:\n")
print(beta)
#------------------------------------------
B_range <- seq(0, 5, length.out = 200)
C_range <- seq(0, 5, length.out = 200)

x <- c(-4, 0, 4)
y <- c(30, 2, 6)

fn_F <- function(B, C, x, y) {
  A <- 1
  F <- numeric(length(x))
  for (i in 1:length(x)) {
    F[i] <- A*(x[i]-B)*(x[i]-C) - y[i]
  }
  return(F)
}

# initialize matrix to store convergence results
con_grid <- matrix(0, length(B_range), length(C_range))

# iterate over the ranges of B and C
for (i in 1:length(B_range)) {
  for (j in 1:length(C_range)) {
    F <- fn_F(B_range[i], C_range[j], x, y)
    con_criteria <- sum(abs(F))
    con_grid[i, j] <- ifelse(con_criteria < 1, 1, 0)
  }
}

colors <- colorRampPalette(c("lightblue", "aliceblue", "white"))(n = 2)  

image(x = B_range, y = C_range, z = con_grid, col = colors, xlab = "B", ylab = "C", 
      main = "Convergence Map")
legend("topright", legend = c("Not Converged", "Converged"), fill = c(colors[1], colors[2]))


# Problem 2
# download the GISTEMP dataset
url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt"
save_f <- tempfile()
GET(url, write_disk(save_f))

# clean the dataset
lines <- read_lines(save_f)

# Filter lines to include only those with numeric data at the beginning (years)
filter_l <- grep("^\\s*\\d{4}", lines, value = TRUE)

# determine the maximum number of columns
max_cols <- max(sapply(filter_l, function(line) length(strsplit(line, "\\s+")[[1]])))

col_names <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "J-D", "D-N", "DJF", "MAM", "JJA", "SON")
extra_cols <- max_cols - length(col_names)
if(extra_cols > 0) {
  col_names <- c(col_names, paste0("Extra", 1:extra_cols))
}

# combine the filtered lines into a single character vector
data_text <- paste(filter_l, collapse = "\n")

# read the combined text as a table
data <- read.table(text = data_text, header = FALSE, fill = TRUE, col.names = col_names)
clean_d <- data[data$`J.D` != "****", ]

#----------------------------------------

# fit the model
yrs <- as.numeric(clean_d$Year)
mid_point <- mean(yrs)
annual_means <- as.numeric(clean_d$`J.D`) # `annual_means` is the mean of the annual temperature

model <- lm(annual_means ~ I(yrs - mid_point), na.action = na.exclude)

mu <- coef(model)[1] # intercept
m <- coef(model)[2]

# plotting
trend <- data.frame(Year = yrs, Trend = mu + m*(yrs-mid_point))
ggplot(data = clean_d, aes(x = Year, y = annual_means)) +
  geom_point() +
  geom_line(data = trend, aes(x = Year, y = Trend), color = "red") +
  labs(title = "Annual Global Temperature Anomalies and Trend",
       x = "Year", y = "Temperature Anomaly (°C)") +
  theme_minimal()

# the estimated values of mu and m
cat("Estimated mu:", mu, "\n")
cat("Estimated m:", m, "\n")

# the standard deviation of the residuals
sd_residual <- sd(resid(model))
cat("Standard deviation of the residuals:", sd_residual, "\n")


# Problem 3
A <- matrix(c(
  1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0,
  1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0,
  1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0,
  1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0
), nrow = 16, byrow = TRUE)

(Aprime_A <- t(A) %*% A)