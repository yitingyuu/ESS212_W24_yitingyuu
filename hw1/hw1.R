# problem 3

# S_1(n)
## iterative algorithm
S1_iter <- function(n) {
  S <- 0
  for (i in  1:n) {
    S <- S + i
  }
  return(S) 
}

## recursive algorithm
S1_rec <- function(n) {
  if (n > 1) {
    S = n + S1_rec(n-1)
  }
  else{
    S = 1
  }
  return(S) 
}

## verify
n <- c(1, 10, 100)
(r_s1 <- data.frame(
  n = n,
  iter_result = sapply(n, S1_iter),
  rec_result = sapply(n, S1_rec),
  real_result = n*(n+1)/2
))

# O_1(n)
## iterative algorithm
O1_iter <- function(n) {
  S <- 0
  for (i in 1:n) {
    S <- S + 2*i-1
  }
  return(S)
}

## recursive algorithm
O1_rec <- function(n) {
  if (n > 1) {
    S = 2*n-1 + O1_rec(n-1)
  } else {
    S = 1
  }
}

## verify
n <- c(1, 10, 100)
(r_o1 <- data.frame(
  n = n,
  iter_result = sapply(n, O1_iter),
  rec_result = sapply(n, O1_rec),
  real_result = n^2
))

# S_2(n)
## iterative algorithm
S2_iter <- function(n) {
  S <- 0
  for (i in 1:n) {
    S <- S + i^2
  }
  return(S)
}
## recursive algorithm
S2_rec <- function(n) {
  if (n > 1) {
    S = n^2 + S2_rec(n-1)
  }
  else{
    S = 1
  }
  return(S) 
}

## verify
n <- c(1, 10, 100)
(r_s2 <- data.frame(
  n = n,
  iter_result = sapply(n, S2_iter),
  rec_result = sapply(n, S2_rec),
  real_result = (n*(n+1)*(2*n+1))/6
))

# problem 4

bino_coef <- function(n,k){
  if (k < n && k > 0) {
    C = choose(n-1, k-1) + choose(n-1, k)
  }
  else{
    if (k > n){
      C = 0
    }
    if (k == 0 || k == n){
      C = 1
    }
  }
  return(C)
}

# verify 
n <- 180
k <- 23
(r_bino_coef <- data.frame(
  n = n, k = k,
  r1 = bino_coef(n, k),
  r2 = choose(n, k)
))

Pt_nrow <- function(n) {
  row <- numeric(2*n-1)
  row[seq(1, 2*n-1, by=2)] <- sapply(0:(n-1), function(k) choose(n-1, k))
  
  # Print the row
  cat(row, "\n")
}

# verify:
Pt_nrow(9)
Pt_nrow(10)

# problem 5

S_n <- function(a, n, r) {
  if (n > 1) {
    Sn = a*(r^(n-1)+S_n(a,n-1,r))
  } 
  else {
    Sn = a
  }
  return(Sn)
}

library(testthat)
a <- 1
n <- 10
# unit tests
test_that("Test S_n function with various r values", {
  # r < 0
  expect_equal(S_n(a, n, -1), a*(1-(-1)^n) / (1-(-1)))
  # r = 0
  expect_equal(S_n(a, n, 0), a)
  # 0 < r < 1
  expect_equal(S_n(a, n, 0.5), a*(1-(0.5)^n) / (1-0.5))
  # r = 1
  expect_equal(S_n(a, n, 1), a*n)
  # r > 1
  expect_equal(S_n(a, n, 1.5), a*(1-1.5^n) / (1-1.5))
})
