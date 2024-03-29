# ESS212 hw2

# Problem 5
## a)
fn_Sn <- function(N) {
  S <- numeric(N + 1)  # initialize S with enough space
  
  # initial conditions
  S[1] <- 0  # S[0] = 0
  S[2] <- 1  # S[1] = 1
  
  if (N == 0) {
    return(S[1])  # return S[0] for N = 0
  }
  
  for (n in 1:N) {
    S[n + 2] <- 2 * S[n + 1] + 2 * S[n]  
  }
  
  return(S[N + 1])  # return the N-th term
}

## c)
compute_Sn <- function(n) {
  A <- sqrt(3) / 6
  B <- - sqrt(3) / 6
  a <- 1 + sqrt(3)
  b <- 1 - sqrt(3)
  Sn <- A * a^n + B * b^n  
  return(Sn)
}

test_fn_Sn <- function() {
  passed <- TRUE
  tol <- 1e-6  
  
  for (n in 0:20) {
    expected <- compute_Sn(n)
    actual <- fn_Sn(n)
    
    if (abs(expected - actual) > tol) {
      cat(sprintf("Test failed for n = %d: expected %f, got %f\n", n, expected, actual))
      passed <- FALSE
    } else {
      cat(sprintf("Test passed for n = %d: expected %f, got %f\n", n, expected, actual))
    }
  }
  
  if (passed) {
    cat("All tests passed.\n")
  } else {
    cat("Some tests failed.\n")
  }
}

test_fn_Sn()

# Problem 6
## b)
sqrt_est <- function(c, tol=1e-6) {
  if (c < 0) {
    stop("c must be non-negative")
  }
  
  # initial guess for the square root of c
  x <- ifelse(c > 0, c / 2, 0)
  
  while (abs(x^2 - c) >= tol) {
    x <- 0.5 * (x + c / x)
  }
  
  return(x)
}

# e.g.
(sqrt_est(1225))
(sqrt_est(pi))
(sqrt_est(0.1004))

## c)
library(testthat)

test_that("sqrt_est correctly estimates the square root", {
  expect_equal(sqrt_est(1225), sqrt(1225), tol = 1e-6)
  expect_equal(sqrt_est(pi), sqrt(pi), tol = 1e-6)
  expect_equal(sqrt_est(0.1004), sqrt(0.1004), tol = 1e-6)
})

# Problem 7
library(ggplot2)

mandelbrot <- function(c, max_iter) {
  z <- 0
  for(i in 1:max_iter) {
    z <- z^2 + c
    if(Mod(z) > 2) {
      return(FALSE)
    }
  }
  return(TRUE)
}

x_min <- -2
x_max <- 0.5
y_min <- -1.5
y_max <- 1.5
x <- seq(x_min, x_max, by = 5e-3)
y <- seq(y_min, y_max, by = 5e-3)
grid <- expand.grid(x = x, y = y)

# check each point in the grid
max_iter <- 500
points <- vector("list", length=nrow(grid))
for(i in 1:nrow(grid)) {
  c <- complex(real = grid$x[i], imaginary = grid$y[i])
  points[[i]] <- mandelbrot(c, max_iter)
}

# convert the results to a dataframe
grid$set <- unlist(points)

ggplot(grid, aes(x, y)) + 
  geom_tile(aes(fill = set)) + 
  scale_fill_manual(values = c("TRUE" = "snow", "FALSE" = "darkslateblue")) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, by = 0.5)) +  theme_minimal() + 
  labs(title = "Mandelbrot Set", x = "Real(c)", y = "Imag(c)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Problem 8
## a)
g <- 9.81 
z0 <- 100 
delta_t <- 1e-6 
v <- 0 

# Will-E scheme, Euler method 
z <- z0 - 1e-6 
t <- 0 

while (z > 0) {
  v <- sqrt(2 * g * (z0 - z)) 
  delta_z <- v * delta_t
  if (z - delta_z < 0) {
    z <- 0
  } else {
    z <- z - delta_z # Loop until z <= 0
  }
  t <- t + delta_t 
}

real_t <- sqrt(2 * z0 / g)

# Results
cat("Time when z reaches 0 calculated by the Will-E scheme:", t, "seconds\n")
cat("Time when z reaches 0 by the physical fact:", real_t, "seconds\n")

## b)
g <- 9.81 
z0 <- 100 
m <- 1 
delta_t <- 1e-3

# initial conditions
z <- z0
v <- 0 
t <- 0

ts <- c()
ki_es <- c()
po_es <- c()
tt_es <- c()
posi <- c() 

# Euler forward method
while (z > 0) {
  v <- v + g * delta_t
  z <- max(0, z - v * delta_t) # ensure z >= 0
  t <- t + delta_t 
  
  ki_e = 0.5 * m * v^2
  po_e = m * g * z
  tt_e = ki_e + po_e
  
  ts <- append(ts, t)
  ki_es <- append(ki_es, ki_e)
  po_es <- append(po_es, po_e)
  tt_es <- append(tt_es, tt_e)
  posi <- append(posi, z) 
}

plot(ts, ki_es, type = 'l', col = 'darkblue', xlim = c(0, 5),
     ylim = c(0, max(tt_es)), xlab = "Time (s)", ylab = "Energy (J)", 
     main = "Energies vs Time")
lines(ts, po_es, col = 'darkred')
lines(ts, tt_es, col = 'darkgreen')
legend("left", inset=.05, legend = c("kinetic energy", "potential energy", "total energy"), 
       col = c("darkblue", "darkred", "darkgreen"), lty = 1, cex = 0.8)

# z = 0
bottom_t <- ts[which.min(posi > 0)]
abline(v = bottom_t, col = "black", lty = 2) 
text(bottom_t, max(tt_es) * 0.5, labels = paste("z=0 at", round(bottom_t, 6), "s"), 
     pos = 4, cex = 0.6)

print(paste("Time when z=0 calculated by the Euler method:", round(bottom_t, 6), "seconds"))
cat("Time when z reaches 0 by the physical fact:", real_t, "seconds\n")

## c)
g <- 9.81 
z0 <- 100 
m <- 1 
delta_t <- 1e-3 
tt_time <- sqrt(2 * z0 / g) 
N <- as.integer(tt_time / delta_t)

v <- rep(0, N) 
z <- rep(0, N) 
times <- seq(0, tt_time, length.out = N)

# initial conditions
v[1] <- 0 
z[1] <- z0 

ki_es <- rep(0, N)
po_es <- rep(0, N)
tt_es <- rep(0, N)

# Leap-frog scheme for i=2 
v[2] <- v[1] + g * delta_t
z[2] <- z[1] - 0.5 * g * delta_t^2 

# Leap-frog iteration for i = 3 to N
for (i in 3:N) {
  v[i] <- v[i-2] + 2 * g * delta_t 
  z[i] <- z[i-2] - 2 * v[i-1] * delta_t 
  
  ki_es[i] <- 0.5 * m * v[i]^2
  po_es[i] <- m * g * (z[i] - 0) 
  tt_es[i] <- ki_es[i] + po_es[i]
}

ki_es[1] <- 0.5 * m * v[1]^2
po_es[1] <- m * g * (z[1] - 0)
tt_es[1] <- ki_es[1] + po_es[1]

plot(times[-2], ki_es[-2], type = 'l', col = 'darkblue', xlim = c(0, 5), 
     ylim = c(min(po_es[-2]), max(tt_es[-2])), xlab = "Time (s)", 
     ylab = "Energy (J)", main = "Energies vs Time") 
lines(times[-2], po_es[-2], col = 'darkred')
lines(times[-2], tt_es[-2], col = 'darkgreen')
legend("left", legend = c("kinetic energy", "potential energy", "total energy"), 
       col = c("darkblue", "darkred", "darkgreen"), lty = 1, cex = 0.8) 

t_bottom <- times[which.min(posi > 0)]
abline(v = t_bottom, col = "black", lty = 2) 
text(t_bottom, max(tt_es) * 0.5, labels = paste("z=0 at", round(t_bottom, 6), "s"), 
     pos = 4, cex = 0.6)

print(paste("Time when z=0 calculated by the leap-frog scheme:", round(t_bottom, 6), "seconds"))
cat("Time when z reaches 0 by the physical fact:", tt_time, "seconds\n")