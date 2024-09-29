mu0 <- 100
mu1 <- 105
sigma <- 10
n <- 50
alpha <- 0.05

# Calculate standard error
SE <- sigma / sqrt(n)

# Calculate z score for alpha (one-tailed)
z_alpha <- qnorm(1 - alpha)

# Calculate critical value (one-tailed)
critical_value <- mu0 + z_alpha * SE
critical_value
# Calculate the z value under H1
z_value_H1 <- (critical_value - mu1) / SE
z_value_H1
# Calculate beta (Type II error for one-tailed)
beta <- pnorm(z_value_H1)
beta
# Calculate power
power <- 1 - beta

# Display results
print(paste("Critical Value:", round(critical_value, 2)))
print(paste("Beta (Type II Error):", round(beta, 4)))
print(paste("Power of the Test:", round(power, 4)))

