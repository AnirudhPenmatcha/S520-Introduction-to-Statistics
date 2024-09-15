#MonishaPatro
urn.model <- function(x) {
  # Defining the urn
  urn <- c(1, 1, 1, 1, 2, 5, 5, 10, 10, 10)
  
  # Draw 40 tickets with replacement
  draws <- sample(urn, size = 40, replace = TRUE)
  sample=sample(c(1,2,5,10),size=x)
  x=sum(sample)
  x
  prob=c(4/10,1/10,2/10,3/10)
  # Return the sum of the draws
  return(sum(draws))
}

# Test the function
set.seed(124)  
y <- urn.model()
print(y)


# Define the urn.model function
urn.model <- function(x) {
  # Define the urn
  urn <- c(1, 1, 1, 1, 2, 5, 5, 10, 10, 10)
  
  # Draw 40 tickets with replacement
  draws <- sample(urn, size = 40, replace = TRUE)
  sample=sample(c(1,2,5,10),size=x)
  x=sum(sample)
  x
  prob=c(4/10,1/10,2/10,3/10)
  # Return the sum of the draws
  return(sum(draws))
}

# Generate a sample of 25 observed sums
set.seed(124) 
y <- replicate(25, urn.model())
urn.model(x=25)
hist(y_dash)

# Print the sample
print(y)

urn.model=function(x)
{
  set.seed(124)
  samples=sample(c(1,2,5,10),size=x,replace=TRUE, prob=c(4/10,1/10,2/10,3/10))
  samples
  x=sum(samples)
  x
}
urn.model(x=25)



# Required libraries
library(ggplot2)

# 1. Histogram and Density Plot
hist(y, prob = TRUE, main = "Histogram of y with Density", xlab = "Sum of 40 draws")
lines(density(y), col = "blue")

# 2. Q-Q Plot
qqnorm(y, main = "Q-Q plot for y")
qqline(y)

# 3. Shapiro-Wilk Test
test <- shapiro.test(y)
print(test)


pnorm(2.24,0,1)

sample774 <- scan("https://mtrosset.pages.iu.edu/StatInfeR/Data/sample774.dat")
install.packages(c("rvest", "dplyr"))
library(rvest)
library(dplyr)

x <- c(0.246, 0.327, 0.423, 0.425, 0.434,
       0.530, 0.583, 0.613, 0.641, 1.054,
       1.098, 1.158, 1.163, 1.439, 1.464,
       2.063, 2.105, 2.106, 4.363, 7.517)
ecdf_x <- ecdf(x)
ecdf_x
plot(ecdf_x, main="Empirical CDF of x", xlab="Value of x", ylab="F(x)")

x <- c(0.246, 0.327, 0.423, 0.425, 0.434,
       0.530, 0.583, 0.613, 0.641, 1.054,
       1.098, 1.158, 1.163, 1.439, 1.464,
       2.063, 2.105, 2.106, 4.363, 7.517)
y <- log(x) 
y
qqnorm(y)
qqline(y)






