# Question 2

# 2 (a)
pnorm(65.5, 63.8, 2.9)

# 2 (b)
qnorm(0.75, 63.8, 2.9) 
qnorm(0.25, 63.8, 2.9)
qnorm(0.75, 63.8, 2.9) - qnorm(0.25, 63.8, 2.9)

# 2 (c)
qnorm(0.025, 63.8, 2.9)
qnorm(0.975, 63.8, 2.9)

#############

# Question 3

# Let's create a standard normal distribution of size 10000
X <- rnorm(10000)
# We are given that Y <- X^2
Y <- X^2
# To calculate P(Y>1)
P <- sum(Y > 1)/10000
cat("The value of P(Y>1) = ", P)
# Therefore the value of P(Y>1) = 0.318

##############

# Question 5

# Reading the dataset
dataset <- read.csv("IUSalaries2023.csv")
# Assigning the salary column alone to a new variable
salaries <- dataset[,11]
# Sorting the dataset so I can find the correct position of Dr. Luen's salary relative to the whole dataset
salaries_sorted <- sort(salaries)
# Finding the position of Dr. Luen's salary in the dataset
rank <- which(salaries_sorted == 81902)
# Percentile Rank
percentile_rank = (rank/3034) * 100
# Therefore, the percentile rank of Dr. Luen's salary is 38%
print(percentile_rank)



