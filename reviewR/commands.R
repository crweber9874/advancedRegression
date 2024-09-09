library(dplyr)

# Types of objects
# 1. Vectors
y = c(0, 1, 3, 3)
class(y)
amatrix = matrix(c(1,3,4, "A"), nrow = 2)
bmatrix = matrix(c(1,3,4,2, 2, 2), nrow = 2)

# 2. Matrices
# multiply two matrices
amatrix %*% t(bmatrix)

# 3. Lists
amatrix
bmatrix
y
mylist = list(amatrix = amatrix, bmatrix = bmatrix, y = "this is my data")

# 4. Data Frames
# Create a data frame
dat = data.frame(bmatrix)

# 5. Functions
x <- rnorm(1000, 0, 1)
mean(x)
# Write a mean function

dat = data.frame(matrix(1:10, nrow =5))


my_mean <- function(x){
  c(mean(x))
}






my_mean(dat)

my_mean(x)

