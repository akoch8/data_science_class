###
### A brief introduction to
### DATA SCIENCE
###
### Basic introduction to R.
###
### Alexander Koch
### 2019
###
#
# Welcome to R!
#
# This is an R script that will introduce you to some of the basics of
# the R programming language.
# Each line of the text you are reading right now is preceded by a "#",
# which makes them what we call "comments". These are lines of code that
# are not executed when you run your program. You can (and should!) use
# them to explain your code.
#
# Let's start writing some code that does something!
# If you want, you can use R as a simple calculator:
1 + 2
4 - 9
3 * 4
16 / 3
2 ^ 2
2 ** 3
16 ^ 0.5
sqrt(16)
?sqrt
2 / 0
16 %% 3
15 %% 3

# We can save the result of a sum by assigning it to a variable.
# (The variable does not actually hold any data! It merely points to where
# the data is stored in your computer's memory.)
x = 1 + 2
x

# You can change the value of x by assigning a different value to it:
x = 5
x

# There are many different types of variables in R, depending on the type
# of data they contain as well as the structure of their value.
# The main data types are:
# - numbers
x = 123.45
x

# - strings
x = 'This is a string'
x

# - booleans
x = FALSE
x
2 > 6
'a' == 'a'

# - factors
x = factor(c('a', 'a', 'b', 'c', 'c', 'c', 'c'))
x

# The main data structures are:
# - vectors:
x = c(1, 2, 3, 4, 5)
x
x[6]
x = rnorm(10)
?rnorm
x
length(x)
mean(x)
median(x)
max(x)
min(x)
summary(x)
x[2:7]
x[11]
x + 2

# - data frames:
x = data.frame(a = c(1, 2, 3), b = c('a', 'b', 'c'), stringsAsFactors=F)
dim(x)
nrow(x)
ncol(x)
x
x[1,]
x[,1]
x[1,1]
str(x)
colnames(x)
colnames(x) = c('column_1', 'column_2')
x
x$column_1

# - matrices:
x = matrix(data=seq(12), nrow=4, byrow=T)
x
x = x * 2.2
x

# - lists:
x = list(a = c(1, 2, 3), b = c('a', 'b', 'c'))
x
length(x)
x[['a']]
x[[2]]
x[[3]]

# We have already used several built-in functions, like mean() and length().
# It is also possible to write your own functions!
triple = function(x) {
  return(x * 3)
}
triple(2)

# An important programming concept is the conditional statement. These allow
# us to perform different actions depending on whether a certain condition
# is true or false.
x = 1
if (x > 3) {
  message('x is greater than 3')
}

x = 3
if (x < 3) {
  message('x is smaller than 3')
} else {
  message('x is greater than 3')
}

if (x < 3) {
  message('x is smaller than 3')
} else if (x == 3) {
  message('x is equal to 3')
} else {
  message('x is greater than 3')
}

# Another important programming concept is "iteration". This is where
# you repeat a block of statements a number of times. There are several
# different ways to achieve this. The two most common ones are:
# - the for loop
for (i in 1:5) {
  message(paste0('i = ', i))
}

x = data.frame(a = c(1, 2, 3), b = c('a', 'b', 'c'), stringsAsFactors=F)
for (i in 1:nrow(x)) {
  message(paste0('row ', i, ': ', paste(x[i,], collapse=', ')))
}

# - the while loop
i = 0
while (i < 5) {
  i = i + 1
  message(paste0('i = ', i))
}

# R has a lot (A LOT!) of options for visualising data, from simple bar
# charts to complex, completely custom figures.
# Start by loading the built-in iris dataset.
?iris
data(iris)
x = iris$Petal.Length
y = iris$Petal.Width
plot(x, y)
?plot
plot(x, y, bty='n', pch=20, col='orange')
abline(lm(y ~ x), col='cornflowerblue', lwd=2, lty=2)
lines(lowess(x, y, f=0.5), col='cornflowerblue', lwd=2, lty=3)

table(iris$Species)
pointColors = as.character(iris$Species)
pointColors[pointColors == 'setosa'] = '#a8e6cf'
pointColors[pointColors == 'versicolor'] = '#fdffab'
pointColors[pointColors == 'virginica'] = '#ffd3b6'
plot(x, y, bty='n', pch=20, col=pointColors, xlab='Petal length', ylab='Petal width')
legend('topleft', levels(iris$Species), col=c('#a8e6cf', '#fdffab', '#ffd3b6'), pch=20, bty='n')

boxplot(iris$Sepal.Length ~ iris$Species, ylab='Sepal length', frame.plot=F, outline=F)

pairs(~., data=iris[,-ncol(iris)])
pairs(~., data=iris[,-ncol(iris)], pch=20, col=pointColors)
