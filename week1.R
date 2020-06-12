# learn dput to read data in R ---
y <- data.frame(a = 1, b = "a")
y <- dput(y)


x <- list(a = list(10, 12, 14), b = c(3.14, 2.81))
x[[c(1,3)]]
x[[1]][[3]]

x[[c(2,1)]]


# subset matrix 
x <- matrix(1:6, 2, 3)
x[1,2] # an element
x[1,2, drop =FALSE] # keep 2 dimention, matrix too

# partial matching ---
x <- list(adbmct = list(10, 12, 14), b = c(3.14, 2.81))
x$a
x[["a", exact = FALSE]]

# removinb NAs
x <- c(1, 2, NA, 4, NA)
bad <- is.na(x)
x[!bad]

good <- complete.cases(x, y) # subset multiple objects or data frame to subset NA out

x * y # element multiplication of two matrix
x %*% y # true matrix multiplication

# hw1
hw1 <- read.csv("hw1_data.csv")
print(hw1[1:2,])
print(hw1[152:153,])
hw1[47,]

sum(is.na(hw1$Ozone))

mean(na.omit(hw1$Ozone))

mean(hw1[hw1$Ozone>31&hw1$Temp>90, "Solar.R"], na.rm = TRUE)

mean(hw1[hw1$Month==6,"Temp"], na.rm = TRUE)

max(hw1[hw1$Month==5,"Ozone"], na.rm = TRUE)







