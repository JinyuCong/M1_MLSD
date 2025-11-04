data(mtcars)
View(mtcars)
head(mtcars)

mtcars$drat <- NULL
mtcars$prix <- sample(10000:40000, nrow(mtcars), replace = TRUE)
mtcars <- mtcars[-1,]

mtcars <- rbind(mtcars, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
rownames(mtcars)[32] <- "new_row"

getwd()
write.csv(mtcars, "C:/Users/congt/Documents/output.csv")
data <- read.csv("C:/Users/congt/Documents/output.csv", header = TRUE, row.names = 1)
data

