example("mean")
mean.Date(cars)

install.packages('ggplot2')
library(ggplot2)

?mean

class(cars)
is.data.frame(cars)
is.na.data.frame(cars)


df = data.frame(
  Nom = c("Alice", "Bob", "Clara"),
  Age = c(23, 25, 22),
  Sexe = c("F", "M", "F")
)

print(df)


head(cars)
summary(cars)
head(mtcars)
mtcars$hp <- NULL

mtcars[-2,] # 删除第二行，负号代表排除第二行
mtcars[1,] # 显示第一行
mtcars <- mtcars[-2,] # 删除第二行
