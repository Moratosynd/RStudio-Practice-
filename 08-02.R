library(ggplot2)

ggplot(data = mpg, aes(x = displ, y = hwy))

ggplot(data = mpg, aes(x= displ, y = hwy)) + geom_point()

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)


