library(arules)
library(arulesViz)
data(Groceries)
g <- Groceries
inspect(g[1:5])
length(g)
str(g)

data <- g@data
means <- rowMeans(data)
str(means)
?rowMeans

itemFrequencyPlot(Groceries, support=0.1)
rules <- apriori(Groceries, parameter = list(support=.005,confidence=.35))
rules <- apriori(Groceries, parameter = list(support=.01,confidence=.5))
inspect(rules)
par(mfrow=c(1,1))
plot(rules)

goodrules <- rules[quality(rules)$lift > 3.5]
plot(goodrules)
inspect(goodrules)

plot(goodrules, method = "graph", control = list(type = "items"))
