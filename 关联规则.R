BASKET_all = read.table("BASKETS1n.txt", head=T, sep=',')
BASKET_all_trans = BASKET_all[, 8:18]
for(i in 1:ncol(BASKET_all_trans))
  BASKET_all_trans[,i] = as.numeric(BASKET_all_trans[,i])
BASKET_all_trans = as.matrix(BASKET_all_trans)

library(arules)
trans = as(BASKET_all_trans, "transactions")           
inspect(head(trans))
summary(trans)
image(trans[1:6])
itemFrequencyPlot(trans, support = 0.001)

rules = apriori(trans, parameter = list(support=0.001, confidence = 0.5))
inspect(rules[1:10])
items(rules)
rules.sorted = sort(rules, by='lift')
inspect(rules.sorted[1:5])

library(arulesViz)
plot(rules)
plot(rules, method="grouped")
plot(sort(rules, by='lift')[1:20], method = "graph")
