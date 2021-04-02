library(arules)
rules <- apriori(book)
arules::inspect(rules)
rules.sorted <- sort(rules,by="lift")
arules::inspect(rules.sorted)

#rules with rhs containing childbks only
rules <- apriori(book,parameter = list(supp=0.1, conf=0.2)
                 ,appearance = list(rhs=c("ChildBks=[0,1]"))
                 ,control = list(verbose=F))
arules::inspect(rules)

#movies
movies <- my_movies[-1,-c(1:5)]
View(movies)
rules1 <- apriori(movies)
arules::inspect(rules1)
rules1.sorted <- sort(rules1,by="lift")
arules::inspect(rules1.sorted)

#rules with rhs containing column 6 only
rules2 <- apriori(movies,parameter = list(supp=0.1, conf=0.2)
                 ,appearance = list(rhs=c("V6=0","V6=1"))
                 ,control = list(verbose=F))
arules::inspect(rules2)
