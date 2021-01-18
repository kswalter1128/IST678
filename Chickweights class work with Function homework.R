chickwts
str(chickwts)

length(chickwts)
?chickwts

as.factor(chickwts$feed)

aggregate(chickwts,list(chickwts$weight))

mean(chickwts$weight[which(chickwts$feed=="linseed")])

#Average Wight of Chickens
mean(chickwts[which(chickwts$feed=="linseed"),1])

chickwts[chickwts$feed=="casein"&chickwts$weight>360,]

#homework write a function to return Average and SD by feed type