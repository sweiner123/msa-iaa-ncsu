
## Data Mining Homework 1

#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)

order <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/orderData.csv")

#How many unique Order Numbers?
length(unique(order$orderNo))
#24,687

#How many unique Items?
length(unique(order$item))
#28

#How many unique guests? (combination of order number & seat number)
order %>% group_by(orderNo, seatNo) %>% summarise(count=n())
#76,233

########## summary about items ############
items <- order %>% group_by(item) %>% distinct(item)

# meat summary
meat <- order[seq(1, nrow(order), 3), ] %>% group_by(item) %>% summarise(count=n()) %>% arrange(desc(count))

ggplot(meat, aes(x= reorder(item, -count), y=count)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45,  hjust= 1)) + 
  labs(x= "Meat", y = "Number of Times Ordered", title = "Summary of Main Entrees") + 
  scale_y_continuous(labels = scales::comma) + 
  geom_text(aes(label = scales::comma(count)), vjust = 4, color = "white")

# Wine summary
wine <- order[seq(2, nrow(order), 3), ] %>% group_by(item) %>% summarise(count=n()) %>% arrange(desc(count))

ggplot(wine, aes(x= reorder(item, -count), y=count)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45,  hjust= 1)) + 
  labs(x= "Wine", y = "Number of Wines Ordered", title = "Summary of Main Wines") + 
  scale_y_continuous(labels = scales::comma) + 
  geom_text(aes(label = scales::comma(count)), vjust = 3, color = "white")


########## remove sides ###########
side <- seq(3, nrow(order), by=3)
order2 <- order[-side, ]


########## transactional data ###########
trans_dat <- as(split(order2$item, list(order2$orderNo,order2$seatNo)), "transactions")
inspect(trans_dat)

########## association analysis ###########
trans_dat@itemInfo$labels
itemFrequencyPlot(trans_dat,topN=5,type="absolute")

# general rules
rules <- apriori(trans_dat, parameter = list(supp = 0.001, conf = 0.01, target="rules"))
rules <- sort(rules, by="confidence", decreasing=TRUE)
inspect <- inspect(rules[1:171])
plot(rules)

top10rules = head(rules, n = 10, by = "confidence")
plot(top10rules, method = "graph",  engine = "htmlwidget")

# rules by meat
filet = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Filet Mignon"), minlen=2)
filet <- sort(filet, by="confidence", decreasing=TRUE)
f <- inspect(filet)

bass = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Sea Bass"), minlen=2)
bass <- sort(bass, by="confidence", decreasing=TRUE)
sb <- inspect(bass)

porkt = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Pork Tenderloin"), minlen=2)
porkt <- sort(porkt, by="confidence", decreasing=TRUE)
pt <- inspect(porkt)

porkchop = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Pork Chop"), minlen=2)
porkchop <- sort(porkchop, by="confidence", decreasing=TRUE)
pc <- inspect(porkchop)

salmon = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Salmon"), minlen=2)
salmon <- sort(salmon, by="confidence", decreasing=TRUE)
sal <- inspect(salmon)

duck = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Duck Breast"), minlen=2)
duck <- sort(duck, by="confidence", decreasing=TRUE)
d <- inspect(duck)

swordfish = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Swordfish"), minlen=2)
swordfish <- sort(swordfish, by="confidence", decreasing=TRUE)
sw <- inspect(swordfish)

chicken = apriori(trans_dat, parameter = list(supp=0.001, conf=0.01),appearance = list(default="rhs",lhs="Roast Chicken"), minlen=2)
chicken <- sort(chicken, by="confidence", decreasing=TRUE)
c <- inspect(chicken)

pairing <- rbind(f, sb, pt, pc, sal, d, sw, c)
names(pairing) <- c('meat', '=>', 'wine', 'support', 'confidence', 'coverage', 'lift', 'count')
pairing <- pairing %>% group_by(meat, lift) %>% arrange(meat, desc(lift))
