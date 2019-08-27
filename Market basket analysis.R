################################################################
# Examining Electronidex
# Market basket analysis

# Discover Associations Between Products

# Created by Eirik Espe
################################################################

#Calling on packages. Install the packages if you do not have them already.
library(arules)
library(arulesViz)
library(ggplot2)

#Upload the dataset
Tr <- read.transactions("ElectronidexTransactions2017.csv", 
                       format = "basket", 
                       header = FALSE, sep = ",",
                       rm.duplicates = TRUE)


#Summary statistics
inspect(head(Tr))       #View the first six transactions
length(Tr)              #Number of transactions
size(head(Tr))          #Number of items per transactions 
                        #for the first six transactions


#Count of the number of items per transaction
summary(factor(size(Tr)))  

#Most and least frequently purchased items
head(sort(itemFrequency(Tr, type="absolute"), decreasing = TRUE), n = 10)
tail(sort(itemFrequency(Tr, type="absolute"), decreasing = TRUE), n = 10)

# 10 most frequently bought items, including support
freq_itemsets <- eclat(Tr)
inspect(freq_itemsets)



# Finding items that was purchased alone
oneItem <- Tr[which(size(Tr) == 1), ]

# In how many transactions is this the case
length(oneItem)
# 2163 items are purchased alone.
# That's in accordance with the summary statistics.


# Which items are most frequently purchased alone
head(sort(itemFrequency(oneItem, type = "absolute"), decreasing = TRUE), n = 10)




#--- Visualization ----

# Frequency plot
itemFrequencyPlot(Tr, topN = 10, type = "absolute", main = "Item Frequency")

image(sample(Tr, 10))

# Plot of items purchased alone
itemFrequencyPlot(oneItem, topN = 10, type = "absolute", 
                  main = "Item Frequency - one item transactions")


# Set up for creating a plot that will help in deciding support and confidence
# for the rules we are creating.

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- integer(length = 9)
rules_sup5 <- integer(length = 9)
rules_sup1 <- integer(length = 9)
rules_sup0.5 <- integer(length = 9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(Tr, parameter = list(sup = supportLevels[1], 
                                                        conf = confidenceLevels[i], 
                                                        target = "rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup5[i] <- length(apriori(Tr, parameter = list(sup = supportLevels[2], 
                                                       conf = confidenceLevels[i], 
                                                       target = "rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup1[i] <- length(apriori(Tr, parameter=list(sup=supportLevels[3], 
                                                     conf=confidenceLevels[i], 
                                                     target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup0.5[i] <- length(apriori(Tr, parameter=list(sup=supportLevels[4], 
                                                       conf=confidenceLevels[i], 
                                                       target="rules")))
  
}



# Making a plot to see number of rules for different support and confidence levels

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, 
                        rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(num_rules, aes(x = confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y = rules_sup10, colour = "Support level of 10%")) + 
  geom_point(aes(y = rules_sup10, colour = "Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y = rules_sup5, colour = "Support level of 5%")) +
  geom_point(aes(y = rules_sup5, colour = "Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y = rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y = rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y = rules_sup0.5, colour = "Support level of 0.5%")) +
  geom_point(aes(y = rules_sup0.5, colour = "Support level of 0.5%")) +
  
  # Labs and theme
  labs(x = "Confidence levels", y = "Number of rules found", 
       title = "Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())



#---Algorithm----

# Using apriori() function to find association rules
rules <- apriori(Tr, parameter = list(supp = 0.01, conf = 0.3))

# Define 'High-confidence' rules
rules_conf <- sort (rules, by = "confidence", decreasing=TRUE)

# Show the support, confidence and lift for for 6 rules with highest confidence
inspect(head(rules_conf))

#Plot
plot(apriori(Tr, parameter = list(supp = 0.01, conf = 0.3)))


#Plot the top 10 rules measured by lift
top10rules <- head(rules, n = 10, by = "lift")
plot(top10rules, method = "graph", engine = "htmlwidget")



# Looking into some smart home devices, as this is products that Blackwell
# Electronics do not have in their current portfolio

# Get rules that lead to buying 'Google Home'

googhome <- apriori(data = Tr, parameter = list(supp = 0.01, conf = 0.3),
                    appearance = list(default = "lhs",rhs = "Google Home"),
                    control = list(verbose = F))
# No, rules with these support and confidence levels


# Get rules that lead to buying 'Apple TV'

apptv <- apriori(data = Tr, parameter = list(supp = 0.0001, conf = 0.1), 
                 appearance = list(default = "lhs",rhs = "Apple TV"), 
                 control = list(verbose = F))

# First 6 rules
inspect(head(apptv))


# Count of appearances of Apple TV and Google Home in the transactions
crossTable(Tr)['Apple TV', 'Apple TV']
crossTable(Tr)['Google Home', 'Google Home']

# 151 transactions contained Apple TV and 84 transactions contained Google Home




#--- Product types ----

# Looking at the items that Electronidex are selling
colnames(Tr)

# Creating a list of product types for the different items, to be able to
# compare with Blackwell's product types.

#Assign product types to the items
# list of the products type in the right order
ListProducts <- c("External Hardrives",
                  "External Hardrives",
                  "Computer Mice",
                  "External Hardrives",
                  "External Hardrives",
                  "Laptops",
                  "Desktop",
                  "Monitors",
                  "Computer Headphones",
                  "Laptops",
                  "Monitors",
                  "Active Headphones",
                  "Active Headphones",
                  "Laptops",
                  "Laptops",
                  "Keyboard",
                  "Smart Home Devices",
                  "Keyboard",
                  "Keyboard",
                  "Monitors",
                  "Laptops",
                  "Desktop",
                  "Monitors",
                  "Computer Cords",
                  "Keyboard",
                  "Accessories",
                  "Speakers",
                  "Printers",
                  "Printer Ink",
                  "Speakers",
                  "Printer Ink",
                  "Printers",
                  "Accessories",
                  "Speakers",
                  "Desktop",
                  "Desktop",
                  "Desktop",
                  "Mouse and Keyboard Combo",
                  "Laptops",
                  "Monitors",
                  "Keyboard",
                  "Speakers",
                  "Printers",
                  "Printer Ink", "Mouse and Keyboard Combo", "Laptops",
                  "Printer Ink", "Printers", "Computer Cords", "Computer Cords",
                  "Computer Tablets", "Smart Home Devices", "Computer Stands",
                  "Computer Mice", "Computer Mice", "Smart Home Devices",
                  "Computer Stands", "Computer Stands", "Computer Cords",
                  "Computer Cords", "Computer Stands", "Laptops", "Printer Ink",
                  "Desktop", "Monitors", "Laptops", "Keyboard", "Computer Mice",
                  "Printers", "Desktop", "Desktop", "Computer Tablets", "Computer Tablets",
                  "Computer Cords", "Speakers", "Computer Headphones", "Computer Tablets",
                  "Computer Headphones", "Accessories", "Desktop", "Monitors", "Laptops",
                  "Computer Mice", "Computer Headphones", "Mouse and Keyboard Combo",
                  "Keyboard", "Mouse and Keyboard Combo", "Mouse and Keyboard Combo", 
                  "Mouse and Keyboard Combo", "Speakers", "Computer Headphones", "Keyboard", 
                  "Computer Mice", "Speakers", "Computer Mice", "Computer Headphones", 
                  "Accessories", "Mouse and Keyboard Combo", "Mouse and Keyboard Combo",
                  "Active Headphones", "Computer Stands", "Active Headphones", 
                  "Active Headphones", "Computer Headphones", "Computer Headphones", 
                  "Active Headphones", "Computer Mice", "Mouse and Keyboard Combo", 
                  "Keyboard", "Speakers", "Smart Home Devices", "Computer Cords",
                  "Computer Tablets", "Monitors", "Monitors", "External Hardrives", 
                  "Computer Mice", "Smart Home Devices", "Speakers", "Computer Cords", 
                  "Computer Cords", "Monitors", "Computer Mice", "Computer Headphones", 
                  "Computer Headphones")


# Number of transactions and items
Tr

Tr@itemInfo$Producttype <- ListProducts

# Assign product types to the items
Tr <- aggregate(Tr, by= Tr@itemInfo$Producttype)


#Summary statistics
inspect(head(Tr))       #View the first six transactions
length(Tr)              #Number of transactions
size(head(Tr))          #Number of items per transactions 
                        #for the first six transactions
summary(Tr)             #Summary


# Finding product types that was purchased alone
oneItem <- Tr[which(size(Tr) == 1), ]

# In how many transactions is this the case
length(oneItem)

# Which product types are most frequently purchased alone
head(sort(itemFrequency(oneItem, type = "absolute"), decreasing = TRUE), n = 10)




#Plot with product types, instead of items
plot(head(apriori(Tr, parameter = list(supp = 0.01, conf = 0.75)), 
          n = 10, by = "lift"), method = "graph", engine = "htmlwidget")


#Rules for smart home devices
smart_home <- apriori(data = Tr, parameter = list(supp = 0.0006, conf = 0.60),
                      appearance = list(default = "lhs", rhs = "Smart Home Devices"),
                      control = list(verbose = F))

# Inspect the rules
inspect(smart_home)


#The 5 rules with highest lift 
top5rules <- head(smart_home, n = 5, by = "lift")
# Plot
plot(top5rules, method = "paracoord",
     control=list(reorder = TRUE, 
                  main= "Top 5 rules for Smart Home Devices"))
