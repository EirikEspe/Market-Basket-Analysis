################################################################
# Examining Electronidex
# Market basket analysis

# Treemap

# Created by Eirik Espe
################################################################


#Calling on packages. Install the packages if you do not have them already.
library(arules)
library(arulesViz)
library(ggplot2)
library(treemapify)
library(scales)

#Upload the dataset
Tr <- read.transactions("ElectronidexTransactions2017.csv", 
                        format = "basket", 
                        header = FALSE, sep = ",",
                        rm.duplicates = TRUE)

#Most frequently purchased items
head(sort(itemFrequency(Tr, type="absolute"), decreasing = TRUE), n = 10)


#Create a dataframe to make a treemap
df <- as.data.frame(head(sort(itemFrequency(Tr, type="absolute"), 
                              decreasing = TRUE), n = 10))

#Rename sales volume variable
names(df) <- c("Volume")

#Adding product type
df$ProductType <- c("Desktop", "Laptop", "Desktop", "Active Headphones", 
                   "Laptop", "Desktop", "Desktop", "Laptop", "Monitor", 
                   "Desktop")

# Adding volume as percentage of total transactions
df$Percentage <- df$Volume/nrow(Tr)

# Treemap
#ggplot(df, aes(area = 1, fill = Volume, subgroup = ProductType), 
#       layout = "srow", start = "bottomright") + 
#  geom_treemap() + geom_treemap_subgroup_border() + 
#  geom_treemap_subgroup_text(place = "centre", alpha = 0.5, 
#                             colour = "gray20", fontface = "italic", 
#                             min.size = 0) + 
#  geom_treemap_text(label=rownames(df), colour = "gray91") + 
#  scale_fill_continuous(high = "#132B43", low = "#56B1F7")




# Treemap
ggplot(df, aes(area = 1, fill = Percentage, 
               subgroup = ProductType), start = "topleft") + 
  geom_treemap(start = "topleft") + 
  geom_treemap_subgroup_border(start = "topleft", colour = "cornsilk3") + 
  geom_treemap_subgroup_text(place = "centre", alpha = 0.5, 
                             colour = "cornsilk3", fontface = "italic", 
                             min.size = 0, start = "topleft") + 
  geom_treemap_text(label = rownames(df), colour = "gray91", start = "topleft") + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7", 
                        labels = percent, name = "Share of total\ntransactions")

  
