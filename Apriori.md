---
title: "Apriori_SCB"
output: pdf_document
---

```{r setup, include=FALSE}
library(dplyr)
library(tidyr)
library(plyr)
library(arules)
library(stringr)
library(gridExtra)
library(arulesViz)
```


Final project for CSPB Data Mining

Correlations between UFO sightings and United States election results


### Data cleaning
```{r data cleaning}
data <- read.csv("cleaned_forApriori.csv")
data$ID <- seq.int(nrow(data))

extras <-  c("\\[", "\\]", "\\'", " ")

data <- data %>% 
  mutate(comments_array = str_remove_all(comments_array, regex(str_c(extras, collapse = '|')))) 

clean <- c("san", "francisco", "new", "york", "vegas",
             "las", "jersey", "pd", "diego",
             "tinley", "park", "puget", "jose")

  #remove string in clean but only if they start "//b" with the string
  #split into rows with strsplit and then unnest
data <- data %>% select(comments_array, ID, last_election_year, party) %>% dplyr::rename("items" = "comments_array") %>% 
  mutate(items = str_remove_all(items, regex(str_c("\\b", clean, "\\b", collapse = '|'))))  %>% 
  mutate(items = strsplit(as.character(items), ",")) %>%
  unnest(items)  



data$items <- gsub("lights", "light", data$items)

# create transaction data
data <- as.data.frame(data)

```


```{r pre 1996}

# split all transactions
transTot <- as(split(data[,"items"],data[,"ID"],), "transactions")

#################################
## simple plot of top 5 frequent items

# get all rules
basket_rulesTot <- apriori(transTot,parameter = list(sup = 0.001, conf = 0.8, minlen = 2))
#in descending order

##remove rules that are subsets of other rules
subset_rules1Tot <- which(colSums(is.subset(basket_rulesTot, basket_rulesTot)) > 1)
rules_conf1Tot <- basket_rulesTot[-subset_rules1Tot]
rules_conf2Tot <- sort (rules_conf1Tot, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf2Tot[1:20])
rules_dataframeTot <- as(rules_conf2Tot[1:20], 'data.frame')


##output data as pdf
pdf(file="Apriori_Results_TotalData.pdf", width = 10, height = 10)  

grid.table(rules_dataframeTot)

#subset(rules, subset = lift > 2)
##ploting the top 20 associaton rules
itemFrequencyPlot(transTot, topN=20, type="absolute", main="Item Frequency") # plot frequent items

plot(basket_rulesTot)
subRules2Tot <- head(rules_conf2Tot, n=20, by="confidence")
plot(subRules2Tot, method = "graph")
plot(subRules2Tot, method = "grouped")
dev.off()
#dev.set(dev.next())
```

```{r pre 1996}

# data subset on year pre 1996
dataL <- data[which(data$last_election_year < "1996"),]
dataL <- dataL %>% select(items, ID)
transL <- as(split(dataL[,"items"],dataL[,"ID"],), "transactions")

#################################

# get all rules
basket_rulesL <- apriori(transL,parameter = list(sup = 0.001, conf = 0.8, minlen = 2))
#in descending order

##remove rules that are subsets of other rules
subset_rules1L <- which(colSums(is.subset(basket_rulesL, basket_rulesL)) > 1)
rules_conf1L <- basket_rulesL[-subset_rules1L]
rules_conf2L <- sort (rules_conf1L, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf2L[1:20])

#create dataframe
rules_dataframeL <- as(rules_conf2L[1:20], 'data.frame')

pdf(file="Apriori_Results_pre_1996.pdf", height = 10, width = 10)  
##ploting the associaton rules
grid.table(rules_dataframeL)
## simple plot of top 20 frequent items
itemFrequencyPlot(transL, topN=20, type="absolute", main="Item Frequency") # plot frequent items

plot(basket_rulesL)
subRules2L <- head(rules_conf2L, n=20, by="confidence")
plot(subRules2L, method = "graph")
plot(subRules2L, method = "grouped")
dev.off()
```


```{r}

# data subset on year post 1996
dataG <- data[which(data$last_election_year >= "1996"),]

transG <- as(split(dataG[,"items"],dataG[,"ID"],), "transactions")


#################################
# get all rules
basket_rulesG <- apriori(transG,parameter = list(sup = 0.001, conf = 0.8, minlen = 2))
#in descending order

##remove rules that are subsets of other rules
subset_rules1G <- which(colSums(is.subset(basket_rulesG, basket_rulesG)) > 1)
rules_conf1G <- basket_rulesG[-subset_rules1G]
rules_conf2G <- sort (rules_conf1G, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf2G[1:10])

rules_dataframeG <- as(rules_conf2G[1:20], 'data.frame')

pdf(file="Apriori_Results_post_1996.pdf", height = 10, width = 10)  
##ploting the associaton rules
grid.table(rules_dataframeG)
## simple plot of top 5 frequent items
itemFrequencyPlot(transG, topN=20, type="absolute", main="Item Frequency") # plot frequent items
##plots for apriori rules
plot(basket_rulesG)
subRules2G <- head(rules_conf2G, n=20, by="confidence")
plot(subRules2G, method = "grouped")
plot(subRules2G, method = "graph")
dev.off()
```

```{r}

# data subset for democratic party
dataD <- data[which(data$party == "democrat"),]

transD <- as(split(dataD[,"items"],dataD[,"ID"],), "transactions")


#################################

# get all rules
basket_rulesD <- apriori(transD,parameter = list(sup = 0.001, conf = 0.8, minlen = 2))
#in descending order

##remove rules that are subsets of other rules
subset_rules1D <- which(colSums(is.subset(basket_rulesD, basket_rulesD)) > 1)
rules_conf1D <- basket_rulesD[-subset_rules1D]
rules_conf2D <- sort (rules_conf1D, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf2D[1:20])

rules_dataframeD <- as(rules_conf2D[1:20], 'data.frame')

pdf(file="Apriori_Results_Dems.pdf", height = 10, width = 10)  
##ploting the associaton rules
grid.table(rules_dataframeD)
## simple plot of top 5 frequent items
itemFrequencyPlot(transD, topN=20, type="absolute", main="Item Frequency") # plot frequent items
plot(basket_rulesD)
subRules2D <- head(rules_conf2D, n=20, by="confidence")
plot(subRules2D, method = "grouped")
plot(subRules2D, method = "graph")
dev.off()
```

```{r}

# data subset for republican party
dataR <- data[which(data$party == "republican"),]

transR <- as(split(dataR[,"items"],dataR[,"ID"],), "transactions")


#################################

# get all rules
basket_rulesR <- apriori(transR,parameter = list(sup = 0.001, conf = 0.8, minlen = 2))
#in descending order

##remove rules that are subsets of other rules
subset_rules1R <- which(colSums(is.subset(basket_rulesR, basket_rulesR)) > 1)
rules_conf1R <- basket_rulesR[-subset_rules1R]
rules_conf2R <- sort (rules_conf1R, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf2R[1:10])

rules_dataframeR <- as(rules_conf2R[1:20], 'data.frame')

pdf(file="Apriori_Results_Repubs.pdf", height = 10, width = 10)  

##ploting the associaton rules
grid.table(rules_dataframeR)
## simple plot of top 5 frequent items
itemFrequencyPlot(transR, topN=20, type="absolute", main="Item Frequency") # plot frequent items
plot(basket_rulesR)
subRules2R <- head(rules_conf2R, n=20, by="confidence")
plot(subRules2R, method = "grouped")
plot(subRules2R, method = "graph")
dev.off()
```