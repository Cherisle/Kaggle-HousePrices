# Henry Vu
# Michael Tran

# Begin Data Analysis work on House Prices

setwd("C:/Users/Michael/Documents/GitHub/Kaggle-HousePrices")

# GOAL : predict the final price of each home in the test set

# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

head(train)
head(test)

test.SalePrice <- data.frame(SalePrice = rep("None", nrow(test)), test[,])

trainEditable <- cbind(train)

data.combined <- rbind(train,test.SalePrice)

summary(data.combined$SalePrice)

data.combined$SalePrice <- as.factor(data.combined$SalePrice)

extractRange <- function(price)
{
  if (is.na(price)) # IS "NA"
  {
    return("NA")
  }
  else # NOT "NA" , return strings named by Bracket & Number
  {
    if (price >= 0 && price < 50000) { return ("Br01") }
    if (price >= 50000 && price < 75000) { return ("Br02") }
    if (price >= 75000 && price < 100000) { return ("Br03") }
    if (price >= 100000 && price < 125000) { return ("Br04") } 
    if (price >= 125000 && price < 150000) { return ("Br05") }
    if (price >= 150000 && price < 175000) { return ("Br06") } 
    if (price >= 175000 && price < 200000) { return ("Br07") }
    if (price >= 200000 && price < 225000) { return ("Br08") } 
    if (price >= 225000 && price < 250000) { return ("Br09") }
    else { return ("Br10") }
  }
}

SaleRange <- NULL
for (i in 1:nrow(train))
{
  SaleRange <- c(SaleRange, extractRange(trainEditable[i, "SalePrice"]))
}

trainEditable$SaleRange <- as.factor(SaleRange)
#for(i in 1461:2919)
#{
#  data.combined$SaleRange <- 
#}

library(ggplot2)
p <- ggplot(trainEditable[1:1460,], aes(x = SaleRange, fill = MSZoning)) +
  geom_bar() + 
  ggtitle("SaleRange") +
  xlab("Brackets") +
  ylab("Total Count") +
  labs(fill = "MSZoning")

p + scale_fill_manual(values = c("red","green", "gray", "blue","purple"))