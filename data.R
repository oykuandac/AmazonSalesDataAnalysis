
data = read.csv("AmazonSales.csv")
head(data)




totalSaleOfEachProduct=summary(as.factor(trimws(data[,c("Product")])))
print(totalSaleOfEachProduct)



totalSaleOfEachCountry = summary(as.factor(trimws(data[,c("Country")])))
print(totalSaleOfEachCountry)



mastercardPrice = 0
count = 1
df = data[,c("Payment_Type","Price")]
for (payment in df$Payment_Type) {
  if ( payment == trimws("Mastercard")){
    mastercardPrice = mastercardPrice + strtoi(gsub(",","",trimws(df$Price)[count]))
  }
  count = count + 1
}
print(mastercardPrice)


data = read.csv("AmazonSales.csv")
library(ggplot2)
ggplot(data, aes(x = trimws(data$Product), y = data$Price))+ geom_point()





data2 = read.table("wdbc.data")
data2$id = NULL
data2$diagnosis = NULL

x = data2
y = iris$diagnosis
y_labels = levels(iris$diagnosis)

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
x_norm <- as.data.frame(lapply(x, normalize))
boxplot(x)
boxplot(x_norm)


set.seed(123)
train_ind <- sample(seq_len(nrow(x_norm)), size = floor(0.75 * nrow(x_norm)))
x_train = x_norm[train_ind,]
x_test = x_norm[-train_ind,] # all indices except train indexes
y_train = y[train_ind]
y_test = y[-train_ind]
dim(x_train)
dim(x_test)











