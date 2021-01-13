# import library
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr') 
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('purrr')) install.packages('purrr'); library('purrr')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('reshape')) install.packages('reshape'); library('reshape')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('cluster')) install.packages('cluster'); library('cluster')
if (!require('fpc')) install.packages('fpc'); library('fpc')
if (!require('flexclust')) install.packages('flexclust'); library('flexclust')
# if (!require('')) install.packages(''); library('')
# if (!require('')) install.packages(''); library('')
# if (!require('')) install.packages(''); library('')
# if (!require('')) install.packages(''); library('')

# set seed
set.seed(125)
# read in table
df = read.csv('crime_data.csv')
# fix columnnames
df = df %>% rename_all(function(x) gsub(" ", "_", x))
df = df %>% rename_all(function(x) gsub("\\.", "_", x))
# select year (2000+1)
#print(colnames(df))
df = subset(df, Year==2002) 
colnames(df)
# select theft, narcotics, motor vehicle theft, battery, assault
df = df[,c(1,2,3,4,6,7,19,20,32)]
# melt data
df = melt(df,id=c("Number_Community","Name_Community","Population","Year"))

# select columns
df = df[,c("Name_Community", "Population", "variable", "value" )]
names(df)[ncol(df)] <- "count"
# one hot encoding
# df <- df %>% mutate(value = 1)  %>% spread(Name_Community, value,  fill = 0 ) 
# scale data
df[!names(df) %in% c("variable")] <- scale(df[!names(df) %in% c("variable")])
# column to rowname
#.rowNamesDF(df, make.names=TRUE) <- df[,"variable"]

# split data
#n <- nrow(df)
#ind1 <- sample(c(1:n), round(n*0.6))
#ind2 <- sample(c(1:n)[-ind1], round(n*0.2))
#ind3 <- setdiff(c(1:n), c(ind1,ind2))
#train_df <- df[ind1, ]
#valid_df<- df[ind2, ]
#test_df<- df[ind3, ]
#

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")















##############################################################################
set.seed(125)
k5 <- kmeans(train_df[!names(train_df) %in% c("variable")], centers=5 , iter.max = 20, nstart = 5)
str(k5)
plot(train_df[,c(1,3)], col = k5$cluster)
points(k5$centers, col = 1:5, pch = 8)
plotcluster(train_df[!names(train_df) %in% c("variable")], k5$cluster)
cl1 = as.kcca(k5, data=train_df[!names(train_df) %in% c("variable")])
# https://stackoverflow.com/questions/20621250/simple-approach-to-assigning-clusters-for-new-data-after-k-means-clustering
#cl1 = kcca(train_df[!names(train_df) %in% c("variable")], k=5, kccaFamily("kmeans"))
str(cl1)
pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=test_df[!names(test_df) %in% c("variable")])

image(cl1)
points(train_df[!names(train_df) %in% c("variable")], col=pred_train, pch=19, cex=0.3)
points(test_df[!names(test_df) %in% c("variable")], col=pred_test, pch=22, bg="orange")
