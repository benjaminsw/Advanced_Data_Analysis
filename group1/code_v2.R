# import library
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr') 
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('reshape')) install.packages('reshape'); library('reshape')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')

# if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('purrr')) install.packages('purrr'); library('purrr')
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
df = dplyr::rename(df, count = value)
# create new var
df$comm_crime <- paste(df$Name_Community, df$variable, sep = "-")
# select columns
df = df[,c("Population", "count", "comm_crime")]
# scale
df[!names(df) %in% c("comm_crime")] <- scale(df[!names(df) %in% c("comm_crime")])

# run Elbow Method
# ref: https://afit-r.github.io/kmeans_clustering
fviz_nbclust(df[!names(df) %in% c("comm_crime")], kmeans, method = "wss")
fviz_nbclust(df[!names(df) %in% c("comm_crime")], kmeans, method = "gap_stat")
fviz_nbclust(df[!names(df) %in% c("comm_crime")], kmeans, method = "silhouette")

# select 2
k2 <- kmeans(df[!names(df) %in% c("comm_crime")], centers = 2, nstart = 25)
fviz_cluster(k2, geom = "point",  data = df[!names(df) %in% c("comm_crime")], 
             palette = "Set2", ggtheme = theme_minimal(), main="k = 2")

# with ratio
df$ratio <- df$count/df$Population
fviz_nbclust(df[!names(df) %in% c("comm_crime")], kmeans, method = "wss")
fviz_nbclust(df[!names(df) %in% c("comm_crime")], kmeans, method = "gap_stat")
fviz_nbclust(df[!names(df) %in% c("comm_crime")], kmeans, method = "silhouette")

# select 3
k3 <- kmeans(df[!names(df) %in% c("comm_crime")], centers = 3, nstart = 25)
fviz_cluster(k3, geom = "point",  data = df[!names(df) %in% c("comm_crime")], 
             palette = "Set2", ggtheme = theme_minimal(), main="k = 3")




