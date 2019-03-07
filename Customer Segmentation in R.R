########################################
### Part 1) Exploratory analysis #######
########################################

library(readr)
dat <- read_csv("C:/Users/Ayota/Desktop/Segmentation Project in Python/segmentation_draft.csv")

head(dat)

# Overall summary and historam of the numerical variables
summary(dat)
hist(dat$totalcomb,100)
hist(dat$newrfpdtq,100)
hist(dat$combnewrenrfpsold,100)

# converting market as factor
dat$market <- as.factor(dat$market)

### Calculating overall mean and mean by Market

overall_mean_tot_comb <- mean(dat$totalcomb)
average_tot_comb_by_market <- aggregate(dat$totalcomb,by=list(dat$market),FUN="mean")
colnames(average_tot_comb_by_market) <- c("Market","Average_tot_comb")
average_tot_comb_by_market


overall_mean_newrfpdtq <- mean(dat$newrfpdtq)
average_newrfpdtq_by_market <- aggregate(dat$newrfpdtq,by=list(dat$market),FUN="mean")
colnames(average_newrfpdtq_by_market) <- c("Market","Average_newrfpdtq")
average_newrfpdtq_by_market

overall_mean_combnewrenrfpsold <- mean(dat$combnewrenrfpsold)
average_combnewrenrfpsold_by_market <- aggregate(dat$combnewrenrfpsold,by=list(dat$market),FUN="mean")
colnames(average_combnewrenrfpsold_by_market) <- c("Market","Average_combnewrenrfpsold")
average_combnewrenrfpsold_by_market


# Merging the mean variables by Market
library(plyr)
merged_data <- join_all(list(average_tot_comb_by_market,average_newrfpdtq_by_market,average_combnewrenrfpsold_by_market), by='Market', type='full')


# To create the plot we are changing the data from wide to long format
library(tidyr)
merged_data_long <- gather(merged_data,Variables,Values,Average_tot_comb:Average_combnewrenrfpsold, factor_key=TRUE)
merged_data_long$Values <- round(merged_data_long$Values,1)

# Ploting
library(ggplot2)
ggplot(data=merged_data_long, aes(x=Market, y=Values, fill=Variables)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  coord_flip() 


################################
### Part 2) Segmentation  ######
################################

## K-means clustering ##


head(dat)

clstr_dat <- dat[,-c(1:2)]
head(clstr_dat)




rng<-2:20 #K from 2 to 20
tries <- 50 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points

for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(clstr_dat,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")


# After 4 value of k the curve changes almost drastically. So, we will use K = 4. Choice of value K
# is a subjective matter. You could use your own intution to pic the value from graph. I am using 4.

set.seed(1000) #Set the seed for reproducibility
k <-kmeans(clstr_dat, centers=4) #Create 5 clusters, Remove columns 1 and 2
k$centers #Display&nbsp;cluster centers
table(k$cluster) #Give a count of data points in each cluster



# Assign class to data frame after clustering
dat$clusters <- k$cluster

write.csv(dat,"C:/Users/Ayota/Desktop/Segmentation Project in Python/k_means_clustering.csv")




## Hierarchical Clustering ##


# Loading data again
dat <- read_csv("C:/Users/Ayota/Desktop/Segmentation Project in Python/segmentation_draft.csv")
clstr_dat <- dat[,-c(1:2)]


dist_mat <- dist(clstr_dat, method = 'euclidean')
clusters <- hclust(dist_mat, method = 'ward.D')
plot(clusters)


clusterCut <- cutree(clusters,4)
table(clusterCut)


# Assign class to data frame after clustering
dat$clusters <- clusterCut


write.csv(dat,"C:/Users/Ayota/Desktop/Segmentation Project in Python/Hierarchical_clustering.csv")
