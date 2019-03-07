library(readr)
dat <- read_csv("C:/Users/Ayota/Desktop/Segmentation Project in Python/segmentation_draft.csv")

head(dat)


summary(dat)
hist(dat$totalcomb,100)
hist(dat$newrfpdtq,100)
hist(dat$combnewrenrfpsold,100)


dat$market <- as.factor(dat$market)



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



library(plyr)
merged_data <- join_all(list(average_tot_comb_by_market,average_newrfpdtq_by_market,average_combnewrenrfpsold_by_market), by='Market', type='full')


library(tidyr)
merged_data_long <- gather(merged_data,Variables,Values,Average_tot_comb:Average_combnewrenrfpsold, factor_key=TRUE)
merged_data_long$Values <- round(merged_data_long$Values,1)


library(ggplot2)


ggplot(data=merged_data_long, aes(x=Market, y=Values, fill=Variables)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  coord_flip() 

