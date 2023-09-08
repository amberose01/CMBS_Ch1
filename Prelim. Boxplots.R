###Data Boxplots###

#CMBS Final Counts
boxplot(final_cmbsc ~ treatment, data = cmbs_counts, cex.axis=.9, main= 'Final CMBS Counts')
table(cmbs_counts$treatment)

#Aphid Counts
boxplot(aphid_avg ~ treatment, data=aphids, cex.axis=.9)
table(aphids$treatment)

boxplot(aphid_sum~treatment, data=aphids, cex.axis=.9)


#Spad Readings
boxplot(spad_avg ~ treatment, data=spad, cex.axis=.9, main='Spad Readings')

#Health Ratings
boxplot(healthrating ~ treatment, data=health_ratings, cex.axis=.9)

#Sooty Mold Ratings
boxplot(sootymoldrating ~ treatment, data=sm_ratings, cex.axis=.9)

#ggplot intial vs final counts
ggplot(cmbs_counts_tp, aes(x=treatment, y= total_cmbs, fill=timepoint)) + 
  geom_boxplot()

#boxplot spad vs final densities of CMBS
library(ggplot2)

