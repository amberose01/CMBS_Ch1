###Data Boxplots###

#CMBS Final Counts
boxplot(final_cmbsd ~ treatment, data = cmbs_counts, cex.axis=.9, main= 'Final CMBS Counts')
table(cmbs_counts$treatment)

boxplot(final_cmbsd/initial_cmbsd~treatment, data=all_data, cex.axis=.9)

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
ggplot(cmbs_counts_tp, aes(x=treatment, y= cmbsd, fill=timepoint)) + 
  geom_boxplot()

#parameters vs final cmbs densities
plot(spad_avg~final_cmbsd, data=all_data) #spad
ggplot(all_data, aes(x=final_cmbsd, y=spad_avg)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)


plot(healthrating~final_cmbsd, data=all_data)
plot(spad_avg~healthrating, data=all_data)
boxplot(spad_avg~healthrating, data=all_data)

library(ggplot2)

ggplot(all_data, aes(x=healthrating, y=spad_avg)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

plot(final_cmbsd~initial_cmbsd, data=all_data)
fit2<-lm(log(final_cmbsd +1)~log(initial_cmbsd+1), data=all_data)
one.way<-aov(fit2)
summary(one.way)
one.way

d_LM<-lm(final_cmbsd~initial_cmbsd, data=all_data)
plot(d_LM)

## Tukeys Post Hoc test- initial vs final densities

fit1<-lm(final_cmbsd ~ initial_cmbsd + treatment, data=cmbs_counts)
library(car)
Anova(fit1, type="II")
plot(fit1)  
fit1_log<-lm(log(final_cmbsd+1)~log(initial_cmbsd+1)+treatment, data=cmbs_counts)
Anova(fit1, type='II')
plot(fit1_log)

library(emmeans)
finalcmbscounts<-cmbs_counts$final_cmbsd
emmeans(fit1_log,pairwise~treatment)
plot(emmeans(fit1,pairwise~treatment))

