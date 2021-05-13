pacman::p_load(pacman, tidyverse,rmarkdown,ggplot2,dplyr,mplot, gvlma,caret,rio,olsrr,car,gridExtra,stringr,gridExtra,hexbin,lmtest,ggpubr)

df <- rio::import('/Users/garethbayvel/Desktop/rStudio/pulitzer.csv',setclass = 'tibble')

# Question One

# percentage change in circulation between 2004 and 2013 as an integer
df$change_0413 <- str_split(df$change_0413, '%',simplify = TRUE)[,1] %>%
  as.integer()

# Append a new variable containg the average of circ_2004 and circ_2013.
df<- df%>%
  rowwise()%>%
  mutate('Circ2004_Circ2013' = mean(c(circ_2004,circ_2013)))
  
# Question Two

# Describe the distribution of the variable representing average circulation
df%>%
  ggplot(aes(x = Circ2004_Circ2013, y = ..density..)) +
  geom_histogram(col = 'black', bins = 25, fill= 'orange') +
  labs(title = "Histogram (Graph 1a)", subtitle = "Average Circulation from 2004 to 2013") +
  xlab("Average Circulation") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(Circ2004_Circ2013, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(Circ2004_Circ2013, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9")) 

# Boxplot to describe Spread

df %>%
  ggplot(aes(x = Circ2004_Circ2013)) +
  geom_boxplot(fill = 'orange') +
  labs(title = "Boxplot (Graph 1b)", subtitle = "Spread of Average Circulation") +
  xlab('Average Circulation')+
  theme(title = element_text(face = 'bold'))

# Spread Using IQR
summary(df$Circ2004_Circ2013)

# IQR
IQR(df$Circ2004_Circ2013)


#  Distribution of change_0413
df%>%
  ggplot(aes(x = change_0413, y = ..density..)) +
  geom_histogram(col = 'black', bins = 25, fill= 'orange') +
  labs(title = "Histogram (Graph 2a)", subtitle = "Percentage Change from 2004 to 2013") +
  xlab("Average Circulation") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(change_0413, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(change_0413, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9")) 

# Boxplot to describe Spread
df %>%
  ggplot(aes(x = change_0413)) +
  geom_boxplot(fill = 'orange') +
  labs(title = "Boxplot (Graph 2b)", subtitle = "Spread of Average Circulation") +
  xlab('Average Circulation')+
  theme(title = element_text(face = 'bold'))

# Spread Using IQR
summary(df$change_0413)

# IQR
IQR(df$change_0413)

df%>%
  filter(change_0413 > 12)%>%
  arrange(change_0413)

# Log Transformation

require(gridExtra)
plot1 <- df%>%
  ggplot(aes(x = change_0413, y = ..density..)) +
  geom_histogram(col = 'black', bins = 20, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Percentage Change from 2004 to 2013") +
  xlab("Average Circulation") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(change_0413, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(change_0413, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9")) 
plot2 <- df%>%
  ggplot(aes(x = log(change_0413), y = ..density..)) +
  geom_histogram(col = 'black', bins = 10, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Log Percentage Change from 2004 to 2013") +
  xlab("Log Average Circulation") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(log(change_0413), na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(log(change_0413), na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9")) 
plot14 <- df%>%
  ggplot(aes(x = mutated , y = ..density..)) +
  geom_histogram(col = 'black', bins = 35, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Standardized Log Percentage Change from 2004 to 2013") +
  xlab("Standardized Log Average Circulation") +
  theme(title = element_text(face = 'bold')) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9")) 
grid.arrange(plot1,                             
             arrangeGrob(plot2, plot14, ncol = 2), 
             nrow = 2) 

# Log Transformation 
require(gridExtra)
plot3 <- df%>%
  ggplot(aes(x = Circ2004_Circ2013, y = ..density..)) +
  geom_histogram(col = 'black', bins = 10, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Percentage Change from 2004 to 2013") +
  xlab("Average Circulation") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(Circ2004_Circ2013, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(Circ2004_Circ2013, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9")) 
plot4 <- df%>%
  ggplot(aes(x = log(Circ2004_Circ2013), y = ..density..)) +
  geom_histogram(col = 'black', bins = 10, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Log Percentage Change from 2004 to 2013") +
  xlab("Log Average Circulation") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(log(Circ2004_Circ2013), na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(log(Circ2004_Circ2013), na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9")) 
grid.arrange(plot3, plot4, ncol=2)


# Question Three

df_lm <- lm(Circ2004_Circ2013~prizes_9014,data = df)
summary(df_lm)

# Log average

df_log_lm <- lm(log(Circ2004_Circ2013)~prizes_9014,data = df)
summary(df_log_lm)


require(gridExtra)
plot5 <- df%>%
  ggplot(aes(x = Circ2004_Circ2013, y = prizes_9014)) +
  geom_point(color = 'purple') +
  geom_smooth(se = F, color = 'orange') +
  labs(title = "Scatterplot", subtitle = "Relationship between circulation and Pulitzer Prizes") +
  xlab("Average Circulation") +
  ylab("No. Prizes")+
  theme(title = element_text(face = 'bold')) 

plot6 <- df%>%
  ggplot(aes(x = log(Circ2004_Circ2013), y = prizes_9014)) +
  geom_point(color = 'blue') +
  geom_smooth(se = F,color = 'orange') +
  labs(title = "Scatterplot", subtitle = "Relationship between Log circulation and Pulitzer Prizes") +
  xlab("Log Average Circulation") +
  ylab("No. Prizes")+
  theme(title = element_text(face = 'bold')) 
grid.arrange(plot5, plot6, ncol=2)





log_average_df <- df%>%
  mutate('log_average' = log(Circ2004_Circ2013))

cor(df$Circ2004_Circ2013, df$prizes_9014,use="complete.obs")
cor(log_average_df$log_average, log_average_df$prizes_9014,use="complete.obs")



df%>%
  ggplot(aes(Circ2004_Circ2013, prizes_9014)) +
  geom_hex(bins = 50)

log_average_df%>%
  ggplot(aes(log_average, prizes_9014)) +
  geom_hex(bins = 50)

# Model predicting change_0413 using prizes_9014

df_change_lm <- lm(change_0413~prizes_9014,data = df)
summary(df_change_lm)

# Log average

df_log_changelm <- lm(log(change_0413)~prizes_9014,data = df)
summary(df_log_changelm)

cor(df$change_0413, df$prizes_9014,use="complete.obs")

# Plotting Relationship
require(gridExtra)
plot7 <- df%>%
  ggplot(aes(x = change_0413, y = prizes_9014)) +
  geom_point(color = 'purple') +
  geom_smooth(se = F, color = 'orange') +
  labs(title = "Scatterplot", subtitle = "Relationship between circulation and Pulitzer Prizes") +
  xlab("Change") +
  ylab("No. Prizes")+
  theme(title = element_text(face = 'bold')) 

plot8 <- df%>%
  ggplot(aes(x = log(change_0413), y = prizes_9014)) +
  geom_point(color = 'blue') +
  geom_smooth(se = F,color = 'orange') +
  labs(title = "Scatterplot", subtitle = "Relationship between Log circulation and Pulitzer Prizes") +
  xlab("Log Change") +
  ylab("No. Prizes")+
  theme(title = element_text(face = 'bold')) 
grid.arrange(plot7, plot8, ncol=2)


df%>%
  filter(change_0413 >0) %>%
  arrange(change_0413)
  
plot(df_log_lm, which = 3)


# Prediction for model One
one_CI <- exp(predict(df_log_lm, newdata = tibble(prizes_9014=3), interval= 'confidence'))
two_CI <- exp(predict(df_log_lm, newdata = tibble(prizes_9014=25), interval= 'confidence'))
three_CI <- exp(predict(df_log_lm, newdata = tibble(prizes_9014=50), interval= 'confidence'))

# Prediction for model Two
predict(df_change_lm, newdata = tibble(prizes_9014=3), interval= 'prediction')
predict(df_change_lm, newdata = tibble(prizes_9014=25), interval= 'prediction')
predict(df_change_lm, newdata = tibble(prizes_9014=50), interval= 'prediction')

# Prediction for model One
exp(predict(df_log_lm, newdata = tibble(prizes_9014=3), interval= 'confidence', level = 0.9))
exp(predict(df_log_lm, newdata = tibble(prizes_9014=25), interval= 'confidence',level = 0.9))
exp(predict(df_log_lm, newdata = tibble(prizes_9014=50), interval= 'confidence',level = 0.9))

# Prediction for model Two
predict(df_change_lm, newdata = tibble(prizes_9014=3), interval= 'prediction',level = 0.9)
predict(df_change_lm, newdata = tibble(prizes_9014=25), interval= 'prediction',level = 0.9)
predict(df_change_lm, newdata = tibble(prizes_9014=50), interval= 'prediction',level = 0.9)

plot(df_log_lm, which =5)
plot(df_change_lm, which =2)

bptest(df_change_lm)
bptest(df_log_lm)

ggqqplot(df$change_0413)
ols_test_normality(df_change_lm)
ols_test_correlation(df_log_lm)
ols_test_breusch_pagan(df_log_lm)
ols_plot_resid_hist(df_log_lm)
ols_plot_resid_hist(df_lm)

4/(50 - 1 - 1)

one_CI <- exp(predict(df_log_lm, newdata = tibble(prizes_9014=3), interval= 'confidence', level = 0.9))
two_CI <- exp(predict(df_log_lm, newdata = tibble(prizes_9014=25), interval= 'confidence',level = 0.9))
three_CI <- exp(predict(df_log_lm, newdata = tibble(prizes_9014=50), interval= 'confidence',level = 0.9))

confidence_interval <- tribble(
  ~Pulitzer_Prizes,~less, ~Same, ~more,
  3,one_CI[2], one_CI[1], one_CI[3],
  25,two_CI[2], two_CI[1], two_CI[3],
  50,three_CI[2], three_CI[1], three_CI[3]
)

confidence_interval



confidence_interval <- confidence_interval %>%
  mutate('Lower' = format(less, big.mark=","))%>%
  mutate('Fit' = format(Same, big.mark=","))%>%
  mutate('Upper' = format(more, big.mark=","))

confidence_interval <- confidence_interval%>%
  select(c(Pulitzer_Prizes,Lower,Fit, Upper))

confidence_interval

one_CI

new <- tribble(
  ~Intervals,~less, ~Same, ~more,
  'lower', one_CI[2], two_CI[2], three_CI[2],
  'fit', one_CI[1], two_CI[1], three_CI[1],
  'upper', one_CI[3], two_CI[3], three_CI[3],
)

new <- new %>%
  mutate('Substantially_less' = format(less, big.mark=","))%>%
  mutate('Same_amount' = format(Same, big.mark=","))%>%
  mutate('Substantially_more' = format(more, big.mark=","))

new <- new%>%
  select(c(Intervals,Substantially_less,Same_amount,Substantially_more ))

new


# Prediction Table
one_PI <- predict(df_change_lm, newdata = tibble(prizes_9014=3), interval= 'prediction',level = 0.9)
two_PI <- predict(df_change_lm, newdata = tibble(prizes_9014=25), interval= 'prediction',level = 0.9)
three_PI <- predict(df_change_lm, newdata = tibble(prizes_9014=50), interval= 'prediction',level = 0.9)

new_prediction <- tribble(
  ~Intervals,~less, ~Same, ~more,
  'lower', one_PI[2], two_PI[2], three_PI[2],
  'fit', one_PI[1], two_PI[1], three_PI[1],
  'upper', one_PI[3], two_PI[3], three_PI[3],
)

new_prediction <- new_prediction %>%
  mutate('Substantially_less' = format(less, big.mark=","))%>%
  mutate('Same_amount' = format(Same, big.mark=","))%>%
  mutate('Substantially_more' = format(more, big.mark=","))

new_prediction <- new_prediction%>%
  select(c(Intervals,Substantially_less,Same_amount,Substantially_more ))

new_prediction


df$change_0413

df$mutated <- log(df$change_0413 + 1 - min(df$change_0413))
df


par(mfrow=c(2,2))
plot(df_log_lm, which =1)
plot(df_log_lm, which =2)
plot(df_log_lm, which =3)
plot(df_log_lm, which =4)

par(mfrow=c(2,2))
plot(df_change_lm, which =1)
plot(df_change_lm, which =2)
plot(df_change_lm, which =3)
plot(df_change_lm, which =4)


hist(df_log_lm$effects)

bptest(df_change_lm)

ggplot(data = df, aes(x = df_change_lm$residuals)) +
  geom_histogram(bins = 12, fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')


# Statistical test

ols_test_normality(df_change_lm)
ols_test_correlation(df_log_lm)
ols_test_breusch_pagan(df_change_lm)
ols_plot_resid_hist(df_change_lm)
ols_plot_resid_hist(df_change_lm)

ncvTest(df_change_lm)

gvlma(df_change_lm)

predict(df_change_lm, tibble(prizes_9014 = 25))
df$prizes_9014


par(mfrow=c(1,2))
hist(df_log_lm$residuals, col = 'orange')
plot(df_log_lm, which = 2)

par(mfrow=c(1,2))
scatterplot(x = df$Circ2004_Circ2013, y = df$prizes_9014, )
plot(df_change_lm, which = 1)

par(mfrow=c(1,2))
plot(df_lm, which =1)
plot(df_log_lm, which =1)

df <- df%>%
  mutate('logged' = log(prizes_9014))

df%>%
  ggplot(aes(log(Circ2004_Circ2013),logged))+
  geom_bar(stat = 'identity')


plot(lm(Circ2004_Circ2013~prizes_9014,data=df))

line = 1
cex = 1
side = 3
adj=0.5

par(mfrow=c(2,2))
plot(lm(change_0413~prizes_9014,data=df), which = 1,col = 'orange')
mtext("change_0413~prizes_9014", side=side, line=line, cex=cex, adj=adj)
plot(lm(log(change_0413)~prizes_9014,data=df), which = 1)
mtext("log(Circ2004_Circ2013)~prizes_9014", side=side, line=line, cex=cex, adj=adj)
plot(lm(change_0413~log(prizes_9014),data=df), which = 1)
mtext("Circ2004_Circ2013)~log(prizes_9014)", side=side, line=line, cex=cex, adj=adj)
plot(lm(mutated~log(prizes_9014),data=df), which = 1)
mtext("log(Circ2004_Circ2013)~log(prizes_9014)", side=side, line=line, cex=cex, adj=adj)

par(mfrow=c(2,2))
plot(lm(change_0413~prizes_9014,data=df), which = 2,col = 'orange')
mtext("change_0413~prizes_9014", side=side, line=line, cex=cex, adj=adj)
plot(lm(log(change_0413)~prizes_9014,data=df), which = 2)
mtext("log(Circ2004_Circ2013)~prizes_9014", side=side, line=line, cex=cex, adj=adj)
plot(lm(change_0413~log(prizes_9014),data=df), which = 2)
mtext("Circ2004_Circ2013)~log(prizes_9014)", side=side, line=line, cex=cex, adj=adj)
plot(lm(mutated~log(prizes_9014),data=df), which = 2)
mtext("log(Circ2004_Circ2013)~log(prizes_9014)", side=side, line=line, cex=cex, adj=adj)


par(mfrow=c(2,2))
plot(lm(change_0413~prizes_9014,data=df), which = 3,col = 'orange')
mtext("change_0413~prizes_9014", side=side, line=line, cex=cex, adj=adj)
plot(lm(log(change_0413)~prizes_9014,data=df), which = 3)
mtext("log(Circ2004_Circ2013)~prizes_9014", side=side, line=line, cex=cex, adj=adj)
plot(lm(change_0413~log(prizes_9014),data=df), which = 3)
mtext("Circ2004_Circ2013)~log(prizes_9014)", side=side, line=line, cex=cex, adj=adj)
plot(lm(mutated~log(prizes_9014),data=df), which = 3)
mtext("log(Circ2004_Circ2013)~log(prizes_9014)", side=side, line=line, cex=cex, adj=adj)
  

summary(lm(mutated~log(prizes_9014),data=df))

        