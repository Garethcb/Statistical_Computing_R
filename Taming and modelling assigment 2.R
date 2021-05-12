# load packages
pacman::p_load(pacman, tidyverse, dplyr, stringr, ggplot2,flextable, rmarkdown,modelr,officer,EBImage,huxtable,ggstance,jtools,png,olsrr,lmtest, finalfit,inspectdf,ggcorrplot,moments ,rio, lubridate,car)

# Import csv.file
df <- rio::import('/Users/garethbayvel/Desktop/melbourne.csv',setclass  = 'tibble')

# IncreSe width of output in console
options(tibble.width = 100)

# Isolate day of week from yyyy-mm-dd format and set as factor
df$day_of_week <- weekdays(df$Date)%>%
  as.factor()

# Isolate month from yyyy-mm-dd format and set as factor
df$month <- month(df$Date) %>%
  as.factor()

# View output ITO our bivariate analysis columns
df <- df%>%
  select(Date,'Evaporation (mm)', month,day_of_week, `Minimum temperature (Deg C)`,  
         `Maximum Temperature (Deg C)`,`9am relative humidity (%)`,everything())

# correllation with quanitative variable
cor(df$`Evaporation (mm)`,df$`Minimum temperature (Deg C)`,use="complete.obs")
cor(df$`Evaporation (mm)`,df$`Maximum Temperature (Deg C)`,use="complete.obs")
cor(df$`Evaporation (mm)`,df$`9am relative humidity (%)`,use="complete.obs")

# Skewness of quanitative variables
skewness(df$`Evaporation (mm)`,na.rm = T)
skewness(df$`Minimum temperature (Deg C)`,na.rm = T)
skewness(df$`Maximum Temperature (Deg C)`,na.rm = T)
skewness(df$`9am relative humidity (%)`,na.rm = T)

skewness_table <- tribble(
  ~Variable,~Skewness,
  'Evaporation (mm)',skewness(df$`Evaporation (mm)`,na.rm = T),
  'Minimum temperature (Deg C)', skewness(df$`Minimum temperature (Deg C)`,na.rm = T),
  'Maximum Temperature (Deg C)', skewness(df$`Maximum Temperature (Deg C)`,na.rm = T),
  '9am relative humidity (%)', skewness(df$`9am relative humidity (%)`,na.rm = T)
)

skewness_table <- tribble(
  ~Variable,~Skewness, ~ Transformation, ~Skewness,
  'Evaporation (mm)',round(skewness(df$`Evaporation (mm)`,na.rm = T),digits = 2),'Square Root',round(skewness(df$sqrt_Evaporation,na.rm = T),digits = 2),
  'Minimum temperature (Deg C)', round(skewness(df$`Minimum temperature (Deg C)`,na.rm = T),digits = 2), 'None',round(skewness(df$`Minimum temperature (Deg C)`,na.rm = T),digits = 2),
  'Maximum Temperature (Deg C)', round(skewness(df$`Maximum Temperature (Deg C)`,na.rm = T),digits = 2),"Log",round(skewness(df$log_Maximum_Temperature,na.rm = T),digits = 2),
  '9am relative humidity (%)', round(skewness(df$`9am relative humidity (%)`,na.rm = T),digits = 2), 'None', round(skewness(df$`9am relative humidity (%)`,na.rm = T),digits = 2)
)

write.table(skewness_table, file = 'skewness_table.csv', sep = ',')


#  Distribution of Evaporation
df%>%
  ggplot(aes(x = `Evaporation (mm)`, y = ..density..)) +
  geom_histogram(col = 'black', bins = 30, fill= '#56B4E9') +
  labs(title = "Histogram", subtitle = "Evaporation in (mm)") +
  xlab("Evaporation (mm)") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(`Evaporation (mm)`, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(`Evaporation (mm)`, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#E69F00"))


# sqrt Distribution of Evaporation
df%>%
  ggplot(aes(x = sqrt(`Evaporation (mm)`), y = ..density..)) +
  geom_histogram(col = 'black', bins = 20, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Evaporation in (mm)") +
  xlab("Evaporation (mm)") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(sqrt(`Evaporation (mm)`), na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(sqrt(`Evaporation (mm)`), na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9"))


#  Distribution of Minimum temp
df%>%
  ggplot(aes(x = `Minimum temperature (Deg C)`, y = ..density..)) +
  geom_histogram(col = 'black', bins = 20, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Evaporation in (mm)") +
  xlab("Evaporation (mm)") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(`Minimum temperature (Deg C)`, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(`Minimum temperature (Deg C)`, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9"))

#  Distribution of Max temp
df%>%
  ggplot(aes(x = `Maximum Temperature (Deg C)`, y = ..density..)) +
  geom_histogram(col = 'black', bins = 20, fill= '#56B4E9') +
  labs(title = "Histogram", subtitle = "Evaporation in (mm)") +
  xlab("Evaporation (mm)") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(`Maximum Temperature (Deg C)`, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(`Maximum Temperature (Deg C)`, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#E69F00"))

#  Distribution of log Max temp
df%>%
  ggplot(aes(x = log(`Maximum Temperature (Deg C)`), y = ..density..)) +
  geom_histogram(col = 'black', bins = 20, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Evaporation in (mm)") +
  xlab("Evaporation (mm)") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(log(`Maximum Temperature (Deg C)`), na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(log(`Maximum Temperature (Deg C)`), na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9"))

#  Distribution of 9am relative humidity
df%>%
  ggplot(aes(x = `9am relative humidity (%)`, y = ..density..)) +
  geom_histogram(col = 'black', bins = 20, fill= 'orange') +
  labs(title = "Histogram", subtitle = "Evaporation in (mm)") +
  xlab("Evaporation (mm)") +
  theme(title = element_text(face = 'bold')) +
  geom_vline(aes(xintercept=mean(`9am relative humidity (%)`, na.rm =T),
                 color="mean"), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(`9am relative humidity (%)`, na.rm =T),
                 color="median"), linetype="dashed", size=1) +
  geom_density(alpha = 0.6,fill="azure") +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "#56B4E9"))



# Boxplot to describe Spread of Sqrt Evaporation
df %>%
  ggplot(aes(`Evaporation (mm)`)) +
  geom_boxplot(fill = 'orange') +
  labs(title = "Boxplot (Graph 2b)", subtitle = "Spread of Average Circulation") +
  xlab('Average Circulation')+
  theme(title = element_text(face = 'bold'))

# Boxplot to describe Spread of log Max temp
df %>%
  ggplot(aes(log(`Maximum Temperature (Deg C)`))) +
  geom_boxplot(fill = 'orange') +
  labs(title = "Boxplot", subtitle = "Log Max Temp") +
  xlab('Log Temp')+
  theme(title = element_text(face = 'bold'))

# Boxplot to describe Spread of 9 am relative humidity
df %>%
  ggplot(aes(`9am relative humidity (%)`)) +
  geom_boxplot(fill = 'orange') +
  labs(title = "Boxplot (Graph 2b)", subtitle = "9am relative humidity (%)") +
  xlab('Relative humidity (%)')+
  theme(title = element_text(face = 'bold'))

# Boxplot to describe Spread of Minimum Temp
df %>%
  ggplot(aes(`Minimum temperature (Deg C)`)) +
  geom_boxplot(fill = 'orange') +
  labs(title = "Boxplot", subtitle = "Minimum temperature (Deg C)") +
  xlab('temperature (Deg C)')+
  theme(title = element_text(face = 'bold'))

df <- df%>%
  mutate(sqrt_Evaporation = sqrt(`Evaporation (mm)`))

# View output ITO our bivariate analysis columns
df <- df%>%
  select('Evaporation (mm)',sqrt_Evaporation ,month,day_of_week, `Minimum temperature (Deg C)`,  
         `Maximum Temperature (Deg C)`,`9am relative humidity (%)`,everything())


df%>%
  filter(`Maximum Temperature (Deg C)`> 35.6)


# correllation with quanitative variable
cor(df$sqrt_Evaporation,df$`Minimum temperature (Deg C)`,use="complete.obs")
cor(df$sqrt_Evaporation,df$`Maximum Temperature (Deg C)`,use="complete.obs")
cor(df$sqrt_Evaporation,df$`9am relative humidity (%)`,use="complete.obs")


# Linear Models
step1_lm <- lm(sqrt_Evaporation~ day_of_week + month + `Minimum temperature (Deg C)`+ 
                 log_Maximum_Temperature + `9am relative humidity (%)` +
             `9am relative humidity (%)`:month ,data = df)

summary(step1_lm)
anova(step1_lm)



# step 2
step2_lm <- lm(sqrt_Evaporation~ day_of_week + month + `Minimum temperature (Deg C)`+ 
                 log_Maximum_Temperature + `9am relative humidity (%)`,data = df)
summary(step2_lm)
anova(step2_lm)

# step 3
step3_lm <-lm(sqrt_Evaporation~ month + `Minimum temperature (Deg C)`+ 
                log_Maximum_Temperature + `9am relative humidity (%)`,data = df)

summary(step3_lm)
anova(step3_lm)

# step 4
step4_lm <-lm(sqrt_Evaporation ~ month + `Minimum temperature (Deg C)`+ 
                 `9am relative humidity (%)`,data = df)


# Linear Assumptions

plot(step4_lm, which =3)

bptest(lm(`Evaporation (mm)` ~ month + `Minimum temperature (Deg C)`+ 
     `9am relative humidity (%)`,data = df))

par(mfrow=c(2,1))
plot(step4_lm, which = 1, col = 'orange')
mtext("Transformed Evaporation", side=side, line=line, cex=cex, adj=adj)
plot(lm(`Evaporation (mm)` ~ month + `Minimum temperature (Deg C)`+ 
          `9am relative humidity (%)`,data = df), which = 1)
mtext("Evaporation", side=side, line=line, cex=cex, adj=adj)

# Assumption Tests
gvlma::gvlma(step4_lm)
dwtest(step4_lm)
gqtest(step4_lm)

# Autocorrelation
durbinWatsonTest(step4_lm)

bptest(step4_lm)
ncvTest(step4_lm)

# Predictions
# question 1
confidence_1 <- predict(step4_lm, newdata = tibble(month = '2',
                                   `9am relative humidity (%)`=74, 
                                   `Minimum temperature (Deg C)` = 13.8), 
                                    interval = 'confidence', level = 0.95)^2

prediction_1 <- predict(step4_lm, newdata = tibble(month = '2',
                                   `9am relative humidity (%)`=74, 
                                   `Minimum temperature (Deg C)` = 13.8), 
                                    interval = 'prediction', level = 0.95)^2

# question 2
confidence_2 <- predict(step4_lm, newdata = tibble(month = '12',
                                   `9am relative humidity (%)`=57, 
                                   `Minimum temperature (Deg C)` = 16.4), 
                                    interval = 'confidence', level = 0.95)^2

prediction_2 <- predict(step4_lm, newdata = tibble(month = '12',
                                   `9am relative humidity (%)`=57, 
                                   `Minimum temperature (Deg C)` = 16.4), 
                                    interval = 'prediction', level = 0.95)^2

# question 3
confidence_3 <- predict(step4_lm, newdata = tibble(month = '1',
                                   `9am relative humidity (%)`=35, 
                                   `Minimum temperature (Deg C)` = 26.5), 
                                    interval = 'confidence', level = 0.95)^2

prediction_3 <- predict(step4_lm, newdata = tibble(month = '1',
                                   `9am relative humidity (%)`=35, 
                                   `Minimum temperature (Deg C)` = 26.5), 
                                    interval = 'prediction', level = 0.95)^2


# question 4
confidence_4 <- predict(step4_lm, newdata = tibble(month = '7',
                                   `9am relative humidity (%)`=76, 
                                   `Minimum temperature (Deg C)` = 6.8), 
                                    interval = 'confidence', level = 0.95)^2

prediction_4 <- predict(step4_lm, newdata = tibble(month = '7',
                                   `9am relative humidity (%)`=76, 
                                   `Minimum temperature (Deg C)` = 6.8), 
                                    interval = 'prediction', level = 0.95)^2

# Confidence interval Table
confidence_interval <- tribble(
  ~Month,~`Min Temperature (Deg C)`, ~`9am relative Humidity (%)`, ~lower,~fit,~upper, 
  'February', 13.8, 74, confidence_1[2],confidence_1[1],confidence_1[3],
  'December', 16.4, 57, confidence_2[2],confidence_2[1],confidence_2[3],
  'January', 26.5, 35, confidence_3[2],confidence_3[1],confidence_3[3],
  'July', 6.8, 76, confidence_4[2],confidence_4[1],confidence_4[3]
)
confidence_interval

# Prediction interval Table
prediction_interval <- tribble(
  ~Month,~`Min Temperature (Deg C)`, ~`9am relative Humidity (%)`, ~lower,~fit,~upper, 
  'February', 13.8, 74, prediction_1[2],prediction_1[1],prediction_1[3],
  'December', 16.4, 57, prediction_2[2],prediction_2[1],prediction_2[3],
  'January', 26.5, 35, prediction_3[2],prediction_3[1],prediction_3[3],
  'July', 6.8, 76, prediction_4[2],prediction_4[1],prediction_4[3]
)

prediction_interval


write.table(confidence_interval, file = 'confidence_interval.csv', sep = ',')
write.table(prediction_interval, file = 'prediction_interval.csv', sep = ',')


# Plotting hotest month and most evaporation
df%>%
  group_by(month)%>%
  summarise(mean_Humidity = mean(`9am relative Humidity (%)`,na.rm = T ),
                  mean_Evaporation = mean(`Evaporation (mm)`, na.rm = T))%>%
  ggplot(aes(x = month, y = mean_Humidity, fill = mean_Evaporation)) +
  geom_bar(stat = 'identity')+
  labs(title = "Mean Humidity & Mean Evaporation by Month") +
  ylab('Mean relative Humidity (%)')+
  xlab('Month')


# Correllation Heatmap
mydata <- df[, c(1,4,5,6)]
cormat <- round(cor(mydata,use="complete.obs"),2)
melted_cormat <- melt(cormat)
heatmap <- ggplot(data = melted_cormat, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "Correllation Heatmap")


