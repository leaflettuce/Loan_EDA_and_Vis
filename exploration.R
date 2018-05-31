library(ggplot2)
library(GGally)
library(gridExtra)
library(tidyverse)
library(plyr)

setwd('D://projects/SNHU/loans')
df <- read.csv('data/Loans_Written.csv')
df_check <- read.csv('data/Loans_Written.csv')
df_rej <- read.csv('data/Loans_Rejected.csv')

summary(df)

#set for regression 
df_reg <- df
#grade to ordinal
df_reg$grade <- as.integer(df_reg$grade)
#employ length to ordinal
df$emp_length <- factor(df$emp_length, levels = c("n/a", "< 1 year", "1 year", "2 years", 
                                                  "3 years", "4 years", "5 years",
                                                  "6 years", "7 years", "8 years",
                                                  "9 years", "10+ years"))
df_reg$emp_length <- as.integer(df_reg$emp_length)
#drop other categoricals
df_reg <- subset(df_reg, select = -c(?..id, member_id, emp_title, title, addr_state, 
                                  sub_grade, int_rate, installment, funded_amnt))

fit <- lm(loan_amnt ~ ., data=df_reg)

summary(fit)
  
#amt by length
ggplot(aes(x =emp_length, y = loan_amnt, alpha = 0.1), data = df[1:3000,]) +
  geom_jitter() + 
  ggtitle('emp length by loan amnt')

#cor matrix
df_pairs <- subset(df_reg, select = -c(ï..id, member_id, title, addr_state, int_rate,
                                        emp_title, sub_grade, installment, funded_amnt))
ggpairs(df_pairs)

#amnt by income
ggplot(aes(x =annual_inc, y = loan_amnt, alpha = 0.1), data = df[1:3000,]) +
  geom_jitter() + 
  ggtitle('annual inc by loan amnt')


#rejected cor matrix
df_rej_drop <- subset(df_rej, select = -c(ï..ID, State))
df_rej_drop$Application.Date <- as.Date(df_rej_drop$Application.Date)
df
df_rej_drop$Debt.To.Income.Ratio <- substr(df_rej_drop$Debt.To.Income.Ratio, 1, 
                                    nchar(as.character(df_rej_drop$Debt.To.Income.Ratio)) - 1)
df_rej_drop$Debt.To.Income.Ratio <- as.numeric(df_rej_drop$Debt.To.Income.Ratio)
ggpairs(df_rej_drop)



#####
#CREATE COMBO DF
#####
df_app <- data.frame(df$emp_length, df$loan_amnt)
df_app$result <- 'approved'
colnames(df_app)[1] <- c('emp_length')
colnames(df_app)[2] <- c('loan_amnt')

df_rej2 <- data.frame(df_rej$Employment.Length, df_rej$Amount.Requested)
df_rej2$result <- 'rejected'
colnames(df_rej2)[1] <- c('emp_length')
colnames(df_rej2)[2] <- c('loan_amnt')

df_comb <- rbind(df_app, df_rej2)

##scatter of results by emp length
g1 <- ggplot(aes(x =emp_length), data = subset(df_comb, df_comb$result == 'approved')) +
  geom_histogram(stat = 'count') + 
  ggtitle('approved requests')

g2 <- ggplot(aes(x =emp_length), data = subset(df_comb, df_comb$result == 'rejected')) +
  geom_histogram(stat = 'count') + 
  ggtitle('rejected requests')

g3 <- ggplot(aes(x =emp_length, y = loan_amnt), data = subset(df_comb, df_comb$result == 'approved')) +
  geom_boxplot() + 
  ggtitle('approved requests')

g4 <- ggplot(aes(x =emp_length, y = loan_amnt), data = subset(df_comb, df_comb$result == 'rejected')) +
  geom_boxplot() + 
  ggtitle('rejected requests')

grid.arrange(g1, g2, g3, g4, ncol=2)

###emp length rates of approval
app_rates <- df_comb %>% group_by(emp_length, result) %>%
              summarize(count = length(result))
app_rates$total_count <- c(10971, 10971, 258797, 258797, 11535, 11535, 15821, 15821,
                           14259, 14295, 10434, 10434, 22657, 22657, 9943, 9943, 
                           9695, 9695, 7990, 7990, 6814, 6814, 60063, 60063)

app_rates$ratio <- (app_rates$count/app_rates$total_count)

ggplot(aes(x =emp_length, y = ratio, fill = result), data = app_rates) +
  geom_bar(stat = 'identity') + 
  ggtitle('approved v rejected rates')


##########################
##subset to 2/3rd range
##########################

summary(df$loan_amnt)

approved_mean <- mean(df$loan_amnt)
approved_median <- median(df$loan_amnt)
approved_sd <- sd(df$loan_amnt)
lower_range <- approved_mean - approved_sd
upper_range <- approved_mean + approved_sd

#check log
df_log <- df
df_log$loan_amnt <- log(df$loan_amnt)

ggplot(aes(x = loan_amnt), data = df_log) + 
  geom_histogram(bins = 10)+
  ggtitle('loan amount histo')
# NO LOG!

#MAD AND MEDIAN
loan_mad <- mad(df$loan_amnt)
m_lower_range <- approved_median - loan_mad
m_upper_range <- approved_median + loan_mad

mad_range <- subset(df, loan_amnt >= round(m_lower_range) & loan_amnt <= round(m_upper_range))
nrow(mad_range)/nrow(df)


#QUANTILE RANGE
quant <- quantile(df$loan_amnt, prob = c(0.18, 0.83))

quantile_range <- subset(df, loan_amnt >= 6000 & loan_amnt <= 21000)
nrow(quantile_range)/nrow(df)

#MEAN AND SD RANGE
test_range <- subset(df, loan_amnt >= round(lower_range) & loan_amnt <= round(upper_range))
nrow(test_range)/nrow(df)

#PLOTS
j1 <- ggplot(aes(x = loan_amnt), data = df) + 
  geom_histogram(bins = 10)+
  ggtitle('loan amount histo')

j2 <- ggplot(aes(x = loan_amnt), data = quantile_range) + 
  geom_histogram(bins = 10)+
  ggtitle('Quantile')

j3 <- ggplot(aes(x = loan_amnt), data = test_range) + 
  geom_histogram(bins = 10)+
  ggtitle('Mean-SD')

j4 <- ggplot(aes(x = loan_amnt), data = mad_range) + 
  geom_histogram(bins = 10)+
  ggtitle('MAD')


grid.arrange(j1, j2, j3, j4, ncol = 2)

ggplot(aes(x = loan_amnt, fill = result), data = df_comb) + 
  geom_histogram(bins = 8)+
  ggtitle('loan amount hist (w/ result')


#############################
#### subset rejection and explore
##########################

range_rej <-subset(df_rej, Amount.Requested >= 6000 & Amount.Requested <= 21000)

ggplot(aes(x = Application.Date, y = Amount.Requested), data = range_rej) +
  geom_point(stat = 'identity') +
  ggtitle('amount requested by date')

##REJ HISTOGRAMS
ggplot(aes(x = Amount.Requested), data = range_rej) +
  geom_histogram() +
  ggtitle('amount requested')

ggplot(aes(x = Employment.Length), data = range_rej) +
  geom_histogram(stat = 'count') +
  ggtitle('Emp Length')

ggplot(aes(x = Employment.Length, y = Amount.Requested), data = range_rej) +
  geom_point(alpha = 0.1) +
  ggtitle('amount by length')

###DEBT CLEAN AND HIST
range_rej$Debt.To.Income.Ratio <- substr(range_rej$Debt.To.Income.Ratio, 1, 
                                           nchar(as.character(range_rej$Debt.To.Income.Ratio)) - 1)
range_rej$Debt.To.Income.Ratio <- as.numeric(range_rej$Debt.To.Income.Ratio)

range_rej$Employment.Length <- factor(range_rej$Employment.Length, levels = c("n/a", "< 1 year", "1 year", "2 years", 
                                                  "3 years", "4 years", "5 years",
                                                  "6 years", "7 years", "8 years",
                                                  "9 years", "10+ years"))
range_rej$Employment.Length <- as.integer(range_rej$Employment.Length)

ggplot(aes(x = Debt.To.Income.Ratio), data = subset(range_rej, Debt.To.Income.Ratio > 0 & 
                                                      Debt.To.Income.Ratio < 100)) +
  geom_histogram(bins = 100) +
  ggtitle('Debt to inc')


#SCORE
ggplot(aes(x = Risk_Score), data = range_rej) +
  geom_histogram(bins = 100) +
  ggtitle('amount by length')

ggplot(aes(x = State), data = range_rej) +
  geom_histogram(stat = 'count') +
  ggtitle('amount by length')


ggplot(aes(x = Debt.To.Income.Ratio, y = Risk_Score, color = Employment.Length), 
       data = subset(range_rej, Debt.To.Income.Ratio <= 100)) +
  geom_point(stat = 'identity', alpha = 0.1) +
  ggtitle('debt to score')

############
##HISTO OF APPROVED
############
ggplot(aes(x = emp_length), data = quantile_range) +
  geom_histogram(stat = 'count') +
  ggtitle('length hist')


ggplot(aes(x = annual_inc), data = subset(quantile_range, annual_inc <= 200000)) +
  geom_histogram() +
  ggtitle('annual hist')


ggplot(aes(x = loan_amnt), data = quantile_range) +
  geom_histogram() +
  ggtitle('amnt hist')

ggplot(aes(x = addr_state), data = quantile_range) +
  geom_histogram(stat = 'count') +
  ggtitle('state histo')

ggplot(aes(x = home_ownership), data = quantile_range) +
  geom_histogram(stat = 'count') +
  ggtitle('home hist')
#
ggplot(aes(x = home_ownership, y = loan_amnt), data = quantile_range) +
  geom_jitter(alpha = 0.1, stat = 'identity') +
  ggtitle('home hist')
#

ggplot(aes(x = grade), data = quantile_range) +
  geom_histogram(stat = 'count') +
  ggtitle('grade')

ggplot(aes(x = term), data = quantile_range) +
  geom_histogram(stat = 'count') +
  ggtitle('term hist')

ggplot(aes(x = installment), data = quantile_range) +
  geom_histogram() +
  ggtitle('install hist')

ggplot(aes(x = int_rate), data = quantile_range) +
  geom_histogram(stat = 'count') +
  ggtitle('int rate hist')

ggplot(aes(x = annual_inc, y = loan_amnt, color = term), data = quantile_range) +
  geom_point(stat = 'identity', alpha = 0.1) +
  ggtitle('inc by amount by term') +
  xlim(0, 300000)


#lets clean and output a new file
summary(df)
unique(df$addr_state)


df$int_rate <- substr(df$int_rate, 1, 
                                  nchar(as.character(df$int_rate)) - 1)
df$int_rate <- as.numeric(df$int_rate)


#write it out
write.csv(df, file = "D://projects/SNHU/loans/for_python.csv", row.names = FALSE)


###################
# REASONS
###############

df2 <- read.csv('loans_cleaned.csv')

ggplot(aes(x = title), data = df2) + 
  geom_histogram(stat = 'count')

#make changes to range
quantile_range2 <- subset(df2, loan_amnt >= 6000 & loan_amnt <= 21000)


l1 <- ggplot(aes(x = title, y = loan_amnt, color = int_rate), data =df2) +
  geom_jitter(stat = 'identity', alpha = 0.1) +
  ggtitle('reason to amount')

l2 <- ggplot(aes(x = title, y = loan_amnt, color = int_rate), data =quantile_range2) +
  geom_jitter(stat = 'identity', alpha = 0.1) +
  ggtitle('reason to amount')

grid.arrange(l1, l2, ncol = 1)

#WRITE CLEANED FILES
write.csv(quantile_range2, file = "D://projects/SNHU/loans/for_vis_approved.csv", row.names = FALSE)
write.csv(range_rej, file = "D://projects/SNHU/loans/for_vis_rejected.csv", row.names = FALSE)
