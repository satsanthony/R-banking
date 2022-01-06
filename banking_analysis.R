#Completed on 1/1/2022 by Satish A.
#install.packages("treemapify")
library(treemapify)
#Plot packages to be loaded
library(ggplot2)
#install.packages("Hmisc") - To get the default histogram in R
library(Hmisc)
#location of the bank dataset
setwd("D:\\xxxx\\yyyyy\\R\\exercises\\data")
banking <- read.csv("bank.csv")
#check the top 10 records of the data frame banking
head(banking)
#structure of the data frame
str(banking)
#stats of the data frame
summary(banking)
head(banking)
library(ggplot2)
#Analysis of Termed Subscriptions - Basic column plot
TS <- ggplot(data=banking, aes(education, fill = deposit))
TS + geom_bar()

#Analysis of % Deposit by Education - Plot #1
library(dplyr)
rm(banking)
banking <- read.csv("bank.csv")
banking$eddep <- paste(banking$education,banking$deposit)
bankingnew <- select(banking, eddep,education,deposit)
g1= bankingnew %>%
  group_by(eddep,education,deposit) %>%
  summarise(pct = length(eddep)/length(banking)) %>%
  ggplot(aes(x = education, y= pct, fill = deposit)) +
  geom_text(aes(label = scales::percent(round(pct/1000,2))),nudge_y = 3.5)+
  geom_bar(stat="identity", width=.5, position = "dodge")+
  scale_y_continuous(labels = function(x) paste0(x*0.1, "%"))

g2 = ggplot(data=banking, aes(x = "", fill = factor(deposit))) +
  geom_bar(stat= "count", width = 1, color = "white") +
  geom_text(aes(label = scales::percent(..count.. / sum(..count..))), stat = "count", position = position_stack(vjust = .5)) +
  coord_polar("y", start = 0, direction = -1) +
  scale_fill_manual(values = c("#00BA38", "#619CFF", "#F8766D")) +
  ggtitle("Breakdown of Deposit")+
  theme_void()

library(gridExtra)
grid.arrange(g2, g1, ncol=2, left = "Analysis of Desposit",
             right = "Analysis of Desposit count % by Education", top="Analysis by Deposit%")


#Distribution of Numeric Data
#install.packages("Hmisc") - Plot #2
library(Hmisc)
hist.data.frame(banking)

#boxplot to analyze balance by deposit flag
u <- ggplot(data=banking, aes(x=deposit, y=balance,
                             color=deposit))
u + geom_boxplot(size=1.2) + geom_jitter()
u + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)

#boxplot to analyze job categories vs deposit flag - Plot #3
v <- ggplot(data=banking, aes(x=job, y=balance,
                              color=deposit))
v + geom_boxplot(size=1.2) + geom_jitter()
v + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)+
  ggtitle("Analysis of Job Categories vs Deposits")


#distribution by education vs deposit flag
vplot <- ggplot(data=banking, aes(x=education, y=balance,
                              color=deposit))
vplot + geom_violin(size=1, alpha=0.9)

#Frequency of Jobs in the Banking data set - Plot#4
#https://r-charts.com/part-whole/treemapify/
tdf <- data.frame(table(banking$job))
ggplot(tdf, aes(area = Freq, fill = Var1,
               label = paste(Var1, Freq, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "maroon",
                    place = "centre",
                    size = 15) +
  scale_fill_brewer(palette = "Blues")+
  ggtitle("Count of Job Categories in the Data set")+
  theme(legend.position = "none")

#boxplot to present Age distribution by type of job - Plot#5
c <- banking[banking$deposit=="yes",]
occ <- ggplot(data=c, aes(x=job, y=age,
                              color=balance))
occ + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)+
  coord_cartesian(ylim=c(10,100)) +
  ggtitle("Age Distribution by Occupation") +
  theme(axis.title.x = element_text(color="DarkGreen", size=30),
        axis.title.y = element_text(color="Magenta", size=30),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title.position = "panel",
        plot.title = element_text(color="DarkBlue",
                                  size=25, family="Courier"))

#Marital Status - Married
m <- banking[banking$marital=="married",]
ggplot(data=m, aes(x = balance)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "blue") +
  geom_density()

#Marital Status - divorced
d <- banking[banking$marital=="divorced",]
ggplot(data=d, aes(x = balance)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "green") +
  geom_density()

#Marital Status - single
s <- banking[banking$marital=="single",]
ggplot(data=s, aes(x = balance)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "gray") +
  geom_density()

#Scatterplot for Balances by Marital Status - Plot#6
ggplot(data=banking, aes(x = duration, y = balance, 
                         color = marital)) +
  ggtitle("Balances by Marital Status") +
  geom_point(size=5)

#Analysis of missed opportunities
w <- ggplot(data=banking, aes(x=marital, y=balance, color=marital))
w + geom_boxplot(size=1.2) + facet_grid(marital~deposit)+
  #scale_y_continuous(labels = paste0(ylab, "K"), breaks = 10^3 * ylab)+
  scale_y_continuous(labels = scales::dollar)+
  ggtitle("Analysis of missed opportunities") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title.position = "panel",
        plot.title = element_text(color="DarkBlue",
                                  size=25, family="Courier"))

#Calculate median of income groups
rm(medbal)
medbal <- read.csv("bank.csv")
for (i in 1:11162){
       if (isTRUE(medbal$marital[i] == "married" && medbal$education[i]=='secondary')){medbal$marital_ed[i] <- 'married-secondary'}
  else if (isTRUE(medbal$marital[i] == "married" && medbal$education[i]=='tertiary')){medbal$marital_ed[i] <- 'married-tertiary'}
  else if (isTRUE(medbal$marital[i] == "married" && medbal$education[i]=='primary')){medbal$marital_ed[i] <- 'married-primary'}
  else if (isTRUE(medbal$marital[i] == "divorced" && medbal$education[i]=='secondary')){medbal$marital_ed[i] <- 'divorced-secondary'}
  else if (isTRUE(medbal$marital[i] == "divorced" && medbal$education[i]=='tertiary')){medbal$marital_ed[i] <- 'divorced-tertiary'}
  else if (isTRUE(medbal$marital[i] == "divorced" && medbal$education[i]=='primary')){medbal$marital_ed[i] <- 'divorced-primary'}
  else if (isTRUE(medbal$marital[i] == "single" && medbal$education[i]=='secondary')){medbal$marital_ed[i] <- 'single-secondary'}
  else if (isTRUE(medbal$marital[i] == "single" && medbal$education[i]=='tertiary')){medbal$marital_ed[i] <- 'single-tertiary'}
  else if (isTRUE(medbal$marital[i] == "single" && medbal$education[i]=='primary')){medbal$marital_ed[i] <- 'single-primary'}
  else {medbal$marital_ed[i] <- ''}
}

#if you need to export to csv, use the command below
#write.csv(medbal,"D:\\xxxx\yyyy\\R\\exercises\\data\\medbal.csv", row.names = FALSE)

med_bal <- aggregate(medbal$balance, by=list(medbal$marital_ed), FUN=median)
#Rename column names
colnames(med_bal)[colnames(med_bal) == 'Group.1'] <- 'MaritalEd'
colnames(med_bal)[colnames(med_bal) == 'x'] <- 'MedianBalance'
#Filter null / unknown education category
med_bal <- med_bal[med_bal$MaritalEd !="",]
med_bal
# Plot#7
ggplot(med_bal, aes(x = MaritalEd, y = MedianBalance)) +
  geom_segment(aes(x = MaritalEd, xend = MaritalEd, y = 0, yend = MedianBalance),
               color = "gray", lwd = 1) +
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1) +
  geom_text(aes(label = MedianBalance), color = "white", size = 3) +
  coord_flip() +
  ggtitle("Median Balance by Marital/Education Grouping") +
  #theme_minimal()
  theme(plot.title.position = "panel",
      plot.title = element_text(color="DarkBlue",
                                size=25, family="Courier"))

#Distribution of loans to Marital Groups with Education
loanbal <- aggregate(medbal$balance, by=list(medbal$marital_ed, medbal$loan), FUN=median)
#Rename column names
colnames(loanbal)[colnames(loanbal) == 'Group.1'] <- 'MaritalEd'
colnames(loanbal)[colnames(loanbal) == 'Group.2'] <- 'loan'
colnames(loanbal)[colnames(loanbal) == 'x'] <- 'MedianBalance'
#Filter null / unknown education category
loan_bal <- loanbal[loanbal$MaritalEd !="",]
loan_bal
# Plot#8
ggplot(loan_bal, aes(x = MedianBalance, y = MaritalEd, color = loan,
               shape = loan)) +
  geom_point(size = 7)+
  ggtitle("Distribution of loans to Marital Groups with Education") +
  theme(axis.title.x = element_text(color="DarkGreen", size=15),
        axis.title.y = element_text(color="Magenta", size=5),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title.position = "panel",
        plot.title = element_text(color="DarkBlue",
                                  size=25, family="Courier"))

#Avg. duration to open a deposit account (1528.539)
durationbal <- read.csv("bank.csv")
avgduration <- mean(durationbal$balance)
for (i in 1:11162){
  if (isTRUE(durationbal$duration[i] > avgduration)){durationbal$MeanBalance[i] <- 'Above'}
  else{durationbal$MeanBalance[i] <- 'Below'}
}

dur_bal <- aggregate(durationbal$balance, by=list(durationbal$MeanBalance, durationbal$deposit), FUN=length)
dur_bal
rm(above_avg)
rm(below_avg)
colnames(dur_bal)[colnames(dur_bal) == 'Group.1'] <- 'AvgDuration'
colnames(dur_bal)[colnames(dur_bal) == 'Group.2'] <- 'HasDeposit'
colnames(dur_bal)[colnames(dur_bal) == 'x'] <- 'CountStatus'
above_avg <- dur_bal[dur_bal$AvgDuration == 'Above',]
below_avg <- dur_bal[dur_bal$AvgDuration == 'Below',]
above_avg$pct <- c(round((above_avg$CountStatus/sum(above_avg$CountStatus)*100),2))
below_avg$pct <- c(round((below_avg$CountStatus/sum(below_avg$CountStatus)*100),2))

#need gridExtra library to combine two charts
library(gridExtra)
# Plot#9
p1<- ggplot(above_avg, aes(x = "", y = pct, fill = HasDeposit)) +
  geom_col(color = "black") +
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#EACF65", "#3C8D53")) +
  theme(axis.title.x = element_text(color="DarkGreen", size=15),
        axis.title.y = element_text(color="Magenta", size=5),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title.position = "panel",
        plot.title = element_text(color="DarkBlue",
                                  size=25, family="Courier"))
p1
p2<- ggplot(below_avg, aes(x = "", y = pct, fill = HasDeposit)) +
  geom_col(color = "black") +
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#BE2A3E", "#EC754A")) +
  theme(axis.title.x = element_text(color="DarkGreen", size=15),
        axis.title.y = element_text(color="Magenta", size=5),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title.position = "panel",
        plot.title = element_text(color="DarkBlue",
                                  size=25, family="Courier"))
p2
grid.arrange(p1, p2, ncol=2, left = "Above Mean of Duration",
             right = "Below Mean of Duration", top="%Conversion to Open Account")


  
