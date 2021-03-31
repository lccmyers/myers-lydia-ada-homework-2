library(tidyverse)
library(mosaic)
library(gridExtra)

d<-read_csv("https://raw.githubusercontent.com/difiore/ada-2021-datasets/master/IMDB-movies.csv",col_names=T)
head(d)

#filtering 
d<-filter(d, startYear %in% 1920:1979  & runtimeMinutes %in% 60:180)
d<-mutate(d, "decade"=paste(10*as.integer(as.numeric(substring(as.character(d$startYear), 3, 4)) / 10)))

#plotting histograms
ggplot(d, aes(x=runtimeMinutes)) + geom_histogram(bins=10)+
  facet_wrap(~decade,scales='free_x')

#calculating mean and sd
print(results <- summarise((group_by(d,decade)), avg = mean(runtimeMinutes, na.rm = T), 
  sd=sd(runtimeMinutes,na.rm=T)))


#creating sample, estimating sample mean & sd, and estimating standard error
set.seed(100)
print(sample <- d %>% group_by(decade) %>% sample_n(100,replace=F)%>%
  summarise(sampleMean=mean(runtimeMinutes,na.rm=T),
      sampleSD=sd(runtimeMinutes,na.rm=T)))
print(se_mean <- sample$sampleMean/(sqrt(100)))

#comparison...the sampling seems to have been accurate, and the standard
#error seems to be accurate as well, but maybe a little high
sample
results
se_mean

#creating sample distribution
sample_dist<-do(1000) * d %>% group_by(decade) %>% sample_n(100,replace=F)%>%
  summarise(sampleMean=mean(runtimeMinutes,na.rm=T),
            sampleSD=sd(runtimeMinutes,na.rm=T))

mean(sample_dist$sampleMean)
sd(sample_dist$sampleSD)

hist(sample_dist$sampleMean,main="Sample Distribution",xlab="Mean")

#the distribution is somewhat normal, not exactly but close

#standard error of mean for first sample
se_mean
#standard error of known population standard deviations for each decade
results$sd/(sqrt(5651))
#standard error as esitmated from sampling distribution of sample means
sample_dist$sampleMean/sqrt(100)




#Challenge 2
l <- 12
#What is the probability that she will see 9 or fewer bees arrive during any given session?
ppois(9,l)

#What is the probability that she will see no bees arrive in a session?
ppois(0,l)

#What is the probability that she will see exactly 5 bees arrive in a session?
dpois(5,l)

#What is the probability that she will see more than 18 bees arrive in a session?
ppois(19,l,lower.tail=F)

#Plot the relevant Poisson mass function over the values in range 0 ≤  x ≤ 24.
x<-c(0:24)
plot(x,dpois(x,l),type='h',main="Poisson Distribution w/lambda = 12")

#Using the rpois() function, simulate 
#1460 results from this distribution (i.e., 4 full years of morning monitoring sessions).
r<-rpois(1460,l)

#Plot the simulated results using the histogram() function from the 
#{mosaic} package and use xlim() to set the horizontal limits to be from 0 to 24. 
#How do your simulated results compare to the shape of the probability mass function you plotted above?
histogram(r,xlim=c(0,24))

#my results are pretty much the same as the probability mass function I plotted above




#Challenge 3

#loading in data
d<-read_csv("https://raw.githubusercontent.com/difiore/ada-2021-datasets/master/zombies.csv",col_names=T)
head(d)

#getting mean and sd
colMeans(d[ , c(5, 6,7,8,10)])
n<-1000
popSD<-function(x){sqrt((n-1)/n) * sd(x)}
apply(d[ , c(5, 6,7,8,10)], 2, popSD)

#boxplots
p1<-ggplot(d, aes(x=gender, y=height)) + 
  geom_boxplot()
p2<-ggplot(d, aes(x=gender, y=weight)) + 
  geom_boxplot()
p3<-ggplot(d, aes(x=gender, y=zombies_killed)) + 
  geom_boxplot()
p4<-ggplot(d, aes(x=gender, y=years_of_education)) + 
  geom_boxplot()
p5<-ggplot(d,aes(x=gender, y=age)) +
  geom_boxplot()

grid.arrange(p1, p2,p3,p4,p5, nrow=2, ncol=3)

#scatterplots
p1<-ggplot(d, aes(x=height, y=age,color=gender)) + geom_point()
p2<-ggplot(d, aes(x=weight, y=age, color=gender)) + geom_point()

grid.arrange(p1,p2,nrow=2)
#height seems more correlated than weight with age - seems to be 
#a direct relationship

p1<-ggplot(d, aes(x=height)) +geom_histogram(binwidth=1)
p2<-ggplot(d, aes(x=weight)) +geom_histogram(binwidth=1)
p3<-ggplot(d, aes(x=zombies_killed)) +geom_histogram(binwidth=1)
p4<-ggplot(d, aes(x=years_of_education)) +geom_histogram(binwidth=1)
p5<-ggplot(d, aes(x=age)) +geom_histogram(binwidth=1)

grid.arrange(p1, p2,p3,p4,p5, nrow=2, ncol=3)

#q-q plots
qqnorm(d$height, main = "Height")
qqline(d$height, col = "red")

qqnorm(d$weight, main = "Weight")
qqline(d$weight, col = "red")

qqnorm(d$zombies_killed, main = "Zombies Killed")
qqline(d$zombies_killed, col = "red")

qqnorm(d$years_of_education, main = "Years of Education")
qqline(d$years_of_education, col = "red")

qqnorm(d$age, main = "Age")
qqline(d$age, col = "red")

#it looks like all but zombies killed and years of education are normally distributed

#taking a sample, etc
set.seed(50)
sample <- d %>% sample_n(50,replace=F)

#mean, SD, SE of each variable
print(means<-colMeans(sample[ ,c(5,6,7,8,10)]))
print(sds<-apply(sample[ , c(5, 6,7,8,10)], 2, sd))
print(se_mean <- means/(sqrt(50)))
print(se_sds<-sds/(sqrt(50)))

#getting confidence intervals
error<- qnorm(.975)*sds/sqrt(50)
print(lower<-means-error)
print(upper<-means+error)

#drawing another 99 samples
sample_dist<-do(50) * d %>%sample_n(50,replace=F) 
m<-aggregate(.~.index, data=sample_dist[ ,c(5,6,7,8,10,12)], mean)
standev<-aggregate(.~.index, data=sample_dist[ ,c(5,6,7,8,10,12)], sd)
print(means<-colMeans(m[ ,c(2,3,4,5,6)]))
print(sds<-apply(m[ , c(2,3,4,5,6)], 2, sd))

#Confidence intervals
error<- qnorm(.975)*sds/sqrt(50)
print(lower_dist<-means-error)
print(upper_dist<-means+error)

#
sds
se_sds
#the standard deviations from the sampling distribution are  much smaller than the
#standard errors I calculated in my first sample


p1<-ggplot(sample_dist, aes(x=height)) +geom_histogram(binwidth=1)
p2<-ggplot(sample_dist, aes(x=weight)) +geom_histogram(binwidth=1)
p3<-ggplot(sample_dist, aes(x=zombies_killed)) +geom_histogram(binwidth=1)
p4<-ggplot(sample_dist, aes(x=years_of_education)) +geom_histogram(binwidth=1)
p5<-ggplot(sample_dist, aes(x=age)) +geom_histogram(binwidth=1)

grid.arrange(p1, p2,p3,p4,p5, nrow=2, ncol=3)
#they're all normally distributed except zombies killed and years
#of education, which are left skewed
upper
upper_dist
lower
lower_dist
#the CIs are very similar to each other

