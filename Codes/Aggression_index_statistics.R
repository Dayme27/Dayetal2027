####Aggression#####
#1.Statistics for aggression quantification

#Clear environment
rm(list=ls())
#Read in data
data<-read.delim("file_name.txt",h=T,sep="\t",na.strings = "NA")
attach(data)

#Extract aggression index for no threat and threat conditions
NT<-total_agg_index  [which(threat=="NT")]
ET<-total_agg_index  [which(threat=="ET")]

#Check normality
shapiro.test(NT)
shapiro.test(ET)

#Run appropriate test
wilcox.test(NT,ET, paired= FALSE)

#Extract mean, median and quantiles for no threat and threat condition
mean(ET)
median(ET)
quantile(ET)

mean(NT)
median(NT)
quantile(NT)
