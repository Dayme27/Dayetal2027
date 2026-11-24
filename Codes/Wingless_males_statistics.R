####Wingless males#####
#1.Statistics for wingless male quantification
#a. Courtship index
#b. Defensive index 


#Clear environment
rm(list=ls())
#Read in data
data<-read.delim("file_name.txt",h=T,sep="\t",na.strings = "NA")
attach(data)

##a. Courtship index
#Extract courtship index for no threat and threat conditions
comp_NT<-CI  [which(threat=="NT")]
comp_ET<-CI  [which(threat=="ET")]

#Check normality
shapiro.test(comp_NT)
shapiro.test(comp_ET)

#Run appropriate test
wilcox.test(comp_NT,comp_ET, paired=FALSE)

#Extract mean, median and quantiles for no threat and threat conditions
mean(comp_NT)
median(comp_NT)
quantile(comp_NT)

mean(comp_ET)
median(comp_ET)
quantile(comp_ET)


##b. Defensive index 
#Extract defensive index for no threat and threat conditions
comp_NT_DI<-DI  [which(threat=="NT")]
comp_ET_DI<-DI  [which(threat=="ET")]

#Check normality
shapiro.test(comp_NT_DI)
shapiro.test(comp_ET_DI)

#Run appropriate test
wilcox.test(comp_NT_DI,comp_ET_DI, paired=FALSE)

#Extract mean, median and quantiles for no threat and threat conditions
mean(comp_NT_DI)
median(comp_NT_DI)
quantile(comp_NT_DI)

mean(comp_ET_DI)
median(comp_ET_DI)
quantile(comp_ET_DI)



