# Before running this code, generate your own personal dataset using 
# the R script my_dataset
# It is a script to generate data in which an orchard has 
# a sample of apples weighed in each year from each tree.
# All trees are fertilized each year, but the amount of
# fertilizer per tree varies slightly among years

# Try this
source(file.choose())

# or set your working directory to the appropriate folder and use
# source('my_dataset.R')



attach(MyData)


# Get an idea of the data by printing it out
head(MyData,30)



cbind(appleWT, YearID,VarID,TreeID,Fdose)

# use install.packages(c('nlme','lattice'),dependencies=T) if necessary
# Then fire up the packages nlme

library(nlme)
library(lattice)



M1<-lm(appleWT~YearID)
M2<-lme(appleWT~1,random=~1|YearID)

# Have a look at the overall relationship with fertilizer does
xyplot(appleWT+fitted(M1)+fitted(M2)~Fdose)

# Split it down by tree
xyplot(appleWT+fitted(M1)+fitted(M2)~Fdose|TreeID)



# print out the two models
summary(M1)
summary(M2)

# use anova(M1) to find the mean square of the fixed effects model
# square root it to find the pooled estimate of the SD.
# how does this compare with the SD in M2
# what is the other SD in M2?
# what is the intercept?

# are the estimated means for each year in M1 and M2 identical
# use coef(M1) & coef(M2) to see

# construct a fixed effects (lm) and random effects (lme)
# model to model the effect of tree instead of year
# call these Models M3 and M4


# use this command to plot them

xyplot(appleWT+fitted(M3)+fitted(M4)~TreeID)

# Why is the shrinkage different in different trees?

# Try fitting TreeID and Fdose like this
M7a<-lm(appleWT~TreeID+YearID+Fdose+VarID)

#  fixed effects model do it?
# Try this and see what goes on


# Just fit Fdose as in a fixed effects model
# but both Fdose and Tree as a random effect!
M7<-lm(appleWT~Fdose+TreeID)
M8<-lme(appleWT~Fdose+TreeID,random=~1|YearID)

xyplot(appleWT+fitted(M7)+fitted(M8)~Fdose|TreeID)

# Why is it possible to fit a random effects model, but not a fixed effects model with both
Fdose and TreeID??

######################################################
########### Exercise ends ############################
######################################################

