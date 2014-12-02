
# For the initial exercise generate your own unique dataset for tree1, 2 & 3

apples1<-rnorm(40,rnorm(1,20,3),rnorm(1,5))
apples2<-rnorm(40,rnorm(1,40,3),rnorm(1,10))
apples3<-rnorm(40,rnorm(1,50,3),rnorm(1,15))

par(mfrow=c(3,1))
hist(apples1,xlim=c(0,100),breaks=seq(0,150,5), border='green')
hist(apples2,xlim=c(0,100),breaks=seq(0,150,5), border='red')
hist(apples3,xlim=c(0,100),breaks=seq(0,150,5), border='blue')

# use the mean() and sd() commands to obtain the mean and SD of each distribution
# compare them with your neighbour

# use lm() command to find the mean & residual variation for each of apples1-3
# e.g. mod1<-lm(apples1~1); summary(mod1)
# Make sure you understand what the residual standard error is
# make sure you understand what the intercept and 'Std. Error' (of the intercept) is, and why it is different

# How do these values relate to the mean and sd you calculated?

# Now bind your dataset into one long vector, and create a factor to distinguish the data from each tree
allApples<-c(apples1,apples2,apples3)
treef<-factor(rep(c('Tree1','Tree2','Tree3'),each=40))
# print them out to have a look by typing their names 'allApples' and 'treef'

# use mod4<-lm(allApples~treef) to fit all 3 tree means.
# use the summary() command to look at the results
# Which mean is the intercept equal to, why?
# Why are the values relating to the other two means not the same as you found before?
# Why are the standard errors for these two values the same, when they have different variances?

# Install the library allowing you to run mixed effects models
library(nlme)

# Fit a simple nlme
mod5<-lme(allApples~1,random=~1|treef)

# Examine the result using the summary() command
# what is the intercept of the fixed effect equal to ?
# what do the StdDev:    (Intercept) Residual refer to
# Use the fitted() command to see what the predictions are for each tree
# Why are these not equal to the means of each tree?


# run this command to modify your data so that weight of each apple depends 
# on its height in the tree (don't worry too much if you don't understand it)

allApplesh<-rep(0,120)
height<-runif(120,1,3)
for (i in c('Tree1','Tree2','Tree3')) {
				residV<-allApples[treef==i]-mean(allApples[treef==i])
				treeM<-mean(allApples[treef==i])
				heightEffect<-height[treef==i]*runif(1,20,40)+runif(1,0,5)
				allApplesh[treef==i]<-abs(treeM+heightEffect)/35+residV
				}

 # enable lattice plots
 library(lattice)

# Use xyplot to examine the data
xyplot(allApplesh~height|treef)

# Fit a different fixed effect of height for each tree, plus the random effects
mod6<-lme(allApplesh~height+height/treef,random=~1|treef)
mod7<-lm(allApplesh~height*treef)
xyplot(allApplesh+fitted(mod6)+fitted(mod7)~height|treef)

# Advanced topic: Why do the fitted lines from lm and lme differ from each other
# generate a new modified dataset if your effect is not obvious

##################################################################
# Applying this logic to genetic data

# Generate some genotypes
nIndivs<-20;nLoci=40
genotypes<-matrix(sample(-1:1,nIndivs*nLoci,T),nrow=nIndivs)

# look at the genotype data using the head(command)
locusEffects<-rnorm(nLoci,sd=3)

# Use matrix multiplication to obtain expected phenotype
expectedWT<-100+genotypes %*% locusEffects

# examine expectedWT

# add the environmental variation to get the phenotype
phenotypeWT<-expectedWT+rnorm(nIndivs,sd=2)

mod10<-lme(phenotypeWT~1,random=~genotypes|1)



######################################################
########### Exercise ends ############################
######################################################

