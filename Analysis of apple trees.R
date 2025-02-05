

# For the initial exercise use the following code 
# to generate your own unique dataset for tree1, 2 & 3
Na <- 20
apples1 <-rnorm(Na,rnorm(1,20,3),rnorm(1,5))
apples2<-rnorm(Na,rnorm(1,40,3),rnorm(1,15))
apples3<-rnorm(Na,rnorm(1,80,3),rnorm(1,10))

# Set up some graphical parameters
par(mfrow=c(3,1))
xvals<-0:100

# Calculate the likelihood curves (normalized)
likeMean1<-dnorm(xvals,mean=mean(apples1),sd=sd(apples1)/Na^.5)*100
likeMean2<-dnorm(xvals,mean=mean(apples2),sd=sd(apples2)/Na^.5)*100
likeMean3<-dnorm(xvals,mean=mean(apples3),sd=sd(apples3)/Na^.5)*100

# Plot the raw data
hist(	apples1,xlim=c(0,100),
		ylim=c(0,max(likeMean1)),breaks=seq(0,150,5), border='purple',
		main='Weights of apples on Tree 1')
		
lines(	xvals,
		dnorm(xvals,mean=mean(apples1),sd=sd(apples1))*100,
		col='purple')

hist(	apples2,xlim=c(0,100),
		ylim=c(0,max(likeMean2)),breaks=seq(0,150,5), border='red',
		main='Weights of apples on Tree 2')

lines(	xvals,
		dnorm(xvals,mean=mean(apples2),sd=sd(apples2))*100,
		col='red')

hist(	apples3,xlim=c(0,100),
		ylim=c(0,max(likeMean3)),breaks=seq(0,150,5), border='blue',
		main='Weights of apples on Tree 3')
		
lines(	xvals,dnorm(xvals,
		mean=mean(apples3),sd=sd(apples3))*100,
		col='blue')

# use the mean() and sd() commands to obtain the mean and SD for each tree
# compare them with your neighbour

# use lm() command to find the mean & residual variation for each of apples1-3
# e.g. mod1<-lm(apples1~1); summary(mod1)
# Make sure you understand what the residual standard error is
# make sure you understand what the intercept and 'Std. Error' (of the intercept) is, and why it is different
# How do these values relate to the mean and sd you calculated?

# plot out the likelihood curves, how do they relate to the Std. Error
par(mfrow=c(3,1))
hist(apples1,xlim=c(0,100),ylim=c(0,max(likeMean1)),breaks=seq(0,150,5), border='purple')
lines(xvals,dnorm(xvals,mean=mean(apples1),sd=sd(apples1))*100,col='purple')
lines(xvals,(likeMean1))

hist(apples2,xlim=c(0,100),ylim=c(0,max(likeMean2)),breaks=seq(0,150,5), border='red')
lines(xvals,dnorm(xvals,mean=mean(apples2),sd=sd(apples2))*100,col='red')
lines(xvals,(likeMean2))

hist(apples3,xlim=c(0,100),ylim=c(0,max(likeMean3)),breaks=seq(0,150,5), border='blue')
lines(xvals,dnorm(xvals,mean=mean(apples3),sd=sd(apples3))*100,col='blue')
lines(xvals,(likeMean3))

# Now bind your dataset into one long vector, and create a factor to distinguish the data from each tree
allApples<-c(apples1,apples2,apples3)
treef<-factor(rep(c('Tree1','Tree2','Tree3'),each=Na))
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


# See how these new fitted values compare to the raw mean values

par(mfrow=c(3,1))
hist(apples1,xlim=c(0,100),ylim=c(0,max(likeMean1)),breaks=seq(0,150,5), border='purple')
lines(xvals,dnorm(xvals,mean=mean(apples1),sd=sd(apples1))*100,col='purple')
lines(xvals,(likeMean1))
abline(v=fitted(mod5)[treef=='Tree1'],col='red')

hist(apples2,xlim=c(0,100),ylim=c(0,max(likeMean2)),breaks=seq(0,150,5), border='red')
lines(xvals,dnorm(xvals,mean=mean(apples2),sd=sd(apples2))*100,col='red')
lines(xvals,(likeMean2))
abline(v=fitted(mod5)[treef=='Tree2'],col='red')

hist(apples3,xlim=c(0,100),ylim=c(0,max(likeMean3)),breaks=seq(0,150,5), border='blue')
lines(xvals,dnorm(xvals,mean=mean(apples3),sd=sd(apples3))*100,col='blue')
lines(xvals,(likeMean3))
abline(v=fitted(mod5)[treef=='Tree3'],col='red')

# Are theseequal to the means of each tree?
# Use the plot(fitted()) command to see what the predictions are for each tree
# Use unique(fitted()) combination to pull out the values
# compare them to your previous estimates



# run this command to modify your data so that weight of each apple depends 
# on its height in the tree 

numTrees<-4			# Number of trees (each has 30 apples)

allApplesh<-rnorm(numTrees*30,40,3)	# give all apples a random element to their weight
height<-runif(numTrees*30,1,3)		# give all apples a randomly allocated height on the tree
treef<-factor(rep(paste('Tree',1:numTrees),each=30))	# A factor to identify the trees (each with 30 apples)

treeIntercepts<-rnorm(numTrees,0,9)	# for each tree choose a value for the intercept 
treeSlopes<-rnorm(numTrees,0,3)		# and slope for the regression of apple weight on height

# Calculate the effect of height in the tree on each apple in each tree and plot
extraWt<-height*treeSlopes[treef]+treeIntercepts[treef]
plot(height,extraWt)

# add the height effect and underlying variation together
allApplesh<-allApplesh+extraWt

 # enable lattice plots
 library(lattice)

# Use xyplot to examine the data
xyplot(allApplesh~height|treef)

# Fit a different fixed effect of height for each tree, plus the random effects
mod6<-lme(allApplesh~height+height/treef,random=~1|treef)
mod7<-lm(allApplesh~height*treef)
mod8<-lme(allApplesh~height,random=~height|treef)
xyplot(allApplesh+fitted(mod7)+fitted(mod8)~height|treef)

# Advanced topic: Why do the fitted lines from lm and lme differ from each other
# generate a new modified dataset if your effect is not obvious

##################################################################
# Applying this logic to genetic data

# Generate some genotypes
nIndivs<-30;nLoci=40
genotypes<-matrix(sample(-1:1,nIndivs*nLoci,T),nrow=nIndivs)

# Give the rows and cols of the matrix appropriate names
gnames<-list()
gnames[[1]]<-paste("Ind", 1:nIndivs)
gnames[[2]]<-paste("Loc", 1:nLoci)
dimnames(genotypes)<-gnames

# print out the top left of the matrix
genotypes[1:10,1:7]

# look at the genotype data using the head(command)

# now generate the effects of each locus
locusEffects<-rnorm(nLoci,sd=3)

# Look at the effect size for each locus by printing the values

# Use matrix multiplication to obtain expected phenotype
expectedWT<-100+genotypes %*% locusEffects

# examine expectedWT

# add the environmental variation to get the phenotype
phenotypeWT<-expectedWT+rnorm(nIndivs,sd=2)

# use the plot function to compare expected and observed phenotypes

# see that lm doesnt work
mod10<-lm(phenotypeWT~genotypes)
summary(mod10)

# Why does it not work?

## We can try a special version of mixed effects modelling to analyse this relationship
install.packages('rrBLUP',dependencies=T)
# load the library
library(rrBLUP)

BLUP<-mixed.solve(phenotypeWT,Z=genotypes,
			K = NULL, SE = FALSE, return.Hinv=FALSE)

# Show that BLUP has successfully obtained an (over) accurate estimate of phenotype
# by taking the BLUP estimates of the genotypic effects 'u' and multiplying them by 
# genotye

est<-as.vector(genotypes%*%BLUP$u)
plot(est,phenotypeWT)

# Actually they have over-fitted the data as you can see by plotting these values
# against the true genotypic effects

plot(expectedWT,est)

# Compare the estimated genotypic effects, BLUP$u 
# with the real 'locusEffects' , using plot()


#### Super advanced stuff######
# you can show this in real life by using half the data to estimate u
# then using it to predict the other half

testWT<-phenotypeWT[16:30]
trainWT<-phenotypeWT[1:15]
testG<-genotypes[16:30,]
trainG<-genotypes[1:15,]

BLUP1<-mixed.solve(trainWT,Z=trainG,K = NULL, SE = FALSE, return.Hinv=FALSE)

# Show that estimated effects matrix-multiplied by genotype is the prediction

est1<-as.vector(trainG%*%BLUP1$u)
plot(est1,trainWT)

# compare the prediction vs the true values
plot(est1,expectedWT[1:15])

# now the harder task, use the estimates to predict the phenotypes that were 
# excluded from the analysis

est2<-as.vector(testG%*%BLUP1$u)

plot(est2,testWT)

# Advanced: why is the 2nd correlation poorer
# What happens if you add more environmental variation to the phenotype?

######################################################
########### Exercise ends ############################
######################################################

