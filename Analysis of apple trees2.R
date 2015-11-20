
# For the initial exercise generate your own unique dataset for trees 1, 2 & 3
# That means you will be using diffenet data than your neighbours.
Nap = 20
apples1 <- rnorm(Nap, rnorm(1, 20, 3), rnorm(1, 7)) # choose some normally distributed random weights
apples1[apples1>149]<-149;apples1[apples1<1]<-1	    # get rid of apples that are too big or small	

apples2 <- rnorm(Nap, rnorm(1, 40, 3), rnorm(1, 20)) # choose some normally distributed random weights
apples2[apples2>149]<-149;apples2[apples2<1]<-1	     # get rid of apples that are too big or small	

apples3 <- rnorm(Nap, rnorm(1, 70, 3), rnorm(1, 30)) # choose some normally distributed random weights
apples3[apples3>149]<-149;apples3[apples3<1]<-1	     # get rid of apples that are too big or small	

# Set up some graphical parameters
par(mfrow = c(3, 1))
xvals <- 0:100

# Calculate the likelihood curves (normalized)
likeMean1 <- dnorm(xvals, mean = mean(apples1), sd = sd(apples1) / Nap ^ .5) * 100
likeMean2 <- dnorm(xvals, mean = mean(apples2), sd = sd(apples2) / Nap ^ .5) * 100
likeMean3 <- dnorm(xvals, mean = mean(apples3), sd = sd(apples3) / Nap ^ .5) * 100

# Plot the raw data
hist(apples1, xlim = c(0, 100), ylim = c(0, max(likeMean1)),
     breaks = seq(0, 150, 5), border = 'purple')
lines(xvals, dnorm(xvals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')

hist(apples2, xlim = c(0,100), ylim = c(0, max(likeMean2)),
     breaks = seq(0, 150, 5), border = 'red')
lines(xvals, dnorm(xvals, mean = mean(apples2), sd = sd(apples2)) * 100,
      col = 'red')

hist(apples3, xlim = c(0,100), ylim = c(0, max(likeMean3)),
     breaks = seq(0, 150, 5), border = 'blue')
lines(xvals, dnorm(xvals, mean = mean(apples3), sd = sd(apples3)) * 100,
      col = 'blue')

# What do the histograms represent? What are the curves?

# Use the mean() and sd() commands to obtain the mean and SD for each tree.
# Compare them with your neighbour. Remember they got different data.
# Who's got the better trees?


# Use the lm() command to find the mean & residual variation for each of
# apples1-3. E.g. mod1<-lm(apples1~1); summary(mod1)


# Make sure you understand what the residual standard error is.
# Make sure you understand what the intercept and
# 'Std. Error' (of the intercept) is, and why it is different.
# How do these values relate to the mean and sd you calculated?

# Plot out the likelihood curves. How do they relate to the Std. Error?
par(mfrow = c(3, 1))
hist(apples1, xlim = c(0, 100), ylim = c(0, max(likeMean1)),
     breaks = seq(0, 150, 5), border = 'purple')
lines(xvals, dnorm(xvals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')
lines(xvals, likeMean1)

hist(apples2,xlim = c(0, 100), ylim = c(0, max(likeMean2)), breaks = seq(0, 150, 5), border = 'red')
lines(xvals, dnorm(xvals, mean = mean(apples2), sd = sd(apples2)) * 100, col = 'red')
lines(xvals, likeMean2)

hist(apples3, xlim = c(0, 100), ylim = c(0, max(likeMean3)), breaks = seq(0, 150, 5), border = 'blue')
lines(xvals, dnorm(xvals, mean = mean(apples3), sd = sd(apples3)) * 100, col = 'blue')
lines(xvals, likeMean3)

# Now bind your dataset into one long vector, and create a factor to distinguish the data from each tree
allApples <- c(apples1, apples2, apples3)
treef <- factor(rep(c('Tree1', 'Tree2', 'Tree3'), each = Nap))
# print them out to have a look by typing their names 'allApples' and 'treef'

# use mod4<-lm(allApples~treef) to fit all 3 tree means.
# use the summary() command to look at the results
# Which mean is the intercept equal to, why?
# Why are the values relating to the other two means not the same as you found before?
# Why are the standard errors for these two values the same, when they have different variances?

# Install the library allowing you to run mixed effects models
library(nlme)

# Fit a simple nlme
mod5<-lme(allApples ~ 1, random = ~ 1 | treef)

# Examine the result using the summary() command
# what is the intercept of the fixed effect equal to ?
# what do the StdDev:    (Intercept) Residual refer to
# Use the plot(fitted()) command to see what the predictions are for each tree

# See how these new fitted values compare to the raw mean values

par(mfrow = c(3, 1))
hist(apples1, xlim = c(0, 100), ylim = c(0, max(likeMean1)),
     breaks = seq(0, 150, 5), border = 'purple')
lines(xvals, dnorm(xvals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')
lines(xvals, likeMean1)
abline(v = fitted(mod5)[treef == 'Tree1'], col = 'red')
abline(v = mean(apples1), col = 'blue')

hist(apples2, xlim=c(0, 100), ylim = c(0, max(likeMean2)),
     breaks = seq(0, 150, 5), border = 'red')
lines(xvals, dnorm(xvals, mean = mean(apples2), sd = sd(apples2)) * 100,
      col = 'red')
lines(xvals, likeMean2)
abline(v = fitted(mod5)[treef == 'Tree2'], col = 'red')
abline(v = mean(apples2), col = 'blue')

hist(apples3, xlim= c (0, 100), ylim = c(0, max(likeMean3)),
     breaks = seq(0, 150, 5), border = 'blue')
lines(xvals, dnorm(xvals, mean = mean(apples3), sd = sd(apples3)) * 100,
      col = 'blue')
lines(xvals, likeMean3)
abline(v = fitted(mod5)[treef == 'Tree3'], col = 'red')
abline(v = mean(apples3), col = 'blue')

# Are these fitted values (red) to the means of each tree (blue line)?
# type out the fitted values i.e. write 'fitted(mod5)', notice many are similar. Why?
# then use unique(round(fitted(mod5),2)) to see the different values (new)
# compare them to your previous estimates unique(round(fitted(mod5),2)).  Why are they different?  
# How can you explain the direction in which they are different?



# run this command to modify your data so that weight of each apple depends 
# on its height in the tree (don't worry too much if you don't understand it
# but do make sure you understand the analysis)

allApplesh <- rep(0, 3*Nap)
# Allocate random heights to all apples
height <- runif(3*Nap, 1, 3)

# Allocate a different slope (for the weight effect) for each tree
slopes<-rnorm(3,mean=c(1,10,20),sd=c(1,3,4))
names(slopes)<-c('Tree1','Tree2','Tree3')

#Allocate a different intercept for each tree
intercepts<-rnorm(3,mean=80,sd=10)
names(intercepts)<-c('Tree1','Tree2','Tree3')

# modify the apple weights to have the correpsonding effect of height
for (i in c('Tree1', 'Tree2', 'Tree3')) {
				residV <- 4*(allApples[treef == i] - mean(allApples[treef == i]))
				heightEffect <- height[treef == i] * slopes[i] + intercepts[i]-min(residV)
				allApplesh[treef == i] <- heightEffect+ residV
				}

# ***********************************************************************
# Make sure you understand from this point on 
 # enable lattice plots
 library(lattice)

# Use xyplot to examine the data
xyplot(allApplesh ~ height | treef)

# Fit the fixed effect of height for each tree, 
# plus random effect for the deviation of slope & intercept for each tree
mod6 <- lme(allApplesh ~ 0+height, random = ~ height | treef)

# Fit a separate linear regression for each tree using lm
mod7 <- lm(allApplesh ~ height * treef)

# Plot out the weights vs height on each tree and the fitted lines
xyplot(allApplesh + fitted(mod6) + fitted(mod7) ~ height | treef, 
	type=c('p','l','l'), 	# Points for data lines for fitted values
	lty=c('','1B','11'),	# Line type absent, solid, dashed
 	distribute.type=T,	
 	xlab=list('Height',cex=1.5),
 	ylab=list('Apple Weight',cex=1.5),
 	key=list(text=list('Fixed (solid) & random effects (dashed) regression lines',cex=1.5))
 	)

# Advanced topic: Why do the fitted lines from lm and lme differ from each other
# generate a new modified dataset if your effect is not obvious

##################################################################
# Applying this logic to genetic data

# Generate some genotypes
nIndivs <- 30; nLoci <- 40
genotypes <- matrix(sample(-1:1, nIndivs * nLoci, T), nrow = nIndivs)

# Give the rows and cols of the matrix appropriate names
gnames <- list()
gnames[[1]] <- paste("Ind", 1:nIndivs)
gnames[[2]] <- paste("Loc", 1:nLoci)
dimnames(genotypes) <- gnames

# print out the top left of the matrix
genotypes[1:10, 1:7]

# look at the genotype data using the head(command)
locusEffects <- rnorm(nLoci, sd = 3)

# Use matrix multiplication to obtain expected phenotype
expectedWT <- 100 + genotypes %*% locusEffects

# examine expectedWT

# add the environmental variation to get the phenotype
phenotypeWT <- expectedWT + rnorm(nIndivs, sd = 2)

# see that lm doesnt work
mod10 <- lm(phenotypeWT ~ genotypes)
summary(mod10)

# Why does it not work?

## We can try a special version of mixed effects modelling to analyse this relationship
# install.packages('rrBLUP', dependencies = T)
# load the library
library(rrBLUP)

testWT <- phenotypeWT[16:30]
trainWT <- phenotypeWT[1:15]
testG <- genotypes[16:30, ]
trainG <- genotypes[1:15, ]

BLUP1 <- mixed.solve(trainWT, Z = trainG, K = NULL, SE = FALSE,
                     return.Hinv = FALSE)

# Show that estimated effects matrix-multiplied by genotype is the prediction

est1 <- as.vector(trainG %*% BLUP1$u)
plot(est1, trainWT)

# compare the prediction vs the true values
plot(est1, expectedWT[1:15])

# now the harder task, use the estimates to predict the phenotypes that were 
# excluded from the analysis

est2 <- as.vector(testG %*% GCA_BLUP$u)

plot(est2, testWT)

# Advanced: why is the 2nd correlation poorer
# What happens if you add more environmental variation to the phenotype?

######################################################
########### Exercise ends ############################
######################################################

