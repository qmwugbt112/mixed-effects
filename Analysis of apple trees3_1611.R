######################################
######################################
# Analysis of the apple trees
######################################
# Version Nov 2016 ###################


######################################
# Generating data
######################################
# In the real world, you would here load the data you collected.
#  For the sake of this exercise, you will generate you own unique dataset,
#  representing the weights of apples on trees 1, 2 & 3. That means you will
#  be using different data than your neighbours!

N.apples <- 20 # Number of apples per tree

# Create a function producing random apple weights
make.apples <- function(m, s){
  apples <- rnorm(N.apples, rnorm(1, m, 3), rnorm(1, s)) # choose some normally distributed random weights
  apples[apples > 149] <- 149; apples[apples < 1] <- 1   # get rid of apples that are too big or small	
  apples
}

# Use this function to create randomly-weighted apples for three trees
apples1 <- make.apples(20, 7) 
apples2 <- make.apples(40, 20)
apples3 <- make.apples(70, 30)

########################################
# Visualise data
########################################

# Set up some graphical parameters
par(mfrow = c(3, 1))
x.vals <- seq(0, 100, 0.1)

# Histograms and curves
hist(apples1, xlim = c(0, 100), #ylim = c(0, max(likeMean1)),
     breaks = seq(0, 150, 5), border = 'purple')
lines(x.vals, dnorm(x.vals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')

hist(apples2, xlim = c(0,100), #ylim = c(0, max(likeMean2)),
     breaks = seq(0, 150, 5), border = 'red')
lines(x.vals, dnorm(x.vals, mean = mean(apples2), sd = sd(apples2)) * 100,
      col = 'red')

hist(apples3, xlim = c(0,100), #ylim = c(0, max(likeMean3)),
     breaks = seq(0, 150, 5), border = 'blue')
lines(x.vals, dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100,
      col = 'blue')
par(mfrow = c(1, 1))
# What do the histograms represent? What are the curves?
#
#
#

# Use the mean() and sd() commands to obtain the mean and SD for each tree.
#
#
#


# Compare them with your neighbour. Remember they got different data.
# Who's got the better trees?
# Use the lm() command to find the mean & residual variation for each of
#  apples1-3. E.g. mod1<-lm(apples1~1); summary(mod1)




# Make sure you understand what the residual standard error is.
#
#
# Make sure you understand what the intercept and 'Std. Error' (of the
#  intercept) is, and why it is different.
#
#
# How do these values relate to the mean and sd you calculated?
#
#
#

# Create a function to calculate the likelihood curves (normalized)
#  for each tree's mean weight
likelihood.of.mean.weight <- function(tree){
  dnorm(x.vals, mean = mean(tree), sd = sd(tree) / N.apples ^ .5) * 100
}

# Compute likelihood curves
likeMean1 <- likelihood.of.mean.weight(apples1)
likeMean2 <- likelihood.of.mean.weight(apples2)
likeMean3 <- likelihood.of.mean.weight(apples3)

# Plot out the likelihood curves. How do they relate to the Std. Error?
par(mfrow = c(3, 1))
hist(apples1, xlim = c(0, 100), ylim = c(0, max(likeMean1)),
     breaks = seq(0, 150, 5), border = 'purple')
lines(x.vals, dnorm(x.vals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')
lines(x.vals, likeMean1)

hist(apples2,xlim = c(0, 100), ylim = c(0, max(likeMean2)), breaks = seq(0, 150, 5), border = 'red')
lines(x.vals, dnorm(x.vals, mean = mean(apples2), sd = sd(apples2)) * 100, col = 'red')
lines(x.vals, likeMean2)

hist(apples3, xlim = c(0, 100), ylim = c(0, max(likeMean3)), breaks = seq(0, 150, 5), border = 'blue')
lines(x.vals, dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100, col = 'blue')
lines(x.vals, likeMean3)
par(mfrow = c(1, 1))

# Now bind all apple weights into one long vector and
#  create a factor to distinguish the data from each tree.
all.apples <- c(apples1, apples2, apples3)
tree.factor <- factor(rep(c('Tree1', 'Tree2', 'Tree3'), each = N.apples))
# Print them out to have a look by typing their names 'all.apples' and 'tree.factor'
# Try a boxplot boxplot(all.apples ~ tree.factor)


#################################
# Compare trees
#################################

# Use mod4<-lm(all.apples~tree.factor) to fit all 3 tree means.

# Use the summary() command to look at the results.

# Which mean is the intercept equal to, why?
# Why are the values relating to the other two means not the same as you found before?
# Why are the standard errors for these two values the same, when they have different variances?

# Load the library allowing you to run mixed effects models.
library(nlme)

# Fit a simple nlme
mod5 <- lme(all.apples ~ 1, random = ~ 1 | tree.factor)

# Examine the result using the summary() command

# what is the intercept of the fixed effect equal to ?
# what do the StdDev:    (Intercept) Residual refer to
# Use the fitted() command to see what the predictions are for each tree
fitted(mod5)

# See how these new fitted values compare to the raw mean values
par(mfrow = c(3, 1))
hist(apples1, xlim = c(0, 100), ylim = c(0, max(likeMean1)),
     breaks = seq(0, 150, 5), border = 'purple')
lines(x.vals, dnorm(x.vals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')
lines(x.vals, likeMean1)
abline(v = fitted(mod5)[tree.factor == 'Tree1'], col = 'red')
abline(v = mean(apples1), col = 'blue')

hist(apples2, xlim=c(0, 100), ylim = c(0, max(likeMean2)),
     breaks = seq(0, 150, 5), border = 'red')
lines(x.vals, dnorm(x.vals, mean = mean(apples2), sd = sd(apples2)) * 100,
      col = 'red')
lines(x.vals, likeMean2)
abline(v = fitted(mod5)[tree.factor == 'Tree2'], col = 'red')
abline(v = mean(apples2), col = 'blue')

hist(apples3, xlim= c (0, 100), ylim = c(0, max(likeMean3)),
     breaks = seq(0, 150, 5), border = 'blue')
lines(x.vals, dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100,
      col = 'blue')
lines(x.vals, likeMean3)
abline(v = fitted(mod5)[tree.factor == 'Tree3'], col = 'red')
abline(v = mean(apples3), col = 'blue')
par(mfrow = c(1, 1))
# Where are these fitted values (red) compared to the means of each tree (blue line)?
# Use unique(round(fitted(mod5),2)) to see the different values.
# Compare them to your previous estimates unique(round(fitted(mod4),2)).
# Why are they different? How can you explain the direction in which they are different?


########################################
# Adding some additional variance
########################################
# The commands below will modify your data so that weight of each apple depends 
# on its height in the tree (don't worry too much if you don't understand it
# but do make sure you understand the analysis)

# Create a vector for apple weights depending on height
all.apples.height <- rep(0, 3 * N.apples)
# Allocate random heights to all apples
height <- runif(3 * N.apples, 1, 3)

# Allocate a different slope (for the weight effect) for each tree
tree.slopes <- rnorm(3, mean=c(1, 10, 20), sd = c(1, 3, 4))
names(tree.slopes) <- c('Tree1', 'Tree2', 'Tree3')

# Allocate a different intercept for each tree
tree.intercepts <- rnorm(3, mean = 80, sd = 10)
names(tree.intercepts) <- c('Tree1', 'Tree2', 'Tree3')

# Modify the apple weights to have the corresponding effect of height
for(i in c('Tree1', 'Tree2', 'Tree3')){
  resid.var <- 4 * (all.apples.height[tree.factor == i] - mean(all.apples[tree.factor == i]))
  height.effect <- height[tree.factor == i] * tree.slopes[i] + tree.intercepts[i] - min(resid.var)
  all.apples.height[tree.factor == i] <- height.effect+ resid.var
}

# A function for plotting weight of apples against their height in the tree
plot.tree <- function(tree.no){
  plot(all.apples.height[tree.factor == tree.no] ~ height[tree.factor == tree.no],
       main = tree.no, ylab = "Height", xlab = "Apple weight",
       ylim = c(min(all.apples.height), max(all.apples.height)),
       xlim = c(min(height), max(height)))
}

# Use the function to visualise our modified data
par(mfrow = c(3, 1))
plot.tree("Tree1")
plot.tree("Tree2")
plot.tree("Tree3")
par(mfrow = c(1, 1))


#################################################
# Mixed effects and individual slopes
#################################################

# Make sure you understand from this point on 

# Fit height as a fixed effect for each tree, plus random effect for the
#  deviation of slope & intercept for each tree. You may have to try this a few
#  times, as the algorithm can fail to solve the problem on some attempts.
mod6 <- lme(all.apples.height ~ height, random = ~ 1 | tree.factor)

# For comparison, fit a separate linear regression for each tree using lm
mod7 <- lm(all.apples.height ~ height * tree.factor)

# Plot out the weights vs height on each tree and the fitted lines
#  This uses a for-loop running the plot-tree and points commands three times,
#  one for each tree.
#  green lines - fixed effect (lm)
#  maroon lines - random effect (lme)
par(mfrow = c(3, 1))
for(tree in c("Tree1", "Tree2", "Tree3")){
  plot.tree(tree)
  points(fitted(mod6)[tree.factor == tree] ~ height[tree.factor == tree], lwd = 2, type = "l", col = "maroon")
  points(fitted(mod7)[tree.factor == tree] ~ height[tree.factor == tree], lwd = 2, type = "l", col = "darkgreen")
}
par(mfrow = c(1, 1))

# Advanced topic: Why do the fitted lines from lm and lme differ from each other?
# Generate a new modified dataset if your effect is not obvious.



#########################################################
#########################################################
# Applying this logic to genetic data
#########################################################
# Generate some genotypes
nIndivs <- 30; nLoci <- 40
genotypes <- matrix(sample(-1:1, nIndivs * nLoci, T), nrow = nIndivs)

# Give the rows and cols of the matrix appropriate names
dimnames(genotypes) <- list(paste0("Ind", 1:nIndivs), paste0("Loc", 1:nLoci))

# print out the top left of the matrix
genotypes[1:10, 1:7]

# look at the genotype data using the head(command)
locusEffects <- rnorm(nLoci, sd = 3)

# Use matrix multiplication to obtain expected phenotype
expectedWT <- 100 + genotypes %*% locusEffects

# Examine expectedWT:



# add the environmental variation to get the phenotype
phenotypeWT <- expectedWT + rnorm(nIndivs, sd = 2)

# see that lm doesnt work
mod10 <- lm(phenotypeWT ~ genotypes)
summary(mod10)

# Why does it not work?

# We can try a special version of mixed effects modelling to analyse this relationship
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

est2 <- as.vector(testG %*% BLUP1$u)

plot(est2, testWT)

# Advanced: why is the 2nd correlation poorer
# What happens if you add more environmental variation to the phenotype?

######################################################
########### Exercise ends ############################
######################################################

