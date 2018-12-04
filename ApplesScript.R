#------------------------------------------------------------------------
#Analysis of the apple trees
# ------------------------------------------------------------------------
# Version Dec 2018 #
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Preparing R for the practical
# ------------------------------------------------------------------------
# You do not have to understand the code accessed by the following
# source command.
# (it creates some functions you will use to generate and plot data.)
# R nerds may want to dig into it by looking at the file. 
# HOWEVER ensure you DO understand ALL the subsequent code 
# after the banner 'Now the practical starts' !!
# and can answer all the questions

source(paste("https://raw.githubusercontent.com/",
             "qmwugbt112/mixed-effects/master/",
             "OrchardUtilities.R",
             sep = "")
)

# If that does not work try 
# Then copy and paste these two lines of code
# if (!library(devtools,logical.return=T)){
# install.packages('devtools')
# library(devtools)}


# ------------------------------------------------------------------------
# 		Now the practical starts
# ------------------------------------------------------------------------
# In the real world, you would at this point load the data you collected.
# For the sake of this exercise, you will generate you own unique dataset,
# representing the weights of apples on trees 1, 2 & 3. That means you will
# be using different data from your neighbours!


# Grow your own orchard with your own unique data
# (create a data frame with apple weights)
my.orchard <- grow_my_apples()

# Make sure you understand the data
head(my.orchard)

# Plot the data
plot_my_orchard(my.orchard)

# What do the histograms represent? What are the curves?


# Extract the weights of the sample of apples on each tree
apples1 <- pick_my_apples(my.orchard, tree = 1)
apples2 <- pick_my_apples(my.orchard, tree = 2)
apples3 <- pick_my_apples(my.orchard, tree = 3)


# Use the mean() and sd() commands to obtain the mean and SD for each tree.



# Compare them with your neighbour. Remember they got different data.
# Who's got the better trees?
# Use the lm() command to find the mean & residual variation for each of
# apples1-3. E.g. mod1<-lm(apples1~1); summary(mod1)


# 1) Make sure you understand what the residual standard error is.
# 2) Make sure you understand what the intercept and 'Std. Error' (of the
# 		intercept) is, and why it is different.
# 3) How do these values relate to the mean and sd you calculated?


# Plot the likelihood curves (normalized)
# for each tree's mean apple weight
plot_likelihood_means()

# how do these curves relate to the mean and SD of the apple weights in the samples?


#################################
# Using lm to compare trees
#################################

# Use mod4<-lm(Apple.Weights~Tree,data=my.orchard) to fit all 3 tree means.

# Use the summary() command to look at the results.

# Which mean is the intercept equal to, why?
# Why are the values relating to the other two means not the same as you found before?
# Why are the standard errors for these two values the same, when samples have different variances?

# Load the library allowing you to run mixed effects models.
library(lme4)

# Fit a simple nlme
mod5 <- lmer(apple_weights ~ 1 + (1 | tree), data = my.orchard)

# Examine the result using the summary() command

# what is the intercept of the fixed effect equal to ?
# what do the StdDev:  (Intercept) Residual refer to
# Use the coef() command to see what the predictions are for each tree
coef(mod5)

# See how these new fitted values compare to the raw mean values
plot_lme_means(my.orchard, m5 = mod5)

# Where are these fitted values (red) compared to the means of each tree (blue line)?
# Compare them to your previous estimates (found using the fitted() function)
# Why are they different? How can you explain the direction in which they are different?




#################################################
# Mixed effects and individual slopes
#################################################


# Fit height of each apple as a fixed effect for each tree, plus random effect for the
# deviation of slope & intercept for each tree. 

mod6 <- lmer(	apple_weights ~ 1 + ( 1 + height | tree ),
				data = my.orchard
				)


# use the summary() function to find the fitted values from mod6
# in the random effects listing what are the 
# Std.Dev. tree
# StdDev Residual
# What are the fixed effects

# For comparison, fit a separate linear regression for each tree using lm
mod7 <- lm(apple_weights ~ height * tree,
				data=my.orchard)

# Plot out the fitted values for lm and lme
# why do the lines differ (why are slopes and intercepts different)
plot_lme(my.orchard, mod7, mod6)




#########################################################
#########################################################
# Applying this logic to genetic data
#########################################################
# Generate some genotypes
n_indivs <- 30; n_loci <- 40
genotypes <- matrix(sample(-1:1, n_indivs * n_loci, T), nrow = n_indivs)

# Give the rows and cols of the matrix appropriate names
dimnames(genotypes) <- list(paste0("Ind", 1:n_indivs), paste0("Loc", 1:n_loci))

# print out the top left of the matrix
genotypes[1:10, 1:7]

# look at the genotype data using the head(command)
locus_effects <- rnorm(n_loci, sd = 3)

# Use matrix multiplication to obtain expected phenotype
expected_wt <- 100 + genotypes %*% locus_effects

# Examine expected_wt:



# add the environmental variation to get the phenotype
phenotype_wt <- expected_wt + rnorm(n_indivs, sd = 2)

# see that lm doesnt work
mod10 <- lm(phenotype_wt ~ genotypes)
summary(mod10)

# Why does it not work?

# We can try a special version of mixed effects modelling to analyse this relationship
# install.packages('rrBLUP', dependencies = T)
# load the library
library(rrBLUP)

test_wt <- phenotype_wt[16:30]
train_wt <- phenotype_wt[1:15]
testG <- genotypes[16:30, ]
trainG <- genotypes[1:15, ]

blup1 <- mixed.solve(train_wt,
                     Z = trainG,
                     K = NULL,
                     SE = FALSE,
                     return.Hinv = FALSE)

# Show that estimated effects matrix-multiplied by genotype is the prediction

est1 <- as.vector(trainG %*% blup1$u)
plot(est1, train_wt)

# compare the prediction vs the true values
plot(est1, expected_wt[1:15])

# now the harder task, use the estimates to predict the phenotypes that were 
# excluded from the analysis

est2 <- as.vector(testG %*% blup1$u)

plot(est2, test_wt)

# Advanced: why is the 2nd correlation poorer
# What happens if you add more environmental variation to the phenotype?

######################################################
########### Exercise ends ############################
######################################################
