# if necessary install rrBLUP
<<<<<<< HEAD
# install.packages('rrBLUP',dependencies=T)
=======
install.packages('rrBLUP',dependencies=T)
>>>>>>> 2e6386641e9327231b3d0acf04080ee36c9a3678

# load the library
library(rrBLUP)

# Load HopperDat.csv
Hdat<-read.table(file.choose(),header=T,sep=',')

# Have a look
Hdat[1:10,1:10]

# Extract a matrix of the SNP data
Gmat<-as.matrix(Hdat[,5:ncol(Hdat)])
dim(Gmat)

Gmat[1:10,1:10]

########## Richard's quick and dirty imputation###################
######### Replace missing values with the column mean #########
Gmat[is.na(Gmat)]<-rep(colMeans(Gmat,na.rm=T),colSums(is.na(Gmat)))

##################################################################



#Check the NAs have been replaced by the column mean
Gmat[1:10,1:10]

# Get the data on the penotypic trait to be analysed (Weight in this case)
Weight<-Hdat$Weight

############ This chunk of code decides which individuals #######
############ to leave out of the training dataset         #######
																#
# Find row numbers for lines with missing values 					
# They can't be used for training 			
missing.weight<-which(is.na(Weight))							
																
#define the training and test populations						
#training-60% validation-40%


#make a random selection of 60% of the grasshoppers
nLines<-length(Weight)
train= sample(1:nLines,(.6)*(nLines))

# remove lines with missing values from training set
train<-setdiff(train,missing.weight)

# get the numbers of the test lines (those not in train)
test<-setdiff(1:nLines,train)
																	#
#####################################################################
#####################################################################

# extract subset of lines and genotypes for training
trainWeight<-Weight[train]
trainGenot<-Gmat[train,]

# Do rrBLUP, see 
# http://www.inside-r.org/packages/cran/rrBLUP/docs/mixed.solve
library(rrBLUP)
GCA_BLUP<-mixed.solve(trainWeight,Z=trainGenot,K = NULL, SE = FALSE, return.Hinv=FALSE)

# Show that estimated effects matrix-multiplied by genotype is the prediction

est1<-as.vector(trainGenot%*%GCA_BLUP$u)
plot(est1,trainWeight)


# Find out how good the correlation is
mod1<-lm(trainWeight~0+est1)
abline(mod1)
anova(mod1)


# Now repeat the process for the test dataset 
# (individuals not used to train the algorithm)
testWeight<-Weight[test]
testGenot<-Gmat[test,]
est2<-as.vector(testGenot%*%GCA_BLUP$u)

plot(est2,testWeight)

# How good is this correlation?
mod2<-lm(testWeight~0+est2)
abline(mod2)
anova(mod2)

# Now estimate breeding values, and assess them for fecundity and heamaglutanin levels