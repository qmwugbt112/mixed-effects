# if necessary install rrBLUP
install.packages(rrBLUP,dependencies=T)

# load the library
library(rrBLUP)

# Load HopperDat.csv
Hdat<-read.table(file.choose(),header=T,sep=',')

Hdat[1:10,1:10]

Gmat<-as.matrix(Hdat[,5:ncol(Hdat)])
dim(Gmat)

########## Richard's quick and dirty imputation###################

# Calculate the mean of each col, ignoring NAs
colmns<-colMeans(Gmat,na.rm=T)
# Find out where the NAs are 
naLocn<-which(is.na(Gmat))
# Convert each location to a col number
colNos<-(naLocn+nrow(Gmat)-1)%/%nrow(Gmat)
# Fill in missing values with corresponding mean
Gmat[naLocn]<-colmns[colNos]
##################################################################

#Check the NAs have been replaced by the column mean
Gmat[1:10,1:10]


Weight<-Hdat$Weight
# Find row numbers for lines with missing values
missing.weight<-which(is.na(Weight))

#define the training and test populations
#training-60% validation-40%
nLines<-length(Weight)
#make a random selection of 60% of the grasshoppers
train= sample(1:nLines,(.6)*(nLines))

# remove lines with missing values from training set
train<-setdiff(train,missing.weight)

# get the numbers of the test lines (those not in train)
test<-setdiff(1:nLines,train)

# extract subset of lines and genotypes for training
trainWeight<-Weight[train]
trainGenot<-Gmat[train,]

# Do rrBLUP, see http://www.inside-r.org/packages/cran/rrBLUP/docs/mixed.solve
library(rrBLUP)
GCA_BLUP<-mixed.solve(trainWeight,Z=trainGenot,K = NULL, SE = FALSE, return.Hinv=FALSE)

# Show that estimated effects matrix-multiplied by genotype is the prediction



est1<-as.vector(trainGenot%*%GCA_BLUP$u)


plot(est1,trainWeight)


mod1<-lm(trainWeight~0+est1)
abline(mod1)
anova(mod1)

testWeight<-Weight[test]
testGenot<-Gmat[test,]
est2<-as.vector(testGenot%*%GCA_BLUP$u)

plot(est2,testWeight)


mod2<-lm(testWeight~0+est2)
abline(mod2)
anova(mod2)