# Generate some genotypes
# nLarge loci have large effects, nSmall have small and the rest have zero
nIndivs<-30;nLoci=40;nLarge=10;nSmall<-20 

genotypes<-matrix(sample(-1:1,nIndivs*nLoci,T),nrow=nIndivs)

# Give the rows and cols of the matrix appropriate names
gnames<-list()
gnames[[1]]<-paste("Ind", 1:nIndivs)
gnames[[2]]<-paste("Loc", 1:nLoci)
dimnames(genotypes)<-gnames

genomic_effects<-rep(0,nLoci)

small_locations<-sample(1:nLoci,nSmall)
large_locations<-sample(setdiff(1:nLoci,small_locations),nLarge)
large_locations 
genomic_effects[small_locations]<-rnorm(nSmall)
hist(genomic_effects)
genomic_effects[large_locations]<-runif(nLarge,3,10)*sample(c(-1,1),nLarge,T)


genotypes[1:7,1:7]

expectedP<-100+genotypes%*%genomic_effects

phenoT<-expectedP+rnorm(nIndivs,sd=2)

library(rrBLUP)
BLUP<-mixed.solve(phenoT,Z=genotypes,K = NULL, SE = FALSE, return.Hinv=FALSE)

# Show that BLUP has successfully obtained an (over) accurate estimate of phenotype
# by taking the BLUP estimates of the genotypic effects 'u' and multiplying them by
# genotye
est<-as.vector(genotypes%*%BLUP$u)
plot(est,phenoT)

# Actually they have over-fitted the data as you can see by plotting these values
# against the true genotypic effects
plot(expectedP,est)


# Plot estiamtes of effect size
plot(genomic_effects,BLUP$u)
points(genomic_effects[large_locations],BLUP$u[large_locations],col='red')
points(genomic_effects[small_locations],BLUP$u[small_locations],col='blue')


abline(0,1)




