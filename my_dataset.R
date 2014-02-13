# Script to generate data in which an orchard has 
# a sample of apples weighed in each year from each tree.
# All trees are fertilized each year, but the amount of
# fertilizer per tree varies slightly among years

Ntrees<-6
Nyears<-10
Tmeans<-rnorm(Ntrees,100,20)				# tree performance mean 100, sd 20
Ymeans<-rnorm(Nyears,0,20)	  				# difference over years mean 0, sd 20
Ssize<-4									# sample size of apples from each tree
FeffectM<-.5								# weight increase per unif of fertiliser each year
FeffectC<-10								# intercept of fertilizer effect

TreeID<-factor(
		rep(1:Ntrees,Nyears,each=Ssize))  	# unique number for each tree

YearID<-factor(rep(2000+(1:Nyears),each=Ntrees*Ssize)) 	# year
Fdose<-vector(length=length(YearID))
	for (i in levels(YearID)) Fdose[YearID==i]<-rnorm(1,30,5)			# dose of fertiliser on each year
	
appleWT<-vector(length=Nyears*Ntrees*Ssize)			# vector to store apple weights
names(Ymeans)<-levels(YearID)						# associate each mean with the year name
names(Tmeans)<-levels(TreeID)						# associate each mean with a tree name
#Add in the different random * fixed effects
for (i in levels(YearID)) appleWT[YearID==i]<-Ymeans[i]
for (i in levels(TreeID)) {
		appleWT[TreeID==i]<-appleWT[TreeID==i]+Tmeans[i]
		}

# add fixed fertizler effect		
for (i in 1:Nyears) appleWT<-appleWT+Fdose*FeffectM+FeffectC
# add error around tree & variety effects
appleWT<-(appleWT+rnorm(Nyears*Ntrees*Ssize,sd=30))/10

MyData<-data.frame(YearID,TreeID,Fdose,appleWT)
