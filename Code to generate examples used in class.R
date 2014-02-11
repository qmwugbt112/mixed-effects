#########################################################################
####### Code for generating the figures used in the class        ########
#########################################################################



# Mixed effects model illustration

Ntrees=3;treeSD=5
Napples=4;appleSD=4

# We have an orchard of Ntrees  with 4 apples from each
treeID<-factor(rep(1:Ntrees,each=Napples))

# The performance of each tree is a random effect
treeRE<-rep(rnorm(Ntrees,100,sd=treeSD),each=Napples)

# The apple weights are normally distributed
appleWT<-treeRE+rnorm(Ntrees*Napples,sd=appleSD)

minWT<-min(appleWT);maxWT<-max(appleWT);rangeWT<-maxWT-minWT
minWT<-minWT-rangeWT/10;maxWT<-maxWT+rangeWT/10

resln<-1000
xvals<-seq(minWT,maxWT,,resln)

normD<-matrix(0,nrow=Ntrees,ncol=resln)			# estimated prob density for apple weight for each tree
meanL<-matrix(0,nrow=Ntrees,ncol=resln)			# estimate for each mean pooled SD
meanLv<-matrix(0,nrow=Ntrees,ncol=resln)		# estimate for each mean SD from each sample

# create a function to return the likelihood of a set of values
# given a particular mean and variance
lset<-function(x,mean,sd) prod(dnorm(x,mean,sd))
lsetV<-Vectorize(lset,'mean')

# use lm to extract pooled SD estimate
m2<-lm(appleWT~treeID)
tSD<-anova(m2)$'Mean Sq'[2]^.5

for (i in 1:Ntrees){
	normD[i,]<-dnorm(xvals,
					mean(appleWT[treeID==i]),
					var(appleWT[treeID==i])^.5)
	meanL[i,]<-lsetV(	appleWT[treeID==i],
					xvals,
					tSD)
	meanLv[i,]<-lsetV(	appleWT[treeID==i],
					xvals,
					var(appleWT[treeID==i])^.5)
	}


# Calculate distributions to plot

normD<-normD/rowSums(normD)
meanL<-meanL/rowSums(meanL)
meanLv<-meanLv/rowSums(meanLv)

# Calculate models to describe the distribution
Modle1<-lm(appleWT~treeID)
Model2<-lme(appleWT~1,random= ~1|treeID)



plot(	xvals,
		normD[1,],col=2,
		type='l',
		ylim=c(0,max(meanL)*1.1),
		xlab='Apple Weight',
		ylab='Probability Density')
for (i in 2:Ntrees) lines(xvals,normD[i,],col=1+i)
points(appleWT,rep(0,length(appleWT)),pch=as.numeric(treeID)+4,col=as.numeric(treeID)+1,xlim=c(minWT,maxWT),
	)

for (i in 1:Ntrees) lines(xvals,meanL[i,],col=1+i,lty=2)

# Break it down
plot(	xvals,
		normD[1,],col=2,
		type='l',
		ylim=c(0,max(meanL)*1.1),
		xlab='Apple Weight',
		ylab='Probability Density')
points(appleWT[treeID==1],rep(0,length(appleWT[treeID==1])))

dsum<-sum(dnorm(	xvals,
				mean=100,
				sd=var(appleWT[treeID==1])^.5))
abline(v=100)

for (i in seq(80,120,by=.2)) abline(v=i)

lines(	xvals,
		dnorm(	xvals,
				mean=100,
				sd=var(appleWT[treeID==1])^.5)/dsum)

for (i in (appleWT[treeID==1])){
	lines(c(i,i),c(0,dnorm(i,100,var(appleWT[treeID==1])^.5)/dsum))
	}

plot(	xvals,
		normD[1,],col=2,
		type='l',
		ylim=c(0,max(meanLv)*1.1),
		xlab='Apple Weight',
		ylab='Probability Density')
for (i in 2:Ntrees) lines(xvals,normD[i,],col=1+i)
points(appleWT,rep(0,length(appleWT)),pch=as.numeric(treeID)+4,col=as.numeric(treeID)+1,xlim=c(minWT,maxWT),
	)

for (i in 1:Ntrees) lines(xvals,meanLv[i,],col=1+i,lty=3)

plot(	xvals,
		normD[1,],col=2,
		type='l',
		ylim=c(0,max(meanLv)*1.1),
		xlab='Apple Weight',
		ylab='Probability Density')

points(appleWT,rep(0,length(appleWT)),pch=as.numeric(treeID)+4,col=as.numeric(treeID)+1,xlim=c(minWT,maxWT),
	)

for (i in 1:Ntrees) lines(xvals,meanLv[i,],col=1+i)	

plot(xvals,normD[1,],col=2,type='n',ylim=c(0,max(meanL)*1.1),lty=2,xlab='Apple Weight',ylab='Density')
for (i in 1:Ntrees) lines(xvals,meanL[i,],col=1+i,lty=2)

points(appleWT,rep(0,length(appleWT)),pch=as.numeric(treeID)+4,col=as.numeric(treeID)+1,xlim=c(minWT,maxWT))

Tmeans<-vector(length=Ntrees)
for (i in 1:Ntrees) Tmeans[i]<-mean(appleWT[treeID==i])

REmean<-fixted.effects(Model2)
Model2
REsd<-3.862575

REd<-dnorm(xvals,REmean,REsd)
REd<-REd/sum(REd)
lines(xvals,REd,lty=3)

Blupd<-meanL*rep(REd,each=Ntrees)
Blupd<-Blupd/rowSums(Blupd)
for (i in 1:Ntrees) lines(xvals,Blupd[i,],col=1+i,lty=3)

for (i in 1:Ntrees) abline(v=unlist(coef(Model2))[i],col=i+1,lty=3)

