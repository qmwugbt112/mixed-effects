# Parameters and utility functions for the apple practical

N.apples 	<- 30 # Number of apples per tree
N.trees		<- 3  # Number of trees
heights		<- runif(3 * N.apples, 1, 3) # heights of each apple

# Allocate random heights to all apples


growMyApples<-function(	ntrees 			= 3,
							nApples 		= N.apples,
							Height 			= heights,
							treeIntercept	=c(25,30,40),
							treeSDs		=c(25,30,40)){
	slopes<-rnorm(ntrees,1,4)
	Apple.Weights<-rnorm(	ntrees*nApples,
								mean=Height*rep(slopes,each=nApples)+
									rep(treeIntercept,each=nApples),
								sd=rep(treeSDs,each=nApples))
	Tree<-factor(rep(1:ntrees,each=nApples))
	return(data.frame(Tree,Apple.Weights,Height))													
	}

pickMyApples<-function(x,tree){
	return(x$Apple.Weights[x$Tree==tree])
}

########################################
# Visualise data
########################################

# Set up some graphical parameters

plotMyOrchard<-function(x=my.orchard){
	par(mfrow = c(3, 1))
	x.vals <- seq(0, 120, 0.1)
	apples1<-pickMyApples(x,1)
	apples2<-pickMyApples(x,2)
	apples3<-pickMyApples(x,3)
	appleRange<-seq(min(x$Apple.Weights)-0.001,max(x$Apple.Weights)+0.001,,150)
# Histograms and curves
	yvals<-dnorm(x.vals, mean = mean(apples1), sd = sd(apples1)) * 100
	hist(apples1, xlim = c(0, 120), ylim = c(0, max(yvals)),
     breaks = appleRange, border = 'purple')
	lines(x.vals, yvals,
      col = 'purple')
	
	yvals<-dnorm(x.vals, mean = mean(apples2), sd = sd(apples2)) * 100
	hist(apples2, xlim = c(0,120), ylim = c(0, max(yvals)),
     breaks = appleRange, border = 'red')
	lines(x.vals, yvals ,
      col = 'red')
	
	yvals<-dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100
	hist(apples3, xlim = c(0,120), ylim = c(0, max(yvals)),
     breaks  = appleRange, border = 'blue')
	lines(x.vals, yvals,
      col = 'blue')
	par(mfrow = c(1, 1))
}

plotLikelihoodMeans<-function(x=my.orchard){
	likelihood.of.mean.weight <- function(tree){
	  dnorm(x.vals, mean = mean(tree), sd = sd(tree) / N.apples ^ .5) * 100
	}

	x.vals <- seq(0, 120, 0.1)
	appleRange<-seq(min(x$Apple.Weights)-0.001,max(x$Apple.Weights)+0.001,,150)

	# Compute likelihood curves
	apples1<-pickMyApples(x,1)
	apples2<-pickMyApples(x,2)
	apples3<-pickMyApples(x,3)
	likeMean1 <- likelihood.of.mean.weight(apples1)
	likeMean2 <- likelihood.of.mean.weight(apples2)
	likeMean3 <- likelihood.of.mean.weight(apples3)

	# Plot out the likelihood curves. How do they relate to the Std. Error?
	par(mfrow = c(3, 1))
	hist(apples1, xlim = c(0, 120), ylim = c(0, max(likeMean1)),
     breaks = appleRange, border = 'purple')
	lines(x.vals, dnorm(x.vals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')
	lines(x.vals, likeMean1)

	hist(	apples2,xlim = c(0, 120), 
			ylim = c(0, max(likeMean2)), 
			breaks = appleRange, 
			border = 'red')
	lines(	x.vals, 
			dnorm(x.vals, mean = mean(apples2), 
				sd = sd(apples2)) * 100, col = 'red')
	lines(x.vals, likeMean2)

	hist(	apples3, 
			xlim = c(0, 120), 
			ylim = c(0, max(likeMean3)), 
			breaks = appleRange, 
			border = 'blue')
	lines(x.vals, dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100, col = 'blue')
	lines(x.vals, likeMean3)
	par(mfrow = c(1, 1))
}	

plotLme<-function(	x=my.orchard,
			m7=mod7,
			m6=mod6,
		 	napples=N.apples){
	plot(	x$Height,
			x$Apple.Weight,
			pch=as.numeric(x$Tree),
	     		col=rep(c('purple','red','blue'),each=napples),
			xlab='Height',
			ylab='Apple Weight',
			main='Models fitted by lm (black) and lme(magenta)')
	for(i in 1:3) lines(x$Height[x$Tree==i],fitted(m7)[x$Tree==i])
	for(i in 1:3) lines(x$Height[x$Tree==i],fitted(m6)[x$Tree==i],col='magenta')

	
}

plotLmeMeans<-function(x=my.orchard,m5=mod5){
		likelihood.of.mean.weight <- function(tree){
	  dnorm(x.vals, mean = mean(tree), sd = sd(tree) / N.apples ^ .5) * 100
	}
	appleRange<-seq(min(x$Apple.Weights)-0.001,max(x$Apple.Weights)+0.001,,150)
	x.vals <- seq(0, 120, 0.1)
	# Compute likelihood curves
	apples1<-pickMyApples(x,1)
	apples2<-pickMyApples(x,2)
	apples3<-pickMyApples(x,3)
	likeMean1 <- likelihood.of.mean.weight(apples1)
	likeMean2 <- likelihood.of.mean.weight(apples2)
	likeMean3 <- likelihood.of.mean.weight(apples3)

	par(mfrow = c(3, 1))
	hist(apples1, xlim = c(0, 120), ylim = c(0, max(likeMean1)),
     breaks = appleRange, border = 'purple')
	lines(x.vals, dnorm(x.vals, mean = mean(apples1), sd = sd(apples1)) * 100,
      col = 'purple')
	lines(x.vals, likeMean1)
	abline(v = fitted(m5)[x$Tree == 1], col = 'red')
	abline(v = mean(apples1), col = 'blue')

	hist(apples2, xlim=c(0, 120), ylim = c(0, max(likeMean2)),
     breaks = appleRange, border = 'red')
	lines(x.vals, dnorm(x.vals, mean = mean(apples2), sd = sd(apples2)) * 100,
      col = 'red')
	lines(x.vals, likeMean2)
	abline(v = fitted(m5)[x$Tree == 2], col = 'red')
	abline(v = mean(apples2), col = 'blue')

	hist(apples3, xlim= c (0, 120), ylim = c(0, max(likeMean3)),
     breaks = appleRange, border = 'blue')
	lines(x.vals, dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100,
      col = 'blue')
	lines(x.vals, likeMean3)
	abline(v = fitted(m5)[x$Tree == 3], col = 'red')
	abline(v = mean(apples3), col = 'blue')
	par(mfrow = c(1, 1))
}
