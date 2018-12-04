# Parameters and utility functions for the apple practical

n_apples   <- 20 # Number of apples per tree
n_trees    <- 3  # Number of trees
heights    <- runif(3 * n_apples, 1, 3) # heights of each apple

# Allocate random heights to all apples


grow_my_apples <- function(ntrees = 3,
                           n_ap = n_apples,
                           height = heights,
                           tree_intercept = c(70, 80, 90),
                           tree_sds = c(25, 30, 40)){
  slopes <- rnorm(ntrees, 1, 4)
  apple_weights <- rnorm(ntrees * n_apples,
                         mean = height * rep(slopes, each = n_ap) +
                                rep(tree_intercept, each = n_ap),
                         sd = rep(tree_sds, each = n_ap))
  tree <- factor(rep(1:ntrees, each = n_ap))
  return(data.frame(tree, apple_weights, height))
  }

pick_my_apples <- function(x, tree){
  return(x$apple_weights[x$tree == tree])
}

########################################
# Visualise data
########################################

# Set up some graphical parameters

plot_my_orchard <- function(x=my.orchard){
  par(mfrow = c(3, 1))
  x.vals <- seq(0, 120, 0.1)
  apples1 <- pick_my_apples(x, 1)
  apples2 <- pick_my_apples(x, 2)
  apples3 <- pick_my_apples(x, 3)
  apple_range <- seq(from = min(x$apple_weights) - 0.001,
                     to = max(x$apple_weights) + 0.001,
                     length.out = 150)
# Histograms and curves
  yvals <- dnorm(x.vals,
               mean = mean(apples1),
               sd = sd(apples1)
               ) * 100
  hist(apples1, xlim = c(0, 120),
       breaks = apple_range, border = "purple"
       )
  lines(x.vals, yvals,
        col = "purple"
        )

  yvals <- dnorm(x.vals, mean = mean(apples2), sd = sd(apples2)) * 100
  hist(apples2, xlim = c(0, 120),
       breaks = apple_range,
       border = "red")
  lines(x.vals, yvals,
        col = "red")

  yvals <- dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100
  hist(apples3, xlim = c(0, 120),
       breaks  = apple_range,
       border = "blue")
  lines(x.vals, yvals,
        col = "blue")
  par(mfrow = c(1, 1))
}

plot_likelihood_means <- function(x = my.orchard){
  likelihood_of_mean_weight <- function(tree){
    dnorm(x.vals, mean = mean(tree),
          sd = sd(tree) / n_apples ^ .5) * 100
  }

  x.vals <- seq(0, 120, 0.1)
  apple_range <- seq(min(x$apple_weights) - 0.001,
                   max(x$apple_weights) + 0.001,
                   length.out = 150)

  # Compute likelihood curves
  apples1 <- pick_my_apples(x, 1)
  apples2 <- pick_my_apples(x, 2)
  apples3 <- pick_my_apples(x, 3)
  like_mean1 <- likelihood_of_mean_weight(apples1)
  like_mean2 <- likelihood_of_mean_weight(apples2)
  like_mean3 <- likelihood_of_mean_weight(apples3)

  # Plot out the likelihood curves. How do they relate to the Std. Error?
  par(mfrow = c(3, 1))
  hist(apples1, xlim = c(0, 120), ylim = c(0, max(like_mean1)),
     breaks = apple_range, border = "purple")
  lines(x.vals,
        dnorm(x.vals,
              mean = mean(apples1),
              sd = sd(apples1)
              ) * 100,
        col = "purple")
  lines(x.vals, like_mean1)

  hist(  apples2, xlim = c(0, 120),
      ylim = c(0, max(like_mean2)),
      breaks = apple_range,
      border = "red")
  lines(x.vals,
        dnorm(x.vals, mean = mean(apples2),
        sd = sd(apples2)) * 100, col = "red")
  lines(x.vals, like_mean2)

  hist(  apples3,
      xlim = c(0, 120),
      ylim = c(0, max(like_mean3)),
      breaks = apple_range,
      border = "blue")
  lines(x.vals, dnorm(x.vals,
                      mean = mean(apples3),
                      sd = sd(apples3)) * 100,
                      col = "blue")
  lines(x.vals, like_mean3)
  par(mfrow = c(1, 1))
}

plot_lme <- function(x = my.orchard,
                  m7 = mod7,
                  m6 = mod6,
                  n_apples = n_apples){
  plot(x$height,
       x$apple_weights,
       pch = as.numeric(x$tree),
       col = rep(c("purple", "red", "blue"), each = n_apples),
      xlab = "height",
      ylab = "Apple Weight",
      main = "Models fitted by lm (black) and lme(magenta)")
  for (i in 1:3) lines(x$height[x$tree == i],
                       fitted(m7)[x$tree == i])
  for (i in 1:3) lines(x$height[x$tree == i],
                       fitted(m6)[x$tree == i],
                       col = "magenta")
}

plot_lme_means <- function(x = my.orchard,
                           m5= mod5){
  likelihood_of_mean_weight <- function(tree){
    dnorm(x.vals,
    mean = mean(tree),
    sd = sd(tree) / n_apples ^ .5) * 100
  }

  apple_range <- seq(from = min(x$apple_weights) - 0.001,
                   to = max(x$apple_weights) + 0.001,
                   length.out = 150)
  x.vals <- seq(0, 120, 0.1)
  # Compute likelihood curves
  apples1 <- pick_my_apples(x, 1)
  apples2 <- pick_my_apples(x, 2)
  apples3 <- pick_my_apples(x, 3)
  like_mean1 <- likelihood_of_mean_weight(apples1)
  like_mean2 <- likelihood_of_mean_weight(apples2)
  like_mean3 <- likelihood_of_mean_weight(apples3)
  par(mfrow = c(3, 1))
  hist(apples1, xlim = c(0, 120), ylim = c(0, max(like_mean1)),
       breaks = apple_range, border = 'purple')
  lines(x.vals, dnorm(x.vals, mean = mean(apples1), sd = sd(apples1)) * 100,
        col = "purple")
  lines(x.vals, like_mean1)
  abline(v = fitted(m5)[x$tree == 1], col = "red")
  abline(v = mean(apples1), col = "blue")
  hist(apples2, xlim=c(0, 120), ylim = c(0, max(like_mean2)),
       breaks = apple_range,
       border = "red")
  lines(x.vals, dnorm(x.vals, mean = mean(apples2), sd = sd(apples2)) * 100,
      col = "red")
  lines(x.vals, like_mean2)
  abline(v = fitted(m5)[x$tree == 2], col = "red")
  abline(v = mean(apples2), col = "blue")

  hist(apples3, xlim= c (0, 120), ylim = c(0, max(like_mean3)),
     breaks = apple_range, border = "blue")
  lines(x.vals, dnorm(x.vals, mean = mean(apples3), sd = sd(apples3)) * 100,
      col = "blue")
  lines(x.vals, like_mean3)
  abline(v = fitted(m5)[x$tree == 3], col = "red")
  abline(v = mean(apples3), col = "blue")
  par(mfrow = c(1, 1))
}
