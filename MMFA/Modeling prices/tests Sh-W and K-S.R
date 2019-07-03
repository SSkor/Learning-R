#Скорикова ПМ3-3#######################################################
x <- replicate(100, { # generates 100 different tests on each distribution
  c(ks.test(rnorm(10), rnorm(10))$p.value,
    ks.test(rnorm(100), rnorm(100))$p.value,
    ks.test(rnorm(1000), rnorm(1000))$p.value,
    ks.test(rnorm(5000), rnorm(5000))$p.value) 
} # rnorm gives a random draw from the normal distribution
)
rownames(x) <- c("n10","n100","n1000","n5000")
rowMeans(x<0.05) # the proportion of significant deviations (доля отклонений)

x <- replicate(100, { # generates 100 different tests on each distribution
  c(shapiro.test(rnorm(10))$p.value,   
    shapiro.test(rnorm(100))$p.value,  
    shapiro.test(rnorm(1000))$p.value,
    shapiro.test(rnorm(5000))$p.value) 
} # rnorm gives a random draw from the normal distribution
)
rownames(x) <- c("n10","n100","n1000","n5000")
rowMeans(x<0.05) # the proportion of significant deviations
######################################################################
#на больших выборках лучше Колмогоров
######################################################################
x <- replicate(100, { # generates 100 different tests on each distribution
  c(ks.test(rt(10, 3), rnorm(10))$p.value,
    ks.test(rt(100, 3), rnorm(100))$p.value,
    ks.test(rt(1000, 3), rnorm(1000))$p.value,
    ks.test(rt(5000, 3), rnorm(5000))$p.value) 
} # rnorm gives a random draw from the normal distribution
)
rownames(x) <- c("n10","n100","n1000","n5000")
rowMeans(x<0.05) # the proportion of significant deviations

x <- replicate(100, { # generates 100 different tests on each distribution
  c(shapiro.test(rt(10, 3))$p.value,   
    shapiro.test(rt(100, 3))$p.value,  
    shapiro.test(rt(1000, 3))$p.value,
    shapiro.test(rt(5000, 3))$p.value) 
} # rnorm gives a random draw from the normal distribution
)
rownames(x) <- c("n10","n100","n1000","n5000")
rowMeans(x<0.05) # the proportion of significant deviations
########################################################################
#со Стьюдентом(3) лучше справляется Ш-У
########################################################################
x <- replicate(100, { # generates 100 different tests on each distribution
  c(ks.test(rlnorm(10), rnorm(10))$p.value,
    ks.test(rlnorm(100), rnorm(100))$p.value,
    ks.test(rlnorm(1000), rnorm(1000))$p.value,
    ks.test(rlnorm(5000), rnorm(5000))$p.value) 
} # rnorm gives a random draw from the normal distribution
)
rownames(x) <- c("n10","n100","n1000","n5000")
rowMeans(x<0.05) # the proportion of significant deviations

x <- replicate(100, { # generates 100 different tests on each distribution
  c(shapiro.test(rlnorm(10))$p.value,   
    shapiro.test(rlnorm(100))$p.value,  
    shapiro.test(rlnorm(1000))$p.value,
    shapiro.test(rlnorm(5000))$p.value) 
} # rnorm gives a random draw from the normal distribution
)
rownames(x) <- c("n10","n100","n1000","n5000")
rowMeans(x<0.05) # the proportion of significant deviations
########################################################################
#С логнормальным на малой выборке лучше справляется Ш-У
########################################################################
#a <- c(1,2,3,5,4,-1)
#dim(a) <- c(2,3)
#rowMeans(a>0)