# Randomization Expectations
# TomW Sept 2019

# v0.0

library(tidyverse)

# for replication

.Random.seed = 9999

# pre-grouping sample size or "Experimantal n"
En = 5000

# Difference between group means in Standard Deviations of the population
calcDelta = function(Tn, En, mean=0, sd=1) {
   r=rep(0,Tn)
   for(i in 1:Tn) {
      r[i] = mean(rnorm(En/2,mean,sd))-mean(rnorm(En/2,mean,sd)) / sd
   }
   return(r)
}

# number of trials or "Trial n"
Tn = 1000000
r = tibble(run=seq_len(Tn),
         delta=calcDelta(Tn, En))

min(r$delta)
max(r$delta)
var(r$delta)^.5

# percentage of trials with delta > .2 Std Deviation of population (Cohen: small effect size)
sum(abs(r$delta)>.2) / Tn

################

# En = 50, TN = 1,000,000
#> min(r$delta)
#[1] -1.278864
#> max(r$delta)
#[1] 1.404144
#> var(r$delta)^.5
#[1] 0.2826473
#>
#> # percentage of trials with delta > .2 Std Deviation of population (Cohen: small effect size)
#> sum(abs(r$delta)>.2) / Tn
#[1] 0.478995

# En = 500, TN = 1,000,000
#>min(r$delta)
#[1] -0.4160234
#> max(r$delta)
#[1] 0.455857
#> var(r$delta)^.5
#[1] 0.08940054
#>
#> # percentage of trials with delta > .2 Std Deviation of population (Cohen: small effect size)
#> sum(abs(r$delta)>.2) / Tn
#[1] 0.025282

# En = 5000, TN = 1,000,000
#> min(r$delta)
#[1] -0.1307717
#> max(r$delta)
#[1] 0.1333587
#> var(r$delta)^.5
#[1] 0.02824924
#>
#> # percentage of trials with delta > .2 Std Deviation of population (Cohen: small effect size)
#> sum(abs(r$delta)>.2) / Tn
#[1] 0
