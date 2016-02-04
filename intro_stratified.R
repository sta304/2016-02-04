library(dplyr)
library(ggplot2)

# Make up some data
N_1 <- 500
N_2 <- 500
N <- N_1 + N_2
stratum <- factor(c(rep("A", N_1), rep("B", N_2)))

set.seed(1)

mu_1 <- 50
mu_2 <- 80
sigma_1 <- 10
sigma_2 <- 10

value <- c(rnorm(N_1, mu_1, sigma_1), rnorm(N_2, mu_2, sigma_2))
population <- data_frame(stratum, value)

# Plot
population %>% 
  ggplot(aes(x = value, color = stratum)) + geom_density()

# Sampling fraction (easier this way - sample size is then frac*N or frac*N_1
# and frac*N_2
frac <- 0.01

# What happens with SRS?
population %>% 
  sample_frac(frac) %>% 
  group_by(stratum) %>% 
  summarize(n=n(), mean=mean(value))

# What happens with Stratified?

population %>% 
  group_by(stratum) %>% 
  sample_frac(frac) %>% 
  summarize(n=n(), mean=mean(value))


# Larger number of SRS simulations
k <- 1000
n <- 10
srs_sims <- replicate(k, 
                      population %>% 
                        sample_n(size = n),
                      simplify = FALSE)
                  
srs_means <- sapply(srs_sims, function(x) mean(x$value))

# Stratified
frac <- 0.01
strat_sims <- replicate(k, 
                        population %>%
                          group_by(stratum) %>% 
                          sample_frac(frac),
                        simplify = FALSE)

strat_mean <- function(x, N_1, N_2, N) {
  mu <- unlist(select(summarize(group_by(x, stratum), mu=mean(value)), mu))
  return(sum(c(N_1, N_2)*mu)/N)
}

strat_means <- sapply(strat_sims, function(x) strat_mean(x, N_1, N_2, N))

# Compare

all_means <- rbind(data_frame(scheme="srs", means=srs_means),
                   data_frame(scheme="strat", means=strat_means))

all_means %>% 
  ggplot(aes(x=means, color=scheme)) + geom_density()

all_means %>%
  group_by(scheme) %>% 
  summarize(mean=mean(means), var=var(means), sd=sd(means))
