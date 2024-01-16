### Question: How many times do I need to roll my die to know if it's fair?
### Answer: N (output below)

n = 6 # NOTE: SET THIS TO THE NUMBER OF SIDES ON YOUR PREFERRED DIE

# install.packages('pwr')
library('pwr')

# Since we will be using the chi-square goodness of fit test
# we'll calculate our sample size (N) using pwr.chisq.test, where
w = 0.3 # I'm choosing a medium effect size, 0.3
# If you'd like to notice a small effect size, change to 0.1
# Although this increases the number of rolls dramatically
df = n-1
sig.level = 0.05 # standard choice for type I error probability
power = 0.9 # 0.8 is the standard power but I think 0.9 is better here, imo


N <- ceiling(pwr.chisq.test(w=w,df=df, sig.level=sig.level, power=power)$N)

print(N) # This is the number of rolls you have to do for your test

### Chi-square Goodness-of-fit test for simulated die

sides <- 1:n

set.seed(12345)
random <- runif(n)
random_weights <- random/sum(random) # I'm randomly assigning weights to
    # the sides of the die.
    # these weights do not correspond to each other as an actual die would, but
    # that is irrelevant as this is just sample code to be replaced with real
    # world observations

### REPLACE 'obs' WITH YOUR OBSERVATIONS
obs <- sample(sides, size = N, replace = TRUE, prob = random_weights)
obs_factor = factor(obs, levels = sides) # making it a factor means the test
  # still functions even if a side is never rolled
hist(obs) #here's a histogram of our simulated observed die

test <- chisq.test(table(obs_factor), p = c(rep(1/n,n)))
p_val <- test$p.value


# Results: 
if (p_val < sig.level) {
  print("We reject the null hypothesis and accept the alternative hypothesis; this die is judged to be unfair")
} else {
  print("We have insufficient evidence to reject the null hypothesis; this die is judged to be fair")
}
