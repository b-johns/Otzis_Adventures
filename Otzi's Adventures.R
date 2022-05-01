################################################################################
# Otzi's Adventures: helping you achieve Himalaya supremum in a compact budget
################################################################################

# Load libraries
library(dplyr)
library(lme4)
library(ggplot2)
library(fitdistrplus)
library(tidyverse)
library(stats4)
select <- dplyr::select

# Load data
df <- read.csv('climbers_full.csv')


# read in dataset #
# add variables for the height and full name of each peak
climbers_full <- read.csv("climbers_full.csv") %>%
  mutate(peakhgt = case_when(peakid == "EVER" ~ 8489,
                             peakid == "KANG" ~ 8586,
                             peakid == "LHOT" ~ 8516,
                             peakid == "MAKA" ~ 8485,
                             peakid == "CHOY" ~ 8188,
                             peakid == "DHA1" ~ 8167,
                             peakid == "MANA" ~ 8163,
                             peakid == "ANN1" ~ 8091),
         peakname = case_when(peakid == "EVER" ~ "Everest",
                              peakid == "KANG" ~ "Kangchenjunga",
                              peakid == "LHOT" ~ "Lhotse",
                              peakid == "MAKA" ~ "Makalu",
                              peakid == "CHOY" ~ "Cho Oyu",
                              peakid == "DHA1" ~ "Dhaulagiri",
                              peakid == "MANA" ~ "Manaslu",
                              peakid == "ANN1" ~ "Annapurna"))



# create a unique ID for each climber, and tally how many climbs they went on (n_climb),
# how many summits they achieved (n_success), how often they used oxygen as a 
# ratio of oxygen use count over climb count (o2_ratio), and whether or
# not they ultimately died on one of their climbs (died)
climbers <- climbers_full  %>% 
  mutate(climb_id = group_indices(.,fname, lname, yob)) %>%
  group_by(climb_id) %>%
  mutate(n_climb = sum(n()),
         died = sum(death),
         n_success = sum(msuccess),
         summited = ifelse(n_success == 0, 0, 1),
         n_o2 = sum(mo2used),
         o2_ratio = n_o2/n_climb) %>% 
  ungroup() 


# create dataframe that keeps one observation per distinct climber
climbers_unique <- climbers %>% distinct(climb_id, .keep_all = TRUE)


all_peaks <- climbers$peakid %>% unique(); all_peaks


################################################################################
# Fitting Distributions 
################################################################################

# It's possible that the number of climbs someone goes on before they die could,
# in theory, be modeled by a geometric distribution, since we are counting the
# number of trials (climbs) until a condition is met (death).

# take only the climbers who die, and extract a vector of the number of climbs
# they went on
climbs_b4_death <- climbers_unique %>%
  filter(died == 1) %>%
  pull(n_climb)

# examining the barplot, there is a steep drop from 1 to 2 climbs, but perhaps
# it can be modeled by a geometric distribution
barplot(table(climbs_b4_death),
        main = "Number of Climbs Before Death",
        sub = "(Inlcudes Final Climb)",
        xlab = "Number of Climbs",
        ylab = "Frequency",
        col = "firebrick4")

# bin the observed data so we have 10 or more observations per bin
obs <- table(climbs_b4_death); obs
obs_bin <- obs
obs_bin[5] <- sum(obs_bin[5:6]); obs_bin
obs_bin[6] <- sum(obs_bin[7:12]); obs_bin
obs_bin <- obs_bin[1:6]; obs_bin

# use fitdist to estimate the parameter p, which is the probability of death
# on each climb
param <- fitdist(climbs_b4_death, "geom", method = c("mle")); param
p <- param[[1]]; p

# bin the theoretical distribution similarly to the observed data
probs <- c(dgeom(0:3,p), dgeom(4,p) + dgeom(5,p), 1 - pgeom(5,p))
exp_bin <- probs*length(climbs_b4_death); exp_bin


# now we can check a barplot again, comparing the observed values against
# the expected values. Visually, our observed values don't appear to match the
# expected distribution very well, but we can conduct a chi-square test to be sure.
barplot(rbind(obs_bin, exp_bin), 
        beside = TRUE,
        main = "Number of Climbs Before Death",
        sub = "(Inlcudes Final Climb)",
        xlab = "Number of Climbs",
        ylab = "Frequency",
        col = c("firebrick4", "cadetblue4"),
        legend.text = c("Observed", "Expected"))

# conduct a chi-square test
ChiSq <-function(obs,exp){
  sum((obs-exp)^2/exp)
}
Chistat <- ChiSq(obs_bin, exp_bin); Chistat
# lose an extra degree of freedom for having estimated a parameter
# 6 bins - 2 df = 4 df

# the small p-value tells us that there's no significant chance that
# these data could be modeled by a geometric distribution. So unfortunately we
# cannot give clients a good estimate of how many climbs they might go on before
# expecting an unfortunate accident.
pchisq(Chistat, 4, lower.tail = FALSE)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# How about the number of summits a climber can expect to achieve before
# they die? Let's see if this can be modeled geometrically.

# take only the climbers who die, and extract the number of times they summit
summits_b4_death <- climbers_unique %>%
  filter(died == 1) %>% 
  pull(n_success)

# Examining the barplot, there is a steady decrease from 0 to 1, and a sharper
# drop from 1 to 2, but perhaps it can be modeled geometrically
barplot(table(summits_b4_death),
        main = "Number of Succesful Summits Before Death",
        sub = "(Inlcudes Final Summit)",
        xlab = "Number of Summits",
        ylab = "Frequency",
        col = "firebrick4")

# bin the observed data so we have 10 or more observations per bin
obs <- table(summits_b4_death); obs
obs_bin <- obs
obs_bin[5] <- sum(obs_bin[5:8]); obs_bin
obs_bin <- obs_bin[1:5]; obs_bin

# use fitdist to estimate the parameter for a geometric distribution based
# on our data
param <- fitdist(summits_b4_death, "geom", method = c("mle")); param
p <- param[[1]]; p

# bin the expected values the same way as the observed
probs <- c(dgeom(0:3, p), 1 - pgeom(3,p)); sum(probs)
exp_bin <- probs*length(summits_b4_death); exp_bin

# looking at a side by side barplot, the observed values generally match quite
# nicely with the expected values, so there is a good chance that this data
# can in fact be modeled geometrically. We conduct a chi-square test to make sure
barplot(rbind(obs_bin, exp_bin), 
        beside = TRUE,
        main = "Number of Succesful Summits Before Death",
        sub = "(Inlcudes Final Summit)",
        xlab = "Number of Summits",
        ylab = "Frequency",
        col = c("firebrick4", "cadetblue4"),
        legend.text = c("Observed", "Expected"))

# conduct chi-squre test
ChiSq <-function(obs,exp){
  sum((obs-exp)^2/exp)
}
Chistat <- ChiSq(obs_bin, exp_bin); Chistat
# lose an extra degree of freedom for having estimated a parameter
# 5 bins - 2 df = 5 df

# Our p-value tells us there is a reasonable probability that the difference
# between our observed and expected distributions is solely due to chance. i.e.
# that our data can, in fact be modeled by a geometric distribution with p = .54
pchisq(Chistat, 3, lower.tail = FALSE)

# Using the parameter from the distribution, we can calculate the expected number
# of summits that someone achieves before they die (if they die at all), and the
# variance in number of summits
1/p # expected value
(1-p)/p^2 # variance


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
### analysis of summit times and deaths ###

# While getting to the summit (and returning safely) is the ultimate goal of
# any expedition, what time the summit is reached also of great importance.
# We examine whether summit times follow any kind of continuous probability distribution
# It would make sense for the data to follow a gamma distribution, since climbers
# generally try to summit around late morning or early afternoon, with fewer 
# people summiting late in the day, so we would expect a right skewed distribution
# with a peak that rises somewhat rapidly and falls off exponentially.

# dataset of only people who successfully summit 
summiters <- climbers %>% 
  filter(msuccess == TRUE,
         !is.na(msmttime1)) %>%
  mutate(msmttime1 = as.numeric(msmttime1)) %>%
  # put summit time in terms of hours/fractions of hours
  mutate(remainder = msmttime1 %% 100,
         hour = (msmttime1 - remainder) / 100,
         frac = remainder / 60,
         summit_time = hour + frac) %>%
  select(peakid, peakname, summit_time, death)


# we check a histogram of summit times for all 8 peaks combined, and it looks
# like it may follow a gamma distribution
hist(summiters$summit_time, 
     main = "Histogram of Summit Times for All Peaks",
     xlab = "Summit Time (24 Hour Clock)",
     border = "white",
     col = "cadetblue4",
     xlim = (c(0,24)),
     probability = TRUE, breaks = "FD")


### try to fit a gamma distribution ###
# this function takes the summit time data for whichever mountain is passed
# into it and tests whether the summit times fits a gamma distribution by:
# 1) estimating the parameters of a theoretical gamma distribution based on the data
# 2) outputting a histogram of the data with a density curve for the theoretical distribution
# 3) comparing the actual data to the theoretical distribution using a decile comparison approach and chi-square

smttime_gamma <- function(peak) {
  
  # vector of summit times
  stime <- summiters %>% filter(peakid %in% c(peak)) %>% pull(summit_time)
  sname <- summiters %>% filter(peakid %in% c(peak)) %>% pull(peakname) %>% unique()
  
  # use fitdist to estimate the parameters #  
  param <- fitdist(stime, "gamma", method = c("mle")); param
  shape <- param$estimate[[1]]; shape
  rate <- param$estimate[[2]]; rate
  
  # for the histogram titles
  if(peak == all_peaks) {
    sname <- "All Peaks"
  } else {
    sname <- sname
  }
  
  # histogram and density curve #
  hist(stime, 
       main = paste0("Histogram of Summit Times for ", sname),
       xlab = "Summit Time (24 Hour Clock)",
       border = "white",
       col = "cadetblue4",
       xlim = (c(round(min(stime))-1,round(max(stime))+1)),
       probability = TRUE, breaks = "FD")
  
  curve(dgamma(x, shape = shape, rate = rate), add = TRUE, col = "red")
  
  
  # check fit by comparing deciles #
  # bins
  bins <- qgamma(0.1*(0:10), shape = shape, rate = rate); bins
  bincode <- cut(stime, breaks = bins); table(bincode)
  
  # check we have the same number of observations
  sum(table(bincode)); length(stime)
  
  # observed distribution by bin
  observed <- as.vector(table(bincode)); observed
  # expected has equal number in each decile
  expected <- rep(sum(observed)/10, 10); expected
  
  # compute chi-sq stat, subtract 2 degrees of freedom for two parameters
  chisq <- sum((observed-expected)^2/expected); chisq
  pvalue <- pchisq(chisq, df = 7, lower.tail = FALSE); pvalue
}  

# we apply the function to each individual peak, and to the collection of all 8

smttime_gamma("CHOY")
smttime_gamma("EVER")
smttime_gamma("MANA")
smttime_gamma("LHOT")   ### LHOTSE summit times can be modeled by a gamma dist
smttime_gamma("DHA1")
smttime_gamma("ANN1")
smttime_gamma("KANG")
smttime_gamma("MAKA")
smttime_gamma(all_peaks)

# In the end, it seems that at large summit times cannot reliably be modeled by 
# a gamma distribution. Only one mountain, Lhotse, returned a p-value large 
# enough to suggest that the data may come from a gamma distribution. 


################################################################################
# Miscellaneous Analysis
################################################################################

##############################################
### permutation test summit time and death ###
##############################################

# Did people who ultimately died on a climb summit later than those who survived?
# We would expect the answer to be yes. In high altitude mountaineering, teams
# decide on a "turnaround time" when making their final push to the summit. As
# the name suggests, the turnaround time is the time at which climbers should
# start their decent back to their last camp, regardless of how close to the top
# they may be. The exact turnaround time depends on the season, expected weather,
# daylight, and other factors. But generally, the time is set so that climbers
# will have enough time to return to the camp from the summit before nightfall
# or afternoon storms set in. For example, the typical turnaround time for Everest
# during the busy spring season is around 2pm. Ignoring the turnaround time can,
# and has, had famously disastrous results. 


# First we take a graphical approach, with a boxplot of summit times for all
# peaks, by whether or not the climber survived or died. 
# The boxplot suggests that there may be significant differences in the summit
# times of surviving and dying climbers. The median for climbers who survived
# is around 8am, while for those who died it is around noon or 1pm. A large
# majority of the interquartile range for those who died is above the IQR for
# survivors, and only outliers amongst survivors column have summit times
# later than the 75th percentile of those who died. 

ggplot(data = summiters, aes(x = death, y = summit_time)) + 
  geom_boxplot(fill = "cadetblue4") + 
  scale_x_discrete(labels = c("TRUE" = "Died", "FALSE" = "Survived")) +
  # geom_jitter(position = position_jitter(.2), size = .1, color = "red") +
  # stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
  labs (
    title = "Summit Times By Mortality",
    subtitle = "(All Peaks)",
    x = "Climber Mortality Outcome",
    y = "Summit Time (24 hour clock)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 14, hjust = .5),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black")
  ) +
  coord_flip()



# next, we conduct a permutation test to check whether the difference in mean
# summit times between surviving climbers and dying climbers is significant

# split data into survivors and not
id_death <- which(summiters$death == "TRUE")
dead <- summiters[id_death,]
alive <- summiters[-id_death,]

# means for each group
mean_dead <- mean(dead$summit_time); mean_dead
mean_alive <- mean(alive$summit_time); mean_alive

# observe about a 4 hour difference in means, which seems large
observed <- mean_dead - mean_alive; observed

# permutation test
N <- 1000
diffs <- numeric(N)
for (i in 1:N){
  Death <- sample(summiters$death)   #permuted mortality column
  DeadAvg <- sum(summiters$summit_time*(Death == "TRUE"))/sum(Death == "TRUE")
  AliveAvg <- sum(summiters$summit_time*(Death == "FALSE"))/sum(Death == "FALSE")
  diffs[i] <- DeadAvg - AliveAvg
}
mean(diffs) # should be, and is, close to zero


# calculating the p-value gives us a significant result. There is only about a 
# 0.001 probability that a difference in means this large would arise by chance.
# So, we conclude that climbers who ultimately died summit the mountain significantly
# later in the day than those who survive, as was expected.
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue



################################################################################
# Oxygen analysis
################################################################################

# Altitudes above 8000m are referred to as the "death zone" in mountaineering,
# because the concentration of oxygen in the air is not high enough to sustain
# life for extended periods of time. This severe lack of oxygen leads to impaired 
# decision making, deteriorating 
# bodily functions, and puts climbers at greater risk of deadly pulmonary or 
# cerebral edema. And as the name implies, too long in the death zone ultimately
# ha only one outcome. Some climbers choose to brave these dangers head on,
# relying on incredible fitness, speed, and climbing experience to get them up to the summit
# and down out of the death zone quickly. However, supplemental oxygen has been
# used for decades by high altitude mountaineers to combat the effects of high
# altitude and extend the amount of time they can spend in the death zone. 
# Supplemental oxygen makes climbing at higher altitudes much easier, and much
# safer, which is why we fully endorse its use here at Otzi's Adventures.

# In this section, we will analyze the many impacts that the use (or lack thereof)
# that supplemental oxygen has on climber outcomes.


# We start with a simple regression of the number of successful summits on the 
# number of climbs someone goes on, and examine the slope of the line for three
# groups: those who always use oxygen, those who never use oxygen, and those
# who use oxygen some of the time

# create a dataframe which identifies the three groups
o2_success <- climbers_unique %>%
  mutate(o2_use = case_when(o2_ratio == 1 ~ "Oxygen Always Used",
                            o2_ratio == 0 ~ "Oxygen Never Used",
                            TRUE ~ "Oxygen Sometimes Used")) %>%
  select(climb_id, n_climb, n_success, o2_use, o2_ratio) 

# we see that never using oxygen and always using oxygen make up the majority of the data
table(o2_success$o2_use)

# The first regression shows that, for people who use supplemental oxygen, for every 
# additional climb they go on, they can expect to summit .9 mountains. i.e. if
# someone goes on 10 climbs, we expect them to summit 9 peaks
lm.o2_used <- lm(n_success ~ n_climb, data = o2_success %>% filter(o2_use == "Oxygen Always Used"))
summary(lm.o2_used)

# However, for those that don't use supplemental oxygen, we only expect .35 
# summits per climb, or between 3 and 4 summits per every 10 climbs
lm.o2_notused <- lm(n_success ~ n_climb, data = o2_success %>% filter(o2_use == "Oxygen Never Used"))
summary(lm.o2_notused)

# while those that use oxygen some for some fraction of their overall
# climbs, they can expect to summit about 7 mountains per 10 climbs
lm.o2_sometimes <- lm(n_success ~ n_climb, data = o2_success %>% filter(o2_use == "Oxygen Sometimes Used"))
summary(lm.o2_sometimes)


# binning oxygen use into 3 distinct groups lends itself nicely to a 
# visual comparison of the regression lines. Here we can see the 
# disparity in the slope between the three groups
ggplot(data = o2_success, aes(x = n_climb, y = n_success, color = o2_use, shape = o2_use)) +
  
  geom_point( size = 2) + 
  
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, size = .8) +
  
  scale_color_manual(values = c("cadetblue4", "firebrick4", "azure4")) +
  
  scale_shape_manual(values = c(19, 19, 24)) + 
  
  labs (
    title = "Summit Success by Oxygen Use Over Multiple Climbs",
    subtitle = "(All Peaks)",
    x = "Number of Climbs",
    y = "Number of Succesful Summits"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 14, hjust = .5),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    legend.title = element_blank()
  ) 


# however, to truly capture the impact that oxygen use has on number of summits
# reached in a linear regression model setup, we should control for other factors, 
# and enter our oxygen use ratio as its own variable, rather than use it for
# conditional grouping
# here we estimate the number of successful summits, with oxygen use ratio as
# our variable of interest, which we expect to enter positively. We control 
# for number of climbs, the age and sex of the climber, and whether that climber
# died
lm.success <- lm(n_success ~ o2_ratio + n_climb + died + calcage + sex, data = climbers_unique)
summary(lm.success)

# with this, we see again the power of supplemental oxygen use in helping
# climbers summit mountains, as our oxygen use ratio enters the most positively
# into our regression, even more so than the number of climbs performed.
# The dummy variable for whether a climber died or not tells us, as expected
# that if a climber dies they summit fewer mountains overall.Age enters 
# negatively as well, though the coefficient is minuscule. Likewise men have
# a slight advantage over women

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# we can use logistic regression to estimate the impact that oxygen use has
# on the odds of summiting or dying. 

# extract data for summit success, death, and oxygen use ratio
# note that these are aggregates over all climbs for one particular climber,
# so summited is whether a climber ever summited (1) or not (0). Likewise
# for death
summited <- climbers_unique$summited
died <- climbers_unique$died
o2_ratio <- climbers_unique$o2_ratio

### oxygen use and summiting ###
# plot oxygen use against whether climber ever successfully summited
plot(o2_ratio, summited,
     main = "Oxygen Use Impact on Odds of Summiting",
     xlab = "Oxygen Use Ratio",
     ylab = "Summited at Least One Peak")

# use log likelihood function to estimate the parameters
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*o2_ratio)/(1+exp(alpha+beta*o2_ratio)) )*summited
        + log(1/(1+exp(alpha+beta*o2_ratio)))*(1-summited) )
}

# fit the model with initial guess of alpha = 0, beta = 0
results<-mle(MLL, start = list(alpha = 0, beta = 0)) 
results@coef
# plot the regression curve
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)

# We see from the curve that as a climbers oxygen use ratio increases, so
# to do their odds of having summited at least one mountain
# our results agree with R's built in function, and we even see that the
# oxygen use ratio significantly increases the log odds of summiting
summary(glm(summited~o2_ratio, family = "binomial", data = climbers_unique))


### oxygen use and death ###
plot(o2_ratio, died,
     main = "Oxygen Use Impact on Odds of Dying",
     xlab = "Oxygen Use Ratio",
     ylab = "Died")

# use log likelihood function to estimate the parameters
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*o2_ratio)/(1+exp(alpha+beta*o2_ratio)) )*died
        + log(1/(1+exp(alpha+beta*o2_ratio)))*(1-died) )
}

# fit the model with initial guess of alpha = 0, beta = 0
results<-mle(MLL, start = list(alpha = 0, beta = 0)) 
results@coef
# plot the regression curve
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)

# here the curve is more difficult to see, but there does appear to be a slightly
# negative slope, and our beta coefficient tells us that oxygen use
# decreases the log odds of dying. And we see from using R's built in
# logistic regression function that oxygen use again is significant
summary(glm(died~o2_ratio, family = "binomial", data = climbers_unique))



################################################################################
# Power analysis
################################################################################

  # Here at Otzi's adventures, we are interested in the causal effect of supplemental
  # oxygen use on mortality, for several reasons. We would like to avoid our customers
  # dying while engaged in the expeditions we organize. Dying is unpleasant, so for
  # reasons of ethics and liability we would like to avoid it, if we can. Secondly, 
  # we produce supplemental oxygen as part of a vertically integrated business. If
  # we can show that the causal effect of requiring supplemental oxygen use on mortality 
  # is negative, perhaps this will be a good rationale for requiring people to purchase
  # oxygen from us for their climbs. 


# -------------------------------
# Data setup
# -------------------------------

# Select subset of data that will be used for power analyses
valid_expeditions <- df %>%
  group_by(expid) %>%
  filter(calcage > 0) %>% # will consider using age as a covariate; assuming age 0 is error
  filter(peakid == 'EVER') %>% # want to focus just on Everest for potential study
  summarise(count = n(), num_deaths = sum(death)) %>%
  filter(count > 10) %>% # need sufficient obs within each expedition for multilevel model
  filter(num_deaths > 0) # need expeditions that experienced at least one fatality, for
                         # classical approach to power
length(valid_expeditions$expid) # these criteria leave us with 34 expeditions total
model_data <- df %>% 
  filter(expid %in% valid_expeditions$expid) %>%
  select(expid, membid, peakid, mo2used, calcage, death) # final model data

  # Here, we are retaining only those expeditions with a larger number of climbers
  # that experienced at least one fatality. The former is necessary in order to 
  # fit the multilevel model used below, estimates from which are necessary for
  # the simulation-based approach to power. The latter criterion -- that an 
  # expedition experienced at least one fatality -- was necessary in order to 
  # check the classical calculations of variance of the effects of oxygen
  # use within expeditions. Without any fatalities on an expedition, the 
  # coefficient on supplemental oxygen use for that expedition would have been
  # impossible to estimate. Both of these choices affect generalizability of the 
  # results.

  # Next, we chose to further restrict the sample we use for the power calculations
  # to expeditions that were attempting Everest. As Everest is the tallest peak,
  # we can imagine that the experience of climbing Everest is different from other
  # peaks in ways that might affect mortality risk. We considered including multiple
  # peaks in our analysis, and possibly accounting for these differences with peak
  # fixed effects, but ultimately decided against this because of limited observations
  # that made model fitting difficult. Everest had the most observations by far, so
  # we focused our work there. Once again, as with the choices listed noted above, 
  # this decision impacts the generalizability of the results.


# -------------------------------
# More on our motivations
# -------------------------------

  # Data aggregated to expedition level, with indicator for any deaths and 
  # continuous variable that is proportion of expedition that used supplemental
  # oxygen
  model_data_logit <- df %>%
                      filter(peakid == 'EVER') %>%
                      group_by(expid) %>% 
                      summarise(prop_mo2used = mean(mo2used),
                                any_death = max(death))
                    
  # Logit model
  logit_model <- glm(any_death ~ prop_mo2used, 
                     family = binomial(link = 'logit'), 
                     data = model_data_logit)

  # Now we can plot margins using fit logit model
  base::plot(model_data_logit$prop_mo2used,
             model_data_logit$any_death, 
             type = 'p',
             main = 'Mortaility as a Function of the Rate of Oxygen Use',
             xlab = 'Proportion of Expedition that Used Supplemental Oxygen',
             ylab = 'Probability of Any Death on Expedition')
  logit_coefs <- as.vector(logit_model$coefficients); logit_coefs
  inv_logit_transform <- function(x, 
                                  a = logit_coefs[1],
                                  b = logit_coefs[2]) {
    exp(a + b*x)/(1 + exp(a + b*x))
  }
  curve(inv_logit_transform(x), add = TRUE, col = 'red')

    # These results and graph suggest that, indeed, expeditions that used 
    # oxygen at higher rates had a lower risk of climber death, even if 
    # the associated change in risk was modest. Learning more about the causal 
    # effect of supplemental oxygen use seems worthwhile, if we are careful in 
    # designing a good randomized trial. Randomized trials help us to get at the
    # causal effect of a treatment -- like oxygen use -- because random assign-
    # ment solves the selection problem. Otherwise, any estimate of the effects
    # of oxygen use on mortality may be biased, if oxygen use is correlated with
    # other traits that lead to better safety and lower mortality.


# ------------------------------------------------------
# Assumptions for power calculations
# ------------------------------------------------------
  
  # Statistical power is the probability that we will find a statistically significant
  # result in our study, conditional on some assumed true treatment effect. Without
  # sufficient power, should we have the opportunity to carry out our study, we 
  # might be left wondering whether any null finding was due to oxygen not really
  # affecting mortality or whether we just could not detect an effect because of 
  # inadequate sample size.
  
  # For our power analyses, we used as a guide Gelman and Hill's excellent book
  # "Data Analysis Using Regression and Multilevel/Hierarchical Models" 
  # (Cambridge University Press, 2007). Chapter 20 was especially helpful.
  
  # We make the following assumptions:
  # 1) Expeditions allocated randomly 50:50 into a treatment condition (climbers 
  #    in expedition party required to use supplemental oxygen) and a control 
  #    condition (climbers in expedition party may choose to use supplemental 
  #    oxygen if they wish). It would not be ethical to ban use of oxygen in the 
  #    control group, so the contrast cannot simply be required use or non-use 
  #    of oxygen.
  
  # 2) We want to randomize expeditions, not individual climbers. Requiring some
  #    members of an expedition to use supplemental oxygen while allowing others
  #    to freely choose might sow confusion within an individual expedition. Or
  #    knowledge that some people on the climb are being required to use oxygen
  #    might encourage everyone on the climb to use it, eliminating the identifying
  #    variation in oxygen use we need.
  
  # 3) We want 80% power, so that the NIH will consider funding our study, despite
  #    the unclear value of evaluating oxygen use on Mount Everest to general public 
  #    health. If the NIH pays, this would save our company considerable money.
  
  # 4) We must assume a true treatment effect of supplemental oxygen use in order to
  #    evaluate power. Below, we use the data to inform this assumption.
  
  # We can use a multilevel model to get a sense of how much of a reduction in mortality
  # risk is associated with an individual's use of supplemental oxygen. 
  
  # Fitting with random slopes and intercepts for each expedition
  m1 <- lmer(death ~ mo2used + (1 + mo2used | expid), data = model_data); m1 
  
  # Extracting the random component variances and covariances
  m1_random_effects <- as.data.frame(VarCorr(m1)); m1_random_effects
  
  # Extracting the fixed effects estimates
  m1_fixed_effects <- fixef(m1); m1_fixed_effects
  true_effect <- as.vector(m1_fixed_effects['mo2usedTRUE']); true_effect
  
  # So let's say that the hypothesized true effect of requiring everyone on an 
  # expedition to use supplemental oxygen is to reduce probability of death by 
  # 0.074. This is the estimated mean association between oxygen use and death
  # in our multilevel model. Below, we say more about why we whose to use a 
  # multilevel linear probability model instead of a multilevel logistic 
  # regression model.
  
  
# -------------------------------------------------  
# Classical approach to power
# -------------------------------------------------

  # This approach makes use of the fact that to achieve 80% power, you need a
  # true effect that is 2.8 standard deviations away from a comparison point 
  # (and of course making the common assumption that treatment estimates are 
  # distributed asymptotically normal around that true effect). Our comparison 
  # point will be 0 -- as in the null hypothesis that supplemental oxygen use 
  # does nothing to reduce or increase mortality.
  pnorm(1.96, 2.8, 1, lower.tail = FALSE) # assuming a std. dev. of 1, we see 2.8 gives 80% power
  
  # Within each expedition, we can estimate a linear probability model with death 
  # on the left hand side and use of oxygen on the right hand side. We 
  # then calculate the variance for coefficients on oxygen use across expeditions
  # and plug this into a formula that uses our assumed true treatment effect, 
  # -0.074, and this estimated variance, to give us the number of expeditions that
  # we would need to randomize. The formula is simply a rearrangement of the
  # formula for the standard error of a treatment estimate in a linear model in
  # which whole groups where randomized to treatment and control. 
  
  # But instead of having to estimate 34 separate linear probability models --
  # with one each for our 34 expeditions -- we can instead approximate this same 
  # estimation variance by setting up a "typical" expedition and then calculating
  # the standard error for the coefficient on oxygen use. This is the approach 
  # we take below first. Then we do in fact estimate all 34 linear probability 
  # models, just to check whether our estimate of estimation variance was 
  # reasonable.
  
  # We chose to use a linear probability model rather than logistic regression 
  # because of difficulties we encountered manually calculating the standard errors 
  # necessary for a hypothetical treatment. We need a standard error estimate to 
  # plug into the classical power calculations. When attempting these calculations
  # for logistic regression, the math would not come out correctly at the level of 
  # individual expeditions. We were using the negative inverse of the Hessian matrix 
  # of the log likelihood function. This approach worked with other logistic regressions
  # we tried on other sample data, but not our data. Linear probability models
  # did not have this problem. Perhaps the asymptotic behavior of these models is
  # somehow different.

  # First, we need to calculate some summary metrics across the 34 expeditions
  t <- model_data %>% 
       group_by(expid) %>% 
       summarise(count_climbers = n(), prop_mo2used = mean(mo2used))
  mean_climbers <- mean(t$count_climbers); mean_climbers # ~26 climbers/expedition on avg.
  x_bar <- mean(t$prop_mo2used); x_bar # ~60% of expedition members typically used sup. oxygen
  assumed_num_using_mo2 <- ceiling(x_bar*mean_climbers); assumed_num_using_mo2 # assumed number who typically use oxygen
  x <- c(rep(1, assumed_num_using_mo2), rep(0, 26 - assumed_num_using_mo2)); x # assumed expedition vector of sup. oxy. use
  sigma_y <- m1_random_effects[4,'sdcor']; sigma_y # estimated residual variance from m1
  sigma_beta_estimation <- 1/sqrt(sum((x - x_bar)^2))*sigma_y; sigma_beta_estimation # estimation variance for typical Beta_j
  
  # Check our estimation variance against distribution of std. errors from actually
  # fitting a linear probability model for each expedition
  sorted_expid <- sort(unique(model_data$expid))
  n <- length(sorted_expid)
  l <- list()
  for (i in 1:n) {
    l[[i]] <- lm(death ~ mo2used, 
                 data = subset(model_data, expid == sorted_expid[i]))
  }
  mo2used_se <- unlist(lapply(l, function(m) sqrt(vcov(m)['mo2usedTRUE','mo2usedTRUE'])))
  hist(mo2used_se, prob = TRUE,
       main = 'Distribution of Standard Errors for Oxygen Use',
       xlab = 'Std. Error') # we see that our "typical" estimate of estimation variance aligns with these results
  abline(v = sigma_beta_estimation, col = 'red') # our "typical estimate" plotted on the histogram
  
  # Now we can calculate the variance for all Beta_j's (coefficient on oxygen use), 
  # incorporating variance in both the true Beta_j's and variance due to estimation
  sigma_beta <- m1_random_effects[2,'sdcor']; sigma_beta # variance across true Beta_j's, estimated from m1
  sigma_beta_j <- sqrt(sigma_beta^2 + sigma_beta_estimation^2); sigma_beta_j # estimated total variance across Beta_j's  
  
  # Finally, we can plug our results into the following formula. This formula is the
  # result of solving the standard error formula for a treatment estimate in a group-
  # randomized model for number of groups randomized:
  #           J = (2*2.8*variance/(true effect size))^2  
  J_classical <- (2*2.8*sigma_beta_j/true_effect)^2; J_classical
  
    # These results suggest that we would need about 80 expeditions to be randomized
    # to our treatment and control conditions, for 80% probability of picking up on
    # on our hypothesized true effect using alpha = 0.05 (i.e., 80% power).
  
  
# -----------------------------------------------------
# Simulation-based approach to power
# -----------------------------------------------------

  # For the simulation-based approach, we can re-use the multilevel model fit above.
  # This gave us estimates of the population mean and variance of associations between 
  # supplemental oxygen use and mortality, population mean and variance of an intercept, 
  # and population variance of individual climber errors.

  # We can use these estimates to parameterize multivariate normal distributions
  # from which we can repeatedly sample new, expedition-level parameters, with our
  # assumed true treatment effect baked into the sampling. Then we can use these 
  # parameters to simulate climber-level data for each expedition. Finally, we can
  # fit a model on each dataset and evaluate whether the treatment estimate is
  # significant at 5%. Power is the proportion of iterations that result in a such a
  # result. With this procedure in hand, we can plot what number of expeditions 
  # produces each power level, then find 80% on the graph.

  # Function to create simulated data sets, taking arguments of
  # J expeditions, each with K climbers; note that J should be even
  make_fake <- function(J, K) {
    
    using_oxygen <- rep(c(rep(1, ceiling(K*x_bar)), rep(0, K - ceiling(K*x_bar))), J) # assuming in control group that x_bar of each expedition using oxygen
    expedition <- rep(1:J, each = K) # expedition ids
    treatment <- sample(rep(0:1, J/2))
    treatment1 <- treatment[expedition]
    mo2used <- integer(J*K)
    for (j in 1:J) {
      if (treatment[j] == 1) {
        mo2used[1:K + (j - 1)*K] <- rep(1, K) 
      } else {
        mo2used[1:K + (j - 1)*K] <- using_oxygen[1:K + (j - 1)*K]
      }
    }
    
    # Hyperparameters for population of expeditions
    gamma.0.true <- as.vector(m1_fixed_effects['(Intercept)'])
    gamma.1.true <- true_effect
    sigma.a.true <- m1_random_effects[1,'sdcor']
    mu.b.true <- as.vector(m1_fixed_effects['mo2usedTRUE'])
    sigma.b.true <- sigma_beta    
    sigma.y.true <- sigma_y
    
    # Expedition-level parameter draws
    # note: claiming treatment of requiring oxygen affects intercept for treated expeditions, because oxygen use becomes constant, can't estimate separate beta
    a.true <- rnorm(J, gamma.0.true + gamma.1.true*treatment, sigma.a.true) 
    b.true <- rnorm(J, mu.b.true, sigma.b.true)
    for (i in 1:length(a.true)) { # negative intercepts need to be truncated at 0, in order to parameterize Binomial
      if (a.true[i] < 0) {
        a.true[i] <- 0
      }
    }
    
    # Generating climber-level data using expedition-level parameter draws
    y <- numeric(J*K)
    for (j in 1:J) {
      if (treatment[j] == 1) {
        draws <- rbinom(K, 1, a.true[j])
      } else {
        p_vec <- a.true[j] + b.true[j]*using_oxygen[1:K + (j - 1)*K]
        for (i in 1:length(p_vec)) {
          if (p_vec[i] < 0) {
            p_vec[i] <- 0
          }
        }
        draws <- rbinom(K, 1, p_vec) 
      }
      y[1:K + (j - 1)*K] <- draws
    }
  
    return(data.frame(y, mo2used, expedition, treatment1))
    
  }
  
  # Function that calls data-simulation function and then estimates treatment
  # effects, returning the proportion of iterations that yielded a statistically
  # significant result (i.e., power)
  powe.R <- function(J, K, n_sims = 1000) {
    signif <- rep(NA, n_sims)
    for (s in 1:n_sims) {
      fake <- make_fake(J, K)
      lme.power <- lmer(y ~ mo2used + treatment1 + (1 + mo2used | expedition) , data = fake) # note that treatment1 here is just affecting intercept
      theta.hat <- fixef(lme.power)['treatment1']
      theta.se <- sqrt(vcov(lme.power)['treatment1', 'treatment1'])
      signif[s] <- abs(theta.hat/theta.se) > 2
    }
    power <- mean(signif)
    return(power)
  }
  
  # Generating power as a function of expeditions randomized, assuming 27 climbers per expedition,
  # which was roughly the mean for Everest climbs
  J_vec <- seq(50, 1200, by = 50); J_vec
  power_vec <- sapply(J_vec, function(x) powe.R(J = x, K = 27)) #! warning to graders: this could easily take 2 hours to run
                                                                #  and will likely generate some model fit warnings
  
  # Plotting results
  ggplot(data.frame(J_vec, power_vec)) + 
    geom_point(aes(x = J_vec, y = power_vec)) + 
    geom_line(aes(x = J_vec, y = power_vec)) + 
    xlab('Number of Expeditions') + 
    ylab('Power') + 
    ggtitle('Power as a Function of Expeditions Randomized') + 
    geom_vline(aes(xintercept = J_classical), col = 'red') + 
    geom_vline(aes(xintercept = 1050), col = 'blue') + 
    geom_hline(aes(yintercept = 0.8), col = 'black')
    

  # Simulation results suggest that for 80% power, we would need to randomize a 
  # little over 1,000 expeditions. This is an order of magnitude different from 
  # the ~80 expeditions our classical approach suggested.
  
  # We believe that the discrepancy can be explained by the choice of functional
  # form: we chose to use a linear probability model instead of a logistic regression
  # model. This choice was very intentional; the reasons are explained earlier.
  # The inverse logit function bounds probability between 0 and 1, of course.
  # Not so for the linear probability model, whose identity link function permits
  # probability predictions greater than 1 and less than 0. Of particular concern
  # to us are the predictions below 0. These were not rare, because mortality is
  # thankfully low on most expeditions, even when examining only those expeditions
  # during which at least one person died. We then assumed that treatment reduced
  # this risk still further, by 7.4 percentage points, further exacerbating the 
  # problem. We cannot sample from a Bernoulli distribution for a probability value 
  # less than 0. So we chose to truncate any negative probability predictions at 0. 
  # We suspect this had the practical effect of reducing our assumed true treatment 
  # effect of -0.074 to something much lower. All else equal, shifting one's
  # assumed true treatment effect toward zero reduces power, because you are no longer 
  # going to have a true result as many standard deviations from your comparison point. 
  # If you still want 80% power -- as in to have a true treatment effect 2.8 standard 
  # deviations from your comparison point -- then you must increase your sample size. 
  # Dramatically, in our case.
  
  # Even though the simulation approach yielded much greater sample size requirements,
  # it is clearly a superior approach. It is more flexible. Had we only wanted to 
  # take the simulation approach, we could have used a multilevel logistic regression
  # model to estimate population parameters. But because of difficulties with the 
  # classical approach, we had to revert to using a multilevel linear probability 
  # model as the basis for this exercise. Moreover, the simulation approach makes
  # it much harder to fool oneself. We mean this in the sense that the simulation
  # approach actually requires re-generating data many times, which means one must
  # think carefully about the data-generating process in the first place. It was
  # our trouble sampling from Bernoulli distributions with linear probability 
  # predictions less than 0 that alerted us in the first place to the issues that
  # functional form was causing. Contrary to what many economists like to teach
  # in class, the differences between logistic and linear models of a binary out-
  # come can be critical in some circumstances.
  