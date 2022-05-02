################################################################################
# Otzi's Adventures: helping you achieve Himalaya supremum in a compact budget
################################################################################
# Install packages
install.packages("survival")
install.packages("survminer")

# Load libraries
library(dplyr)
library(lme4)
library(ggplot2)
library(fitdistrplus)
library(tidyverse)
library(stats4)
select <- dplyr::select
library(survival)
library(readr)
library(survminer)

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

## Descriptive Statistics of Climbing Data
# Read in dataframe 
climbers_full <- read_csv("Downloads/climbers_full.csv")

attach(climbers_full)
# Climbers-Specific 
  # Sex
  table(sex)
  table(sex)/length(sex) # About 90% of climbers to date are male. 
  # Age
  table(calcage) # looks like we need to convert values of 0 to missing data 
  hist(calcage) # The majority of climbers are mid-working age adults. 
  # Citizen 
  table(citizen) # This is certainly freetext and not standardized. 
  # Status 
  table(status) # Again a freetext field, but we see the majority are labled as  "Climbers", "H-A Worker", or "Leader"
  # Leader
  table(leader)
  table(leader)/length(leader) # About 13% of climbers are considered leaders of the expedition. 
# Expedition 
  # Peak 
  table(peakid)
  table(peakid)/length(peakid) # # The majority of climbers are climbing Everest (~52%), followed by Manaslu (~17%), and Cho Oyu (~13%).
  # Year
  table(myear)
  table(myear)/length(myear) # Our observations are relatively well distributed across the 11 years of data. 
  # Season
  table(mseason)
  table(mseason)/length(mseason) # 72% of climbers climb in the spring and 28% climb in the fall. Almost no one attempts to climb in summer (monsoon season) or winter.
  # Personal High Point
  hist(mperhighpt) # looks like we need to convert values of 0 to missing data 
  # We can tell that not all climbers reach the summit since our dataset only contains 8000m peaks, and we have personal high points that are sub-8000m. 
  # Summit Time 
  hist(as.numeric(msmttime1)) # Looks like most climbers that summit do so in the early morning to mid-morning. 
# Complications
  # Death
  table(death)
  table(death)/length(death) # ~1% mortality rate in observations. This is good news for climbers, but might make it difficult to do any analyses that compare deaths.
  # Reason for Death 
  table(deathtype) # Need to convert to factor with factor labels. There is decent variation in death type but a large number of deaths attributed to avalanche (apparently there was a historic avalanche on Everest in this time period).
  # Injury
  table(injury)
  table(injury)/length(injury) # ~1.6% morbidity rate in observations 
  # Injury Type 
  table(injurytype) # Need to convert to factor with factor labels. Exposure is the most common injury experienced. 

## Data Cleaning 
# Change various parts of dataframe based off exploratory analyses 
climbers_full <- climbers_full %>%
  dplyr::mutate(calcage = ifelse(calcage == 0, NA, calcage), 
                mperhighpt = ifelse(mperhighpt == 0, NA, mperhighpt),
                mperhighpt = ifelse(mperhighpt > 8167 & peakid == "DHA1", 8167, mperhighpt),
                mperhighpt = ifelse(mperhighpt > 8849 & peakid == "EVER", 8849, mperhighpt),
                deathtype = ifelse(deathtype == 0, NA, deathtype), 
                injurytype = ifelse(injurytype == 0, NA, injurytype))

climbers_full$deathtype <- climbers_full$deathtype %>%
  factor(levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
         labels=c("AMS", "Exhaustion", "Exposure", "Fall", "Crevasse", "Icefall", "Avalanche", "Falling Rock/Ice", "Disappearance", "Illness", "Other", "Unknown") )

climbers_full$injurytype <- climbers_full$injurytype %>%
  factor(levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
         labels=c("AMS", "Exhaustion", "Exposure", "Fall", "Crevasse", "Icefall", "Avalanche", "Falling Rock/Ice", "Disappearance", "Illness", "Other", "Unknown") )

# Create additional variables from existing data
climbers_full <- climbers_full %>%
  dplyr::mutate(age_grp = case_when(calcage < 18 ~ 1, # standardized age groups
                                    calcage >= 18 & calcage < 35 ~ 2,
                                    calcage >= 35 & calcage < 49 ~ 3,
                                    calcage >= 50 & calcage < 65 ~ 4, 
                                    calcage >= 65 ~ 5)) %>%
  group_by(expid) %>%
  mutate(exp_members = n(),
         exp_sex = mean(sex == "M"), # proportion of expedition members that are male
         exp_retire = mean(age_grp == 5), # proportion of expedition members that are retirement age
         exp_wrk_age = mean(age_grp == 2 | age_grp == 3 | age_grp == 4), # proportion of expedition members that are working age
         exp_leaders = mean(leader == TRUE), # proportion of expedition members that are leaders
         exp_max_ht = max(mperhighpt)) %>% # highest point reached by anyone on the expedition
  ungroup %>%
  dplyr::mutate(rel_max_ht = mperhighpt/exp_max_ht) # each member's high point relative to highest point reached by anone on the expedition

climbers_full$age_grp <- climbers_full$age_grp %>%
  factor(levels = c(1, 2, 3, 4, 5),
         labels = c("<18", "18-34", "35-49", "50-64", "65+"))



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


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Let's try to model a variable that are measured on a scale between 0 and 1 with a beta distribution
# Proportion of Males on Expedition
exp_sex_mu = mean(climbers_full$exp_sex); exp_sex_mu # mean
exp_sex_var = var(climbers_full$exp_sex); exp_sex_var # variance

# Construct alpha and beta from mean and variance 
alpha <- ( (exp_sex_mu^2 - exp_sex_mu^3)/exp_sex_var ) - exp_sex_mu; alpha # 4.6753
beta <- (alpha/exp_sex_mu) - alpha; beta # 0.5126206

# Overlay beta distribution over actual data 
hist(climbers_full$exp_sex, prob=TRUE, main = "Histogram", xlab = "Proportion of Expedition Members who are Male")
# Expeditions are highly concentrated with male climbers. 
curve(dbeta(x, alpha, beta), add = TRUE, col = "red")
# The curve of the beta distribution appears to approximately fit the distribution. 

# Let's run chi-sq analysis to see exactly how well the beta distribution might fit.
beta_deciles <- qbeta(seq(0, 1, 0.01), alpha, beta); beta_deciles

# We have lots of observations so we can have a large number of bins and still have very large expected counts. 
bins <- numeric(100)

# Observed counts in each bin
for (i in 1:100) {
  bins[i] <- sum((climbers_full$exp_sex >= beta_deciles[i] & climbers_full$exp_sex < beta_deciles[i+1]))
}
bins

# Expected counts in each bin 
exp_counts <- rep(length(climbers_full$exp_sex)/100, 100); exp_counts

# Calculate chi-squared statistic 
chi_sq_stat <- sum( (bins-exp_counts)^2 / exp_counts ); chi_sq_stat # 15302.89, offhand this appears large 
pchisq(chi_sq_stat, df=97, lower.tail = FALSE) # df = 100 bins - 2 parameters - 1 calculation of chi sq stat
# Our p-value is so small that it is calculated as 0. This means that the beta distribution does not appropriately model the distribution of proportion of expedition members who are male. 

# We can confirm this result with the Q-Q plot 
# Calculate deciles from actual data 
data_deciles <- quantile(climbers_full$exp_sex, seq(0, 1, 0.01), type=2); data_deciles

# Plot deciles of beta distriubtion against deciles from data 
plot(beta_deciles[1:100], data_deciles[1:100], xlim=c(0,1), ylim=c(0,1), xlab = "Deciles of Beta Distribution", ylab="Deciles of Data", main = "Q-Q Plot")
y <- function(x) x
curve(y, to=1, col="red", add=TRUE)
# Our data points of the deciles aren't linear, so our p-value and conclusion makes sense. 
# It is likely that the reason the beta distribution is failing to be appropriate is because 36% of our observations are at 1 and because though the proportion is a continuous variable, to some extent it is taking discrete values.



################################################################################
# Miscellaneous Analysis
################################################################################

#############################################
### Adapting a survival plot to understand our data 
#############################################

## Survival Plots 

# We can repurpose an epidemological plot, called "Survival Plot." This will provide us a graphical representation of how high climbers make it up the mountain. 
ever <- climbers_full %>%
  filter(peakid == "EVER" & mperhighpt != 0) %>% # We will focus on climbers on Everest who have a reported personal high climbing point.
  dplyr::mutate(status = 1)

hist(ever$mperhighpt) # Most of our climbers get very close to the summit of Everest. 
table(ever$mperhighpt)

# For all climbers
ggsurvplot(
  fit = survfit(Surv(mperhighpt, status) ~ 1, data = ever), 
  main = "Climbing Everest",
  xlab = "Height (in m)", 
  ylab = "Overall probability")
# Looks like about 75% of our sampled climbers are reaching the peak of Everest. 

# By sex
ggsurvplot(
  fit = survfit(Surv(mperhighpt, status) ~ strata(sex), data = ever), 
  conf.int = TRUE, 
  title = "Climbing Everest",
  xlab = "Height (in m)", 
  ylab = "Overall probability")
# Looks like females reach higher points of Everest at slighly lower rates than males. 

summary(survfit(Surv(mperhighpt, status) ~ strata(sex), data = ever), times = 8000)

# By age-groups
ggsurvplot(
  fit = survfit(Surv(mperhighpt, status) ~ strata(age_grp), data = ever), 
  conf.int = TRUE, 
  title = "Climbing Everest",
  xlab = "Height (in m)", 
  ylab = "Overall probability")
# It looks like climbing success decreases as you increase the age group. Those under 18 are the most sucessful with over 80% summiting (though this is likely a selection effect), but those over 65 summit at about a rate of 25%

# By leader status
ggsurvplot(
  fit = survfit(Surv(mperhighpt, status) ~ strata(leader), data = ever), 
  conf.int = TRUE, 
  title = "Climbing Everest",
  xlab = "Height (in m)", 
  ylab = "Overall probability")
# Leaders have about a 60% probability of summiting, while non-leaders have about a 75% probability. I wonder if there is a practical reason for this, like leaders do many trips so don't need to summit everytime or leaders are responsible for straglers on the expedition who can't make it to the peak.

# By year 
ggsurvplot(
  fit = survfit(Surv(mperhighpt, status) ~ strata(myear), data = ever), 
  conf.int = TRUE, 
  title = "Climbing Everest",
  xlab = "Height (in m)", 
  ylab = "Overall probability")
# No one summits in 2015 because of a major avalanche on Everest. Great to see that our data is reflecting known realities.

# By season
ggsurvplot(
  fit = survfit(Surv(mperhighpt, status) ~ strata(mseason), data = ever), 
  conf.int = TRUE, 
  title = "Climbing Everest",
  xlab = "Height (in m)", 
  ylab = "Overall probability")
# No only is the spring the most popular season to climb, it appears to the only season where we observe people actually summitting. 

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

##############################################
### Climbing Success of Busines People vs. Alpinists ###
##############################################

# Let's compare the success rates of summitting for business people vs professional alpinists 
# Using simulation approach 

# Use fuzzy text matching on the free-text occupation
biz <- agrep("business", climbers_full$occupation)
alp <- agrep("alpine", climbers_full$occupation)

climbers_full <- climbers_full %>%
  mutate(index = row_number()) 

# Create datasets of just business people and just professional alpinists
businesspeople <- climbers_full %>%
  subset(index %in% biz) %>%
  mutate(occ = "business")
samp_size <- nrow(businesspeople)
alpinists <- climbers_full %>%
  subset(index %in% alp) %>%
  mutate(occ = "alpinist")

# Crate dataset with both businesspeople and alpinists
occ <- rbind(businesspeople, alpinists)
occ_length <- nrow(occ)

# Construct observed difference in avg proportion of business people who summit and avg proportion of alpinists who summit
BizAvg <- mean(as.numeric(businesspeople$msuccess)); BizAvg # 41.7% of business people summit 
AlpAvg <- mean(as.numeric(alpinists$msuccess)); AlpAvg # 47.4% of alpinists summit 
obs <- BizAvg - AlpAvg; obs # Businesspeople summit at a rate that is 5.7% lower than alpinists. 

# Permutation test
N <- 10000
diffs <- numeric(N)    #empty vector to hold difference in percent success
for (i in 1:N){
  index <- sample(occ_length, samp_size)
  bizavg <- mean(as.numeric(occ[index,]$msuccess))
  alpavg <- mean(as.numeric(occ[-index,]$msuccess))
  diffs[i] = bizavg - alpavg 
}

# Plot permutation results 
hist(diffs, main = "Histogram of Simulated Rate Differences", xlab= "Differnce in Summit Rate for Business People vs Alpinists")
abline(v=obs, col="red") # observed 
# Our observed rate difference is certainly on the tail of the histogram. 
(sum(abs(diffs) > abs(obs))+1)/(N+1)
# Only 3.7% of our simulated rate differences are as great in magnitude as our observed difference. At at 5% significance level, we would reject the null hypothesis that there is no difference in summitting rates between business people and alpinists. 
(sum(diffs < obs)+1)/(N+1)
# Ony 1.7% of our simulate rate differences are more negative that the observed difference. This would be a one-way t-test and provide evidence that business people summit at a lower than professional alpinists.

# Let's compare our simulation result against the built-in chi squared function 
chisq.test(occ$occ, occ$msuccess, correct=F)
# Our p-value is 0.03678, so our results and conclusions match!


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

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
### Weather Data 

# Barometric pressure affects oxygen saturation: specifically, lower barometric pressure reduces oxygen saturation. So, analyzing the weather patterns at Everest's basecamp can help us identify times during the hiking season where climbers will potentially require greater supplemental oxygen. 

# Load dataset with hourly weather readings from Everest basecamp starting Nov 1, 2019 to Jun 30, 2021
basecamp <- read_csv("Downloads/Base_Camp/Base_Camp_20210630.csv") 

# Reformat and rename variables 
basecamp <- basecamp %>%
  mutate(time = as.POSIXct(TIMESTAMP, format = "%m/%d/%Y %H:%M")) %>% # converts character to date-time 
  rename(humid = RH, # rename columns to more descriptive names 
         temp = T_HMP, 
         pressure = PRESS, 
         precip = PRECIP)

# Create new variables from our date-time variable 
basecamp <- basecamp %>%
  mutate(date = format(time, "%m/%d/%Y"),
         month = format(time, "%m"), 
         day = format(time, "%d"), 
         year = format(time, "%Y"), 
         hr = format(time, "%H"),
         time_24 = format(time, "%H:%M"))

# Construct a dataset with just the spring weather observations (2020 & 2021)
spring_weather <- basecamp %>% 
  filter(month == "03" | month == "04" | month == "05") %>% # the most popular season for expeditions is spring: March - May 
  group_by(date) %>%
  mutate(total_precip = sum(precip)) %>% # total precipitation over the course of a day 
  ungroup %>%
  mutate(rainy_day = as.numeric(total_precip > 0), # rainy day if have any precipitation 
         high_humidity = as.numeric(humid > mean(humid))) # high humidity day if above average humidity 

# Let's see if there are other weather variables that might be easier to measure/predict that help us predict barometric pressure
cor(spring_weather$temp, spring_weather$pressure) # Correlation coefficient is almost 0.58 - that's pretty strong. Temperature might serve as a good predictor for barometric pressure.
cor(spring_weather$humid, spring_weather$pressure)
cor(spring_weather$high_humidity, spring_weather$pressure) # Both of the correlation coefficients for humidity-related variables are under 0.20, might not be the best to start there with creating a model.

# Using a projection matrix approach, let's find the coefficients for a linear regression of barometric pressure on temperature

m1 <- rep(1, length(spring_weather)) # vector of 1s 
m2 <- spring_weather$temp # temperature 
m3 <- spring_weather$pressure # pressure 

A <- cbind(m1, m2) # vector of 1s, temperature
A_t <- t(A) # transpose of A
B <- A_t%*%A # A_t %*% A
B_inv <- solve(B); B_inv # invert B 
coeff <- B_inv%*%A_t%*%m3; coeff # coefficients come from B_inv %*% A_t %*% pressure
b <- coeff[1,1]; b # intercept: 534.027. Interpretation: If it is 0 degrees Celsius, we expect the barometric pressure to be 534.027 hPa
a <- coeff[2,1]; a # slope: 0.347086. Interpretation: For every 1 degree increase in temperature, we expect the barometric pressure to increase by 0.37 hPa.

# Manual calculation of R^2
pred_pressure <- b + a*spring_weather$temp
spring_weather$resid_pressure = spring_weather$pressure - pred_pressure
var_pred <- var(pred_pressure)
var_obs <- var(spring_weather$pressure)
var_pred/var_obs # We can explain about 34% of the variation in pressure by temperature. 

# Let's check this against the built-in function
summary(lm(pressure ~ temp, data = spring_weather)) # Our coefficients and R-squared values match those we manually calculated!

# Let's plot the data and the regression line we calculated 
ggplot(data = spring_weather, aes(x = temp, y = pressure)) + 
  geom_point() + 
  labs(title = "Spring Temperatures and Barometric Pressure at Everest Basecamp",
       x = "Temperature (degrees C)",
       y = "Barometric Pressure (hPa)") + 
  geom_abline(intercept = b, slope = a, color="red", size=1.5)
# Our regression line seems to be capturing the general relationshp between temperature and barometric pressure. 

# Let's double check that a linear relationship is appropriate by examining the residual plot. 
ggplot(data = spring_weather, aes(x = temp, y=resid_pressure)) + 
  geom_point() +
  labs(title= "Residual Plot for Pressure ~ Temperature", 
       x= "Temperature (degrees C)",
       y = "Residual") + 
  geom_hline(yintercept = 0, color ="blue")
# Our residuals aren't necessarily evenly distributed across temperature values. We see some extreme negative residuals (<-5) only between -10 and 0 degrees C, and there is less variation in residual magntitude at the extremes of our temperature range. Nevertheless, the residual plot doesn't suggest that a linear model isn't appropriate.

# Let's try adding a second predictor to our model. 

# Both of our humidity-related variables had much weaker correlation to pressure. But, we can test adding one of them to our model. 
# First, let's check that we would be adding a second predictor that is not highly correlated with temperature, that would pose the issue of multi-colinearity.
cor(spring_weather$humid, spring_weather$temp)
cor(spring_weather$high_humidity, spring_weather$temp) # Neither humidity measure appears to be correlated with temperature. 

# Okay, so let's try adding a second predictor variable that is a binary variable: high_humidity.

# Because we are using a factor variable, it is easy to add a visualization of a 3rd variable. We can start here to see what results we might expect by following the same steps as above.
ggplot(data = spring_weather, aes(x = temp, y = pressure, color = as.factor(high_humidity))) + 
  geom_point() + 
  labs(title = "Spring Temperatures and Barometric Pressure at Everest Basecamp",
       x = "Temperature (degrees C)",
       y = "Barometric Pressure (hPa)",
       color = "High Humidity") 
# It's not readily apparent that we should expect high humidity to have statistical significance as a predictor in our model. There is similar variation in both temperature and barometric pressure for high humidity and non-high humidity days.

# Using projection matrix, 
m1 <- rep(1, length(spring_weather))
m2 <- spring_weather$temp
m3 <- spring_weather$high_humidity
m4 <- spring_weather$pressure

A <- cbind(m1, m2, m3)
A_t <- t(A)
B <- A_t%*%A
B_inv <- solve(B); B_inv #
coeff <- B_inv%*%A_t%*%m4; coeff 
b <- coeff[1,1]; b # Intercept: 533.6646. Interpretation: If it is 0 degrees Celsius and non-high humidity, we expect the barometric pressure to be 533.6646 hPa.
a1 <- coeff[2,1]; a1 # Coefficient on temperature: 0.34624 . Interpretation: For every 1 degree increase in temperature, holding high humidity status constant, we expect the barometric pressure to increase by 0.35 hPa.
a2 <- coeff[3,1]; a2 # Coefficient on high humidity: 0.6470104. Interpretation: Holding temperature constant, we expect high-humidity observations to have barometric pressure 0.65 hPa higher than low-humidity observations.

# Manual calculation of R^2
pred_pressure <- b + a1*spring_weather$temp + a2*spring_weather$high_humidity
spring_weather$resid_pressure = spring_weather$pressure - pred_pressure
var_pred <- var(pred_pressure)
var_obs <- var(spring_weather$pressure)
var_pred/var_obs # We can explain about 35% of the variation in pressure by temperature and high humidity status. This is only 1% higher than if we use only temperature, indicating that we probably shouldn't be using high humidity as a predictor.

summary(lm(pressure ~ temp + high_humidity, data = spring_weather)) # All our coefficients and R-square calculations match the built-in function. 
# It is interesting here that we find the coefficient on high-humidity to be statistically significant with a t value of 11.25. The plot doesn't provide convincing evidence that this might the case, and we end up seeing that high humidity is a weak predictor (both in correlation w/ our response of barometric pressure & contribution to R-squared).

# Since temperature can serve as a predictor for barometric pressure, let's see if we can model temperatures over the spring season.
temp_2020 <- spring_weather[1:2207,]$temp # temperatures in 2020
temp_2021 <- spring_weather[2208:4414,]$temp # temperatures in 2021
avg_temp <- (temp_2020 + temp_2021)/2 # take the average of our 2 years of observations 
month <- spring_weather[1:2207,]$month 
day <- spring_weather[1:2207,]$day
hr <- spring_weather[1:2207,]$hr
avg_spring_temp <- cbind(month, day, hr, avg_temp) # dataframe with avg temp at date and time (i.e. avg temp on April 1 at 06:00)

# We can use Fourier analysis to create models that match our temperature data to varying extents. 

nhours <- nrow(avg_spring_temp); nhours # 2207 
plot(1:nhours, avg_temp, type = "l", main = "Average temperatures in Spring", xlab="Hour index", ylab="Average temperature (in degrees C)") # line plot of temperature over the spring 
# It looks like we are picking up mostly the daily variation in temperature but we can see that temperatures definitely rise as we get to the end of the spring season.

# FUNCTIONS
# Cosine and sine functions with m periods in 2207 hours
myCos <- function(m) cos((1:nhours)*m*2*pi/nhours)
mySin <- function(m) sin((1:nhours)*m*2*pi/nhours)
# Fourier coefficient a_m
coeffA <- function(m){
  sum(avg_temp*2*myCos(m)/nhours)
}
# Fourier coefficient b_m
coeffB <- function(m){
  sum(avg_temp*2*mySin(m)/nhours)
}
# Takes a set number of coefficients and overlays the Fourier plot 
plot_Fourier <- function(ncoeff) {
  FourierA <- sapply(1:ncoeff,coeffA)
  FourierB <- sapply(1:ncoeff,coeffB)
  #Find the amplitude of the sum of the cosine and sine terms
  Fourier <- sqrt(FourierA^2+FourierB^2)
  Fourier
  recon <- mean(avg_temp)
  for (m in 1:ncoeff) {
    recon <- recon + FourierA[m]*myCos(m)+FourierB[m]*mySin(m)
  }
  plot(1:nhours, avg_temp, type = "l", main = "Average temperatures in Spring", xlab="Hour index", ylab="Average temperature (in degrees C)")
  points(1:nhours,recon, type = "l", col = "red",lwd = 2) 
}

# Evaluate the first 100 coefficients to see which ones might be the largest 
#Evaluate ncoeff coefficients
ncoeff <- 100
FourierA <- sapply(1:ncoeff,coeffA)
FourierB <- sapply(1:ncoeff,coeffB)
#Find the amplitude of the sum of the cosine and sine terms
Fourier <- sqrt(FourierA^2+FourierB^2)
Fourier
# The first three coefficients are all greater than 1 and then we have a large coefficient (~2.6) on the 92nd coefficient.

# ncoeff = 1; The first Fourier coefficient is the largest 
plot_Fourier(1) 
# A single oscillation is not terrible, but we are overestimating early in the season and underestimating at the end of the season. 

# ncoeff = 3; The first 3 Fourier coefficients are all large, and this would correspond to the 3 months of data. 
plot_Fourier(3) 
# We are now capturing the temperature trends at the start and end of spring. 

# ncoeff = 13; Even though the 13th Fourier coefficient isn't large, there are approximately 13 weeks in the season so this would pick up weekly trends. 
plot_Fourier(13) 

# ncoeff = 92; The 92nd coefficient is large, reflecting the fact that there are 92 days of data and that daily variation is important.
plot_Fourier(92) 
# We are almost perfectly reflecting our data, though we look to be failing to reflect the full peak of temperature peaks in a day.

# ncoeff = 200; Let's see if 200 coefficients gets us a perfect replication. 
plot_Fourier(200)
# Yes, 200 Fourier coefficients is enough to replicate the actual temperature data. 

# For our purposes of using predicted temperatures to estimate barometric pressure and thus potential supplemental oxygen needs, we should either use the ncoeff = 3 or ncoeff = 13 models. 
# Either way, we see that the lowest temperatures are expected earliest in the season (March into April). Thus, we would expect barometric pressure to be lowest in the early climbing season, so we should focus on selling supplemental oxygen in March/April rather than May. 




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
  # which was roughly the mean for Everest climbs:
  # J_vec <- seq(50, 1200, by = 50); J_vec #! warning: full code here takes 2 hrs to run; will just show a test case below
  # power_vec <- sapply(J_vec, function(x) powe.R(J = x, K = 27))
  J_vec <- 80 # this was our result from classical approach; simulation says this is very inadequate sample size
  power_vec <- sapply(J_vec, function(x) powe.R(J = x, K = 27))
  # ggplot(data.frame(J_vec, power_vec)) + 
  #   geom_point(aes(x = J_vec, y = power_vec)) + 
  #   geom_line(aes(x = J_vec, y = power_vec)) + 
  #   xlab('Number of Expeditions') + 
  #   ylab('Power') + 
  #   ggtitle('Power as a Function of Expeditions Randomized') + 
  #   geom_vline(aes(xintercept = J_classical), col = 'red') + 
  #   geom_vline(aes(xintercept = 1050), col = 'blue') + 
  #   geom_hline(aes(yintercept = 0.8), col = 'black')
    

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
  