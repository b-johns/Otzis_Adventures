################################################################################
# Otzi's Adventures
################################################################################

# Load libraries
library(dplyr)
library(lme4)
library(ggplot2)

# Load data
df <- read.csv('climbers_full.csv')







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
  