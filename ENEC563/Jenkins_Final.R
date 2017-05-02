# Due Date: Tuesday May 9th, 2017
# 
# 
# 
# Problem 1
quinn1 = read.csv("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/quinn1-1.csv", header = TRUE)

# Questions:
# 1.1) Refit your final egg count rate model from Assignment 1 as a Bayesian model.  
# This should be a model with a normally distributed response in which there is a two-factor interaction 
# between Density and Season.
#   1.1a) Provide diagnostics that indicate that the Markov chains have converged 
    #to the corresponding posterior distributions and are properly mixing.
# (Hint, recall that the library rstanarm has equivalent functions for lm,glm,lmer,glmer where you just need to add stan_ 
# to the beginning of those functions.  e.g. stan_lm).
# Hint: For question 1 you may get an error for fitting your model that says "'location' must be specified".  
# For some reason, the stan_lm function requires a prior argument. Use prior=NULL, to remove this error.


# 1.2) Obtain 95% percentile and 95% HPD credible intervals for all the treatment effects.

# 1.3) Graph the estimated posterior distributions for all the parameters in the model.

# 1.4) Obtain estimates of the six treatment means as well as 95% percentile credible intervals for these means. 
# Compare these values to what you obtained in Assignment 2. (You may use the key for Assignment 2 if you wish.)
# 
# 
# 
# Problem 2: 
# This data set contains the results of an experiment set up to determine the effect of temperature on the growth rates of corals. The variables contained in the data set are the following.
# 
# tank: identifies the three different aquaria that serve as replicates for a given temperature treatment. This identifier is not unique.
# temp: is the temperature that a particular aquarium was maintained at for the duration of the experiment. Three temperature levels were used: 25°C, 28°C, and 32°C.
# incr: the gain in dry mass (g) experienced by a coral over one time period.
# id: an identifier that uniquely identifies the different coral animals used in the experiment.
# surf.area: the surface area of the individual corals (cm2) at the start of the experiment.
# tank.grp: a unique identifier that identifies the nine different aquaria used in the experiment.
# inctime: the length of time (days) over which the mass gain (incr) occurred.
# rate: this is the growth rate, the mass gain (incr) divided by time (inctime) reported in mg/day.
# time.period: identifies the time period of the growth. It is numbered 1, 2, and 3 to correspond to the first, second, and third growth rate measurements for a given animal.
# Because of logistical difficulties it was not possible to measure all 162 coral animals on the same day. So, the length of the observational periods varied slightly for some of the animals from different aquaria. To account for this growth increments were divided by the corresponding time interval to yield a growth rate. All analyses were carried out on the growth rates.
# 
# It was suspected that there might be some latency effects of temperature. In particular it was thought that there would be an initial adjustment period during which the animals acclimated to their aquaria after which the animals would begin to exhibit their true response to temperature. So a linear relationship between growth and time was not expected. As a result it was decided to treat time as a categorical variable (time.period) in this analysis.
# 
# Important features of the experimental design
# 
# There are nine aquaria and each aquarium contained 18 different coral animals.
# Growth rates were obtained for each coral animal at three different time points in the study yielding 3 × 18 × 9 = 486 observations. At each time point the growth rate was calculated as the change in dry mass of the animal since the last measurement divided by the length of time. The first growth rate was obtained as the change from the animal's initial dry mass (a value that is not reported in the data you are given).
# Temperature treatments were randomly assigned to aquaria so that there were three aquaria (replicates) at each temperature. The same temperature was maintained in that aquarium for the duration of the experiment. The nine different aquaria used in the experiment are uniquely identified by the variable tank.grp.
# Although effort was made to choose coral animals of roughly the same initial size, this turned out to be impossible. It is suspected that an animal's size might affect its growth rate over time or its response to temperature or both. Because an animal's initial surface area was thought to be a more useful predictor than its initial mass, surface area (cm2) is reported in the data set for each animal at the beginning of the study.
# The primary objective of this exercise is to determine how an animal's growth rate changes over time and to determine if the nature of that change varies with temperature. This needs to be done in a way that controls for any confounding variables while properly accounting for the experimental design.
# 

corals = read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/corals.txt", header = TRUE)

# Questions:
# 2.1) Identify as many of the important statistical experimental design features of this experiment that you can. 
# Be sure to use the language we've presented in this course when naming these design features.
# Hint 1: There are at least three distinct design features you could mention here.
# Hint 2: The basic research question here is whether the average growth rate (rate) 
# is different at different temperatures (temp). 
# Without any design complications it would seem that this question could be addressed with a one-way analysis of variance, 
# or perhaps a two-way ANOVA with temperature and time as potentially interacting factors. 
# Because of the way the experiment was designed such simple analyses are inappropriate. 
# The design features I'm referring to each individually force the analysis to deviate from an ordinary ANOVA in specific ways. 
# If it were possible to add these design features sequentially each addition would cause us to change the form or type of model 
# that we are fitting from what it was without it.


# 2.2) Fit a sensible starting model for this experiment that properly accounts for all the design features you mentioned 
# in Question 1.
# Hint: Some of the factor variables have numeric codes for their categories. 
# Don't slip up and treat these variables as numeric in your model. 
# You don't know that the growth rate is monotonic with temperature or with time.


# 2.3) Simplify the model you fit in Question 2 so that all the remaining terms are statistically significant.


# 2.4) Obtain estimates of the mean growth rates during each time period separately by temperature 
# and obtain 95% confidence intervals for the means. 
# Carry out these calculations for a typical coral animal with an initial surface area of 10 cm2, 
# a value that is close to the mean initial size of animals used in the study.
# Hint 1: Sometimes a convenient way to fit a model so that the estimated mean corresponds 
# to a specific value of a covariate is to center the covariate at that value when you fit the model. 
# This is particularly convenient here when coupled with Hint 2 because it allows you to ignore some of the "nuisance" terms
# in your final model when obtaining confidence intervals for the terms you care about.
# Hint 2: You might also consider using the trick in which you leave out selected terms from the model 
# causing R to estimate the same model but with a different parameterization. 
# If you do this the right way you can then easily obtain the confidence intervals for the means you need 
# using a standard function for this type of model. 
# The model you want has only the two 2-factor interactions in it and nothing else 
# (no intercept, no main effects). Also, to get this to work you need to list as the first term in this model 
# the interaction term whose values correspond to the individual estimates you want. 
# The nuisance interaction term (the one that will drop out when surface area = 10) should be second. 
# Be sure to verify that the model you get has the same AIC as the regularly parameterized final model.
# Hints 1&2 add up to including a model to replace surf.area with I(surf.area-10) in your original model 
# and removing the intercept from your model . 
#You can then use intervals(newmodelname,which="fixed") to calculate the mean and confidence intervals 
# for all treatment combinations.   
# Hint 3: You can also set up a contrast matrix and get the standard errors with a sandwich expression 
# involving the variance-covariance matrix.


# 2.5) Prepare a suitable graph that shows the individual treatment means and the confidence intervals 
# that you calculated in Question 4 and permits easy comparison of the effect 
# of temperature on the mean growth rates over time.




# Problem 3
# The phenomenon of industrial melanism is one of the textbook examples of natural selection in action. The relative prevalence of two natural color morphs of the moth Biston betularia in England has changed over time apparently in response to industrial air pollution. The dark morph flourishes in polluted areas where tree trunks are darkened by soot while the light morph flourishes in less polluted areas where tree trunks are lighter. Bishop (1971) investigated a naturally occurring cline of Biston betularia extending from industrial Liverpool (most polluted) to the rural countryside of North Wales (least polluted). One of the analyses he carried out is the subject of this exam question.
# 
# Bishop selected seven woodlands (Location) at varying distances (Distance) from Liverpool (in km). He describes his experimental protocol as follows, p. 224-225.
# 
# In June and early July 1966 and 1967, eight trees (occasionally sixteen) were selected at random at each of several localities every day. Equal numbers of frozen typical and carbonaria moths were glued to these in life-like positions. It was assumed that these moths would be subject to predation by birds in the manner observed by Kettlewell (1955). The moths were placed on a different aspect of the tree-trunks each day at heights of from 0.5 to 2. m. The position of each moth was noted and after 24 hrs a record was made as to whether or not it had been removed (or preyed upon). Remaining moths were then detached and the process was repeated with fresh moths on a different random series of trees. The long duration of the experiment meant that predation occurred over a range of weather conditions.
# Unfortunately the raw data are unavailable. Instead we have the results summarized by site for each Morph. The variable Num_moths records how many total moths of a particular Morph were exposed to predation at a particular site and the variable Num_removed records the number of these individuals that was removed presumably by predators. The question of interest is whether the predation rate  (proportion of individuals removed) varies with the distance from Liverpool and, more importantly, whether this relationship is different for the two color morphs.
# 

moths = read.csv("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/moths.csv", header = TRUE)

# Questions:
# 3.1) A single regression model involving the predictors Distance and Morph can answer the researcher's question of interest. 
# Write down that regression model in generic form. What I want here is the expression that would appear on right side 
# of a regression equation written out as a sum of parameters times predictors.
# Hint 1: I'm looking for an expression of the form ??0 + ??1x + ., 
# where you should include as many terms as are needed to describe the basic outlines of the experiment 
# and answer the researcher's question. Be sure to identify what the variables in your expression represent.
# Hint 2: I'm not asking you to include the complications discussed in Question 6 at this point.



# 3.2) Using the expression you've written as your answer to Question 1, state a null hypothesis 
# in terms of model parameters that directly tests whether the relationship between predation rate and distance is the same 
# for the two morphs.



# 3.3) Given the nature of the response variable, fit an appropriate regression model 
# that addresses the researcher's primary question.


# 3.4) Test the overall fit of the model of Question 3 using an appropriate goodness of fit test. 
# Verify that the test is appropriate.


# 3.5) There's a structural characteristic of these data that we've been ignoring that may be making the data heterogeneous. 
# The structure is represented by a variable in the data set. What am I talking about?


# 3.6) Refit your model from Question 3 but this time also account for the structure of the data. 
# In reference to this structure, which variable in your model is a level-1 variable and which variable is a level-2 variable?


# 3.7) The statistical evidence for this structure turns out to be very weak. 
# Demonstrate this either by carrying out a formal statistical test or by citing relevant statistics. 
# (Note: We'll continue to use the model with structure in the remaining questions anyway.)
# Hint 1: If you elect to carry out a statistical test using the likelihood 
# you need to be aware that in the hypothesis test you're carrying out, 
# H0: ??2 = 0, zero is a boundary value for ??2. 
# The usual distribution of the likelihood ratio statistic is incorrect for boundary values. 
# The p-value adjustment that we used for testing H0: ?? = 0 in a negative binomial model (Page 3 on  lecture 17) 
# is the same adjustment you need to carry out here.


# 3.8) Using the model from question 6, compute a statistic that compares the odds of being removed 50 km away 
# from Liverpool to the odds of being removed in Liverpool (0 km away). 
# Calculate this statistic separately for the dark and light morphs and interpret your results.


# 3.9) Using the model from question 6, produce a graph that summarizes the results of the analysis as follows.
#   a) Plot the empirical proportions (the observed proportions of moths eaten) as a function of distance. 
#   Distinguish the plotted values by their Morph type.


#   b) Plot the predicted probability of being eaten as a function of distance using only 
#   the fixed effect estimates from your model. 
#   Display these as curves superimposed on your scatter plot of empirical probabilities


#   c) Plot the predicted probability of being eaten as a function of distance using both 
#   the fixed effect estimates and the random effect predictions. 
#   Plot these as points being sure to distinguish them from the points you plotted in (a).


#   d) Label your diagram appropriately using a coherent set of colors and symbol types.


# 3.10) It was mentioned in the background section above that the data we are using are tabulated versions 
# of the raw data from each location for each morph. 
# The actual observations are the number of moths removed out of a fixed total on separate trees at a given location. 
# The results for individual trees were then combined to yield the data we used for this analysis. 
# Which of the four basic assumptions of the probability model that we've been using is the one that is most likely to be 
# (but not necessarily) violated by combining data in this fashion?                                                                                                                                                                             