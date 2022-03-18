#################################################
#################################################
#### ANOVA: Single Environment Trial analysis
#################################################
#################################################

## Let's use the omer.sorghum from the {agridat} package
## Tip: look at the summary of any data before proceeding with the analysis

#### Data preparation  ---------------------------------------------------------
library(tidyverse)
library(agridat)
data(omer.sorghum)
omer.sorghum
str(omer.sorghum)
view(omer.sorghum)
omer.sorghum <- omer.sorghum %>% 
  as_tibble()
omer.sorghum
summary(omer.sorghum)
levels(omer.sorghum$env)
levels(omer.sorghum$rep)
levels(omer.sorghum$gen)

## We can get first insights
dat.summary <- omer.sorghum %>% 
  group_by(env) %>%
  summarize(
    myield=mean(yield, na.rm=TRUE),
    vyield=var(yield, na.rm=TRUE),
    sdyield=sd(yield, na.rm=TRUE),
    Ngeno=length(unique(gen)),
    Nobs=n(),
  )
dat.summary

## let's consider the first env, E1
levels(omer.sorghum$env)
omer.sorghum.E1 <- omer.sorghum %>% 
  filter(env=="E1") 

summary(omer.sorghum.E1)

omer.sorghum.E1 <- omer.sorghum %>% 
  filter(env=="E1") %>% 
  droplevels()

summary(omer.sorghum.E1)

#### The model  ----------------------------------------------------------------
## Design: RCBD
library(lmerTest)  ## or library(lme4)

################################################
################ Case 1: Genotype is fixed
################################################

# Yield_ij = mean + gen_i + rep_j + error_ij

## same variance among groups
# groups: the genotypes -> genotypes have the same variance
# variance = se * se

model.E1 <- lmer(yield ~ gen + (1 | rep), data = omer.sorghum.E1)
# If the design was alpha lattice
# model.E1 <- lmer(yield ~ gen + (1 | rep) + (1|rep:block), data = omer.sorghum.E1)

summary(model.E1)
test.E1 <- anova(model.E1, ddf = "Kenward-Roger", type = 3)
test.E1 
########## ------------
## test: 
#1st question
# H0 (null hypothesis): all the genotypes are the same (no difference)
# H1(alternative hypo.) : at least two genotypes are different
# if p-value > 0.05, then I consider H0. Stop
# if p-value < 0.05, then I reject H0 (I consider H1). What is(are) the best? -> 2nd question
#2nd question
# Multiple comparisons
########## ------------

## We are also interested to get the means we can estimate from the model, 
## the adjusted means or least square means (lsmeans)
## these are also Best Linear Unbiased Estimations (BLUEs)
## We get the adjusted means with the function emmeans()

#### The adjusted means  -------------------------------------------------------
library(emmeans)
lsmeans.E1 <- emmeans(model.E1, ~gen)
lsmeans.E1

## We can do multiple comparisons of means 
## and see differences between genotypes.
## Each genotype is compared to the other
pairs(lsmeans.E1)

## With a high number of treatment (genotype)
# (18 genotypes. n * (n+1)/2 = 18*19/2=171 comparisons!)
## Better to compare each genotype to only one or two checks
## Suppose G18 is a check, we then want to see 
## what genotypes are different to G18?

comparisons.G18 <- emmeans(model.E1, specs = trt.vs.ctrl ~ gen, ref = 18)
str(comparisons.G18)
names(comparisons.G18)

comparisons.G18$contrasts %>% 
  as_tibble() %>% 
  arrange(desc(estimate))

## Let's consider the data of the environment E3
## Run a One-way ANVOA
## Estimate the adjusted means
## What genotypes are different to G05?

################################################
################ Case 2: Genotype is random
################################################

#### BLUPS  --------------------------------------------------------------------
## If our goal is  selection (identifying the best genotypes) 
## then the genotype should be considered random and 
## Best Linear Unbiased Predictions (BLUPs) estimated
model.E1.r <- lmer(yield ~ (1 | gen) + (1 | rep), data = omer.sorghum.E1)
summary(model.E1.r)

## The BLUPS are estimated using the the fixef and ranef functions
Intercept <- fixef(model.E1.r)[[1]]
BLUPS.E1 <- Intercept + ranef(model.E1.r)$gen
BLUPS.E1 

#### Heritability  -------------------------------------------------------------
## We can estimate the heritability/repeatability "correlations among the reps"
## heritability = genotypic variance / phenotypic variance
## heritability = genotypic variance / (genotypic variance + environment)
## h2 = vg / (vg + ve/nrep)
summary(model.E1.r)
4641.87/(4641.87+1070.61/4)

## Extract variance components (vc)
vc <- model.E1.r %>% 
  VarCorr %>% 
  as_tibble 
vc
h2 <- vc$vcov[1]/(vc$vcov[1] + vc$vcov[3]/nlevels(omer.sorghum.E1$rep))
h2






