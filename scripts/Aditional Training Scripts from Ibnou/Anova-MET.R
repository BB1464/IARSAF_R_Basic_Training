#################################################
#################################################
#### ANOVA: Multi Environment Trial analysis
#################################################
#################################################

## Let's consider the omer.sorghum data of the agridat package, 
## a multi-environment trial of sorghum conducted in Sudan
library(tidyverse)
### To load the data
## option1:
library(agridat)
data(omer.sorghum)

omer.sorghum <- omer.sorghum %>% 
  as_tibble()

omer.sorghum

summary(omer.sorghum)
levels(omer.sorghum$env)
levels(omer.sorghum$rep)
levels(omer.sorghum$gen)

table(omer.sorghum$env)
table(omer.sorghum$gen)
table(omer.sorghum$gen, omer.sorghum$env)

## We can get first insights
dat.summary <- omer.sorghum %>% 
  group_by(env) %>%
  summarize(
    myield=mean(yield, na.rm=TRUE),
    vyield=var(yield, na.rm=TRUE),
    sdyield=sd(yield, na.rm=TRUE),
    Ngeno=length(unique(gen)),
    Nobs=n(),.groups = 'drop'
    )

dat.summary

#### The model  ----------------------------------------------------------------
## Design: RCBD
library(lmerTest)

################################################
################ Case 1: Genotype is fixed
################################################

# Yield_ijk = mean + gen_i + rep_j + env_k + gen*env_ik + error_ijk
# genotype i
# rep j
# environment k

model <- lmer(yield ~ gen + env + gen:env + (1 | env:rep), omer.sorghum)
# rep is nested into env
# gen*env = gen + env + gen:env
# model <- lmer(yield ~ gen*env + (1 | env:rep), omer.sorghum)
test <- anova(model, ddf = "Kenward-Roger", type = 3)

test

# If the design was alpha lattice
#model.r <- lmer(yield ~ gen + env + gen:env + (1 | env:rep) + (1 | env:rep:block), omer.sorghum)
## block nested into that is nested into env

#### The adjusted means  -------------------------------------------------------
## We can get the estimated means of each gen in each env with the function emmeans
library(emmeans)
lsmeans <- emmeans(model, ~gen:env)
#emmeans(model, ~gen)
lsmeans

## In presence of interaction, we can do multiple comparisons of the levels of 
## one factor within the levels of another (within group comparisons)
## Then could be interesting to compare the genotypes for each level of env

lsmeans.gen.env <- emmeans(model, specs = pairwise ~ gen | env)
lsmeans.gen.env

## Let’s consider G18 as a check and compare each gen to G18 within each env

comparisons.G18 <- emmeans(model, specs = trt.vs.ctrl ~ gen | env, ref = 18)
comparisons.G18$contrasts

################################################
################ Case 2: Genotype is random
################################################

#### The BLUPS  ----------------------------------------------------------------
## Let's consider all the factors as random
model.r <- lmer(yield ~ (1|gen) + (1|env) + (1|gen:env) + (1|env:rep), omer.sorghum)
summary(model.r)
## In the case of Alpha Lattice design
#model <- lmer(yield ~ (1|gen) + (1|env) + (1|gen:env) + (1|env:rep) + (1|env:rep:block), data=omer.sorghum)

## The BLUPS are estimated using the the fixef and ranef functions
Intercept <- fixef(model.r)[[1]]

## Lets' take a look at the random coefficients
ranef(model.r)

coefs.gen <- ranef(model.r)$gen %>% 
  as.data.frame() %>% 
  rename(Means.G="(Intercept)") 
dim(coefs.gen)
rownames(coefs.gen)
coefs.gen <- coefs.gen %>% 
  rownames_to_column(var = "G") %>% 
  as_tibble()

coefs.env <- ranef(model.r)$env %>% 
  as.data.frame() %>% 
  rename(Means.E="(Intercept)")
coefs.env <- coefs.env %>% 
  rownames_to_column(var = "E") %>% 
  as_tibble()

coefs.gen.env <- ranef(model.r)$`gen:env` %>% 
  as.data.frame() %>% 
  rename(Means.GE="(Intercept)") 
coefs.gen.env <- coefs.gen.env %>% 
  rownames_to_column(var = "Effect") %>% 
  as_tibble() %>% 
  separate(Effect, into=c("G", "E"), sep=":")

BLUPS.gen.env <- coefs.gen.env %>% 
  left_join(coefs.env) %>% 
  left_join(coefs.gen) %>% 
  mutate(BLUPS = Intercept + Means.GE + Means.E + Means.G) %>% 
  select(G, E, BLUPS)

BLUPS.gen.env

#### Heritability  -------------------------------------------------------------
## We can estimate the combined heritability
## h2 = vg / (vg + vgxe/nenv + ve/(nenv*nrep))
# nenv: number of env
# nrep: # of reps
# H2 Cullis is an alternative. handle
# better unbalacande data, based on sed

summary(model.r)
1169/(1169 + 21343/6 + 24660/(6*4))

## Extract variance components (vc)
vc <- model.r %>% 
  VarCorr %>% 
  as_tibble 
vc
vg <- vc %>% 
  filter(grp=="gen") %>% 
  select(vcov) %>% 
  as.numeric()
vgxe <- vc %>% 
  filter(grp=="gen:env") %>% 
  select(vcov) %>% 
  as.numeric()
ve <- vc %>% 
  filter(grp=="Residual") %>% 
  select(vcov) %>% 
  as.numeric()

h2 <- vg / (vg + 
              vgxe/nlevels(omer.sorghum$env) + 
              ve / (nlevels(omer.sorghum$env) * nlevels(omer.sorghum$rep))
            )
h2

#### Biplot  -------------------------------------------------------------------
## Let's have the data in wide format: genotypes in rows and env in columns
BLUPS.gen.env
blups.biplot <- BLUPS.gen.env %>% 
  pivot_wider(names_from = E, values_from = BLUPS) %>% 
  column_to_rownames("G")
blups.biplot

pca <- prcomp(blups.biplot, scale = TRUE, center = TRUE)

library(factoextra)
fviz_pca_biplot(pca, col.ind="blue", col.var="red", title="")




