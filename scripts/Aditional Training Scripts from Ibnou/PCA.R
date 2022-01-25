
##################
##Load Libraries #
##################

library(tidyverse)
library(agridat)


#######################################
# Principal Component Analysis - PCA #
######################################

data(australia.soybean)
dat1 = australia.soybean %>%
  as_tibble()

#Let's consider one location - "Brookstead"
dat.Brookstead <- dat1 %>%
  filter(loc=="Brookstead")

str(dat.Brookstead)

#Keep the names of genotype & trial years
labels.gen <- dat.Brookstead$gen
labels.year <- as.factor(dat.Brookstead$year)

#Remove factor variables
dat.Brookstead <- dat.Brookstead %>%
  select(-c(env, loc, year, gen))

#PCA
dat.Brookstead.pca<-prcomp(dat.Brookstead, center=TRUE, scale=TRUE)
dat.Brookstead.pca

#Scree plot
plot(dat.Brookstead.pca, type = "l")

#Summary
summary(dat.Brookstead.pca)




# Bi-Plot
#library(devtools)
#Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
#install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(dat.Brookstead.pca) +
  labs(title = "Yield and other traits of soybeans in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::australia.soybean"
  ) +
  theme_classic()

#Add the names of the genotypes
ggbiplot(dat.Brookstead.pca, labels=labels.gen) +
  labs(title = "Yield and other traits of soybeans in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::australia.soybean"
  ) +
  theme_classic()

#Add the year
ggbiplot(dat.Brookstead.pca, labels=labels.gen, groups=labels.year)+
  labs(title = "Yield and other traits of soybeans in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::australia.soybean"
  ) +
  theme_classic()



