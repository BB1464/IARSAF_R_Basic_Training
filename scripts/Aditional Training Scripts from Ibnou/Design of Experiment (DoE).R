#Load libraries
library(tidyverse)
library(agricolae)

####################################
#Completely Randomized Design - CRD#
####################################

#Planning CRD
trt <- c("G1","G2","G3","G4","G5")
plan <- design.crd(trt,3, seed=7638)
plan <- plan$book
View(plan)

# write out the plan
write_csv(plan, file = "CRDplan.csv")


#########################################
#Randomized Complete Block Design - RCBD#
#########################################

#Planning RCBD
trt1 <- c("A","B","C","D","E")
plan2 <- design.rcbd(trt1, 4, seed = 161)
plan2 <- plan2$book
View(plan2)


# write out the plan
write_csv(plan2, file = "RCBDplan.csv")

###############################
#Incomplete Block Design - IBD#
###############################

#Planning Row Column Design -  Square Lattice Design SLD
trt2 <- 1:81
plan3 <- design.lattice(trt2, r=2, seed = 123)
plan3 <- plan3$book

View(plan3)


# write out the plan
write_csv(plan3, file = "SLDplan.csv")



#Planning an Alpha design - AD
#30 test materials
trt <- 1:30
#trt <- letters[1:12] 
t <- length(trt)
# size block k
k <- 3
# Blocks s
s <- t/k
# replications r
r <- 2 

plan4 <- design.alpha(trt,k,r,serie=2, seed=1234)

plan4 <- plan4$book

View(plan4)

# write out the plan
write_csv(plan4, file = "ADplan.csv")


###############################
#Split Plot Design - SPD #
###############################
trt1 <- c("A", "B", "C", "D", "E")
trt2 <- c("N0", "N50", "N100")
planSP <- design.split(trt1, trt2, r=3, serie=2, seed=14)

planSP <- planSP$book

View(planSP)

# write out the plan
write_csv(planSP, file = "SPplan.csv")




###############################
#Augmented Block Design - ABD #
###############################

#Planning ABD
check <- c("A","B","C","D")  #Checks
new <- letters[20:26]   #New treatments
# 5 Replication or blocks
plan5 <-design.dau(check, new, r=5, serie=2, seed = 1611)
plan5 <- plan5$book
View(plan5)

# write out the plan
write_csv(plan5, file = "ABDplan.csv")

