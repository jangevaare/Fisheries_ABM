###########################################################
# stock_events.R
# October 2014
# Justin Angevaare
###########################################################

###########################################################
# SCOPE:
# The functions defined in this file will be used to apply
# mortality at the stock level and to calculate 
# reproduction. Modeling of the adult life stages will 
# follow a simple age-structured model.
###########################################################

###########################################################
# INITIALIZATION - LIBRARIES, PATHS, REQUIRED DATA, &
# ASSUMPTIONS
###########################################################

setwd("/Users/justin/Dropbox/Projects/[in progress] Fisheries ABM/")

# Proportion of fish sexually mature at each age class
prop_mature = c(0.1, 0.5, 0.9, 1, 1)

# Eggs produced by a female of each age class, if sexually
# mature (ages 2, 3, 4, 5, 6+)
egg_num = c(7500, 15000, 20000, 22500, 25000)

# Annual survivorship (ages 2-3, 3-4, 4-5, 5-6+, 6+-6+), 
# natural mortality only
survivorship=c(0.35, 0.45, 0.4, 0.35, 0.2)

# Initial age distribution, and simulation length (50 years)
Age_Matrix=matrix(nrow=5, ncol=50, 0)
N[,1]=c(rpois(1, 30000), rpois(1, 20000), rpois(1, 15000), rpois(1, 10000), rpois(1, 8000))

###########################################################
# AGE
###########################################################
Adult_Update=function(Age_Matrix, survivorship, t){
	Age_Matrix[, t] = c(0, # Set as zero until entered from ABM	
						rbinom(1, Age_Matrix[1,t-1], survivorship[1]),
						rbinom(1, Age_Matrix[2,t-1], survivorship[2]),
						rbinom(1, Age_Matrix[3,t-1], survivorship[3]),
						rbinom(1, Age_Matrix[4,t-1], survivorship[4]) +
						rbinom(1, Age_Matrix[5,t-1], survivorship[5]))}

###########################################################
# EGG PRODUCTION
###########################################################

###########################################################
# FISHERIES MORTALITY
###########################################################

