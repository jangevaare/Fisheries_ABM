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
# EGG PRODUCTION
###########################################################

egg_production=function(Age_Matrix, prop_mature, egg_num, time){
	 c(rpois(rbinom(1, Age_Matrix[1, time], 0.5*prop_mature[1]), egg_num[1]),
	   rpois(rbinom(1, Age_Matrix[2, time], 0.5*prop_mature[2]), egg_num[2]), 
	   rpois(rbinom(1, Age_Matrix[3, time], 0.5*prop_mature[3]), egg_num[3]),
	   rpois(rbinom(1, Age_Matrix[4, time], 0.5*prop_mature[4]), egg_num[4]),
	   rpois(rbinom(1, Age_Matrix[5, time], 0.5*prop_mature[5]), egg_num[5]))}

###########################################################
# AGE ADULTS
###########################################################

adult_update=function(Age_Matrix, survivorship, event_db, time){
	Age_Matrix[, t+1] = c(event_db$num_alive[(event_db$time==time) & 
	                     (event_db$stage=='adult'),],
	                    rbinom(1, Age_Matrix[1,t], survivorship[1]),
						rbinom(1, Age_Matrix[2,t], survivorship[2]),
						rbinom(1, Age_Matrix[3,t], survivorship[3]),
						rbinom(1, Age_Matrix[4,t], survivorship[4]) +
						rbinom(1, Age_Matrix[5,t], survivorship[5]))
	Age_Matrix}

###########################################################
# FISHERIES MORTALITY
###########################################################

# To be completed...
