###########################################################
# simulation.R
# October 2014
# Justin Angevaare
###########################################################

###########################################################
# SCOPE:
# This file illustrates the functions of 
# `individuals_events.R` and `stock_events.R` in the 
# simulation of a fish population. Assumptions of this 
# simulation are collected at the beggining of the file
# for easy modification
###########################################################

###########################################################
# ASSUMPTIONS & SIMULATION PARAMETERS
###########################################################

# Natural mortality assumptions by habitat preferability
# 1, 2, and 3. Weekly egg mortality related to (1939 Price) 
egg_nat_mortality_rates = c(0.02, 0.02, 0.02)
larval_nat_mortality_rates = c(0.02, 0.03, 0.05)
juvenile_nat_mortality_rates = c(0.01, 0.02, 0.04)
nat_mortality_rates = list(egg_nat_mortality_rates, larval_nat_mortality_rates, juvenile_nat_mortality_rates)

# Additional mortality in the presence and absence of 
# anthropogenic impact
egg_anthro_mortality_rate = c(0, 0.01)
larval_anthro_mortality_rate = c(0, 0.05)
juvenile_anthro_mortality_rate = c(0, 0.05)
anthro_mortality_rates = list(egg_anthro_mortality_rate, larval_anthro_mortality_rate, juvenile_anthro_mortality_rate)

# Proportion of fish sexually mature at each age class
# (ages 2, 3, 4, 5, 6+)
prop_mature = c(0.1, 0.5, 0.9, 1, 1)

# Eggs produced by a female of each age class, if sexually
# mature (ages 2, 3, 4, 5, 6+)
egg_num = c(7500, 15000, 20000, 22500, 25000)

# Annual survivorship (ages 2-3, 3-4, 4-5, 5-6+, 6+-6+), 
# natural mortality only
survivorship=c(0.35, 0.45, 0.4, 0.35, 0.2)

# Initial age distribution, and simulation length (50 years)
sim_length=50
Age_Matrix=matrix(nrow=5, ncol=sim_length, 0)
Age_Matrix[,1]=c(rpois(1, 30000), rpois(1, 20000), rpois(1, 15000), rpois(1, 10000), rpois(1, 8000))

# Eggs assumed to incubate for 19 (1939 Price) weeks before
# hatching into larvae. Remain as larvae until year completed.
incubation = 19

###########################################################
# REQUIRED DATA & LIBRARIES
###########################################################

setwd("/Users/justin/Dropbox/Projects/[in progress] Fisheries ABM/")

source("stock_events.R")
source("individual_events.R")

library(reshape)

# Spawning map
spawning_map = as.matrix(read.csv("masks/spawning.csv", header=F))
colnames(spawning_map) = NULL
spawning_melt = melt(spawning_map)
spawning_melt=spawning_melt[(is.na(spawning_melt$value)==FALSE),]

# Habitat preferability map
habitat_map = as.matrix(read.csv("masks/habitat.csv", header=F))
colnames(habitat_map) = NULL
habitat_melt = melt(habitat_map)
habitat_melt=habitat_melt[(is.na(habitat_melt$value)==FALSE),]

# Anthropogenic effect map
anthro_map = as.matrix(read.csv("masks/anthro.csv", header=F))
colnames(anthro_map) = NULL
anthro_melt = melt(anthro_map)
anthro_melt=anthro_melt[(is.na(anthro_melt$value)==FALSE),]

###########################################################
# BEGIN SIMULATION
###########################################################

# Set time = 1 (year) so event database can be established
t=1

# Produce eggs from adult population
eggs = egg_production(Age_Matrix, prop_mature, egg_num, t)	

# Generate initial event_db based on eggs
event_db=spawning(eggs, spawning_melt, t, NA)

# Weekly mortality and movement
for(w in 1:incubation){
	event_db=nat_mortality(event_db, habitat_melt, t, nat_mortality_rates)
	event_db=anthro_mortality(event_db, anthro_melt, t, anthro_mortality_rates)
	}

# Eggs hatch...
event_db=egg_to_larvae(event_db, t)

for(w in (incubation+1):52){
	event_db=nat_mortality(event_db, habitat_melt, t, nat_mortality_rates)
	event_db=anthro_mortality(event_db, anthro_melt, t, anthro_mortality_rates)
	}

# Harvest adults here

# Larvae become juveniles...
event_db=larvae_to_juvenile(event_db, t)

# Insert 'new' adults into age-structured model (none in for 
# the first year), and age surviving adults
Age_Matrix=adult_update(Age_Matrix, survivorship, event_db, t)

# Now loop for remainder of the simulation
for(t in 2:(sim_length-1)){
	eggs = egg_production(Age_Matrix, prop_mature, egg_num, t)	
	event_db=spawning(eggs, spawning_melt, t, event_db)	

	for(w in 1:incubation){
		event_db=nat_mortality(event_db, habitat_melt, t, nat_mortality_rates)
		event_db=anthro_mortality(event_db, anthro_melt, t, anthro_mortality_rates)
		}

	event_db=egg_to_larvae(event_db, t)

	for(w in (incubation+1):52){
		event_db=nat_mortality(event_db, habitat_melt, t, nat_mortality_rates)
		event_db=anthro_mortality(event_db, anthro_melt, t, anthro_mortality_rates)
		}

	# Harvest adults here

	event_db=juvenile_to_adult(event_db, t)
	event_db=larvae_to_juvenile(event_db, t)
	Age_Matrix=adult_update(Age_Matrix, survivorship, event_db, t)
	if(sum(Age_Matrix[,t+1])>1e13) break
	if(any(is.na(Age_Matrix[,t+1]))) break
	}
