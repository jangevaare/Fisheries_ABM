###########################################################
# individual_events.R
# October 2014
# Justin Angevaare
###########################################################

###########################################################
# SCOPE:
# The functions defined in this file will be used to apply
# mortality, movement, and growth at the individual level 
# to eggs, larvae, and juvenille fish. These events will be
# tracked using an event-sourcing methodology through
# discrete time (week level) and space (5' x 5' grid)
###########################################################

###########################################################
# INITIALIZATION - LIBRARIES, PATHS, REQUIRED DATA, &
# ASSUMPTIONS
###########################################################
library(reshape)

setwd("/Users/justin/Dropbox/Projects/[in progress] Fisheries ABM/")

# Spawning map
spawning_map = as.matrix(read.csv("masks/spawning.csv", header=F))
colnames(spawning_map) = NULL
spawning_melt = melt(spawning_map)
spawning_melt=spawning_melt[(is.na(spawning_melt$value)==FALSE),]

# Habitat preferability map
habitat_map = as.matrix(read.csv("masks/habitat.csv", header=F))
colnames(habitat_map) = NULL
habitat_melt = melt(habitat_map)
rownames(habitat_melt)
habitat_melt=habitat_melt[(is.na(habitat_melt$value)==FALSE),]

# Natural mortality assumptions by habitat preferability
# 1, 2, and 3
nat_mortality_rates = c(0.1, 0.2, 0.4)

###########################################################
# SPAWNING (LIST OF BROOD SIZES, SPAWNING MAP)
###########################################################

spawning=function(eggs, spawning_melt, time, event_db){
	# Number of spawning `sites`
	sites=dim(spawning_melt)[1]
	
	# Assign sites to `eggs`
	site_assignment = apply((rmultinom(length(eggs), 1, rep(1/sites, sites)) == 1), 2, which)

	# Output data.frame, event_db == NULL if new simulation
	if(is.na(event_db)){
		data.frame('agent_id'=1:length(eggs),
				   'change_id'=1,
				   'time'=time, 
				   'stage'='egg',
				   'location_id'=rownames(spawning_melt)[site_assignment], 
				   'location_x'=spawning_melt$X2[site_assignment], 
				   'location_y'=spawning_melt$X1[site_assignment],
				   'birth_d'=time, 
				   'num_alive'=eggs, 
				   'num_natural_death'=0, 
				   'num_anthro_death'=0)}
	else{
		rbind(event_db,
		data.frame('agent_id'=(max(event_db$agent_id)+1):(max(event_db$agent_id)+length(eggs)),
				   'change_id'=max(event_db$change_id)+1,
				   'time'=time,
		 		   'stage'='egg',
		 		   'location_id'=rownames(spawning_melt)[site_assignment],
				   'location_x'=spawning_melt$X2[site_assignment], 
				   'location_y'=spawning_melt$X1[site_assignment],
		 		   'birth_d'=time, 
		 		   'num_alive'=eggs, 
		 		   'num_natural_death'=0, 
		 		   'num_anthro_death'=0))}}
	
###########################################################
# NATURAL MORTALITY (LOCATION, STAGE)
###########################################################

natmortality=function(event_db, habitat_melt, time, nat_mortality_rates)
	{
	# Load in latest portion of `event_db`
	sub_event_db = event_db[(event_db$time==max(event_db$time)) & 
	                        (event_db$change_id==max(event_db$change_id)),]
	
	# Determine habitat preferability
	preferability_fun=function(location_id){
		habitat_melt$value[rownames(habitat_melt)==location_id]}
	
	# Link habitat preferability to natural mortality rates
	mortality_rate=nat_mortality_rates[sapply(sub_event_db$location_id, preferability_fun)]
	
	# Probabilistically cause death within each brood
	binomial_death=function(x){
		rbinom(1, sub_event_db$num_alive[x], mortality_rate[x])}
	
	deaths=sapply(1:length(mortality_rate), FUN = binomial_death)
	
	# Prepare to update `event_db`
	sub_event_db$num_alive = sub_event_db$num_alive - deaths
	sub_event_db$num_natural_death = sub_event_db$num_natural_death + deaths
	sub_event_db$change_id = sub_event_db$change_id + 1
	sub_event_db$time = time
	
	rbind(event_db, sub_event_db)
	}

###########################################################
# OTHER ANTHROPOGENIC MORTALITY (LOCATION, STAGE)
###########################################################

###########################################################
# STAGE ADVANCEMENT (STAGE, TIME)
###########################################################

###########################################################
# MOVEMENT (LOCATION, STAGE)
###########################################################

###########################################################
# ADVANCE TIME (EVENT DATABASE)
###########################################################