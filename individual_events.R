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
habitat_melt=habitat_melt[(is.na(habitat_melt$value)==FALSE),]

# Anthropogenic effect map
anthro_map = as.matrix(read.csv("masks/anthro.csv", header=F))
colnames(anthro_map) = NULL
anthro_melt = melt(anthro_map)
anthro_melt=anthro_melt[(is.na(anthro_melt$value)==FALSE),]

# Natural mortality assumptions by habitat preferability
# 1, 2, and 3
larval_nat_mortality_rates = c(0.2, 0.3, 0.5)
juvenile_nat_mortality_rates = c(0.1, 0.2, 0.4)

# Additional mortality in the presence and absence of 
# anthropogenic impact
larval_anthro_mortality_rate = c(0, 0.2)
juvenile_anthro_mortality_rate = c(0, 0.2)

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
				   'stage'=as.vector(rep('egg', length(eggs))),
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
		 		   'stage'=as.vector(rep('egg', length(eggs))),
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

nat_mortality=function(event_db, stage, habitat_melt, time, nat_mortality_rates)
	{
	# Load in latest portion of `event_db` specific to `stage`
	sub_event_db = event_db[(event_db$change_id==max(event_db$change_id[(event_db$time==time) & (event_db$stage==stage)])),]
	
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

anthro_mortality=function(event_db, anthro_melt, time, anthro_mortality_rates)
	{
	# Load in latest portion of `event_db` specific to `stage`
	sub_event_db = event_db[(event_db$change_id==max(event_db$change_id[(event_db$time==time) & (event_db$stage==stage)])),]
	                        	
	# Determine location 
	anthro_fun=function(location_id){
		anthro_melt$value[rownames(anthro_melt)==location_id]}
	
	# Link location to special mortality rates
	mortality_rate=anthro_mortality_rates[sapply(sub_event_db$location_id, preferability_fun)]
	
	# Probabilistically cause death within each brood
	binomial_death=function(x){
		rbinom(1, sub_event_db$num_alive[x], mortality_rate[x])}
	
	deaths=sapply(1:length(mortality_rate), FUN = binomial_death)
	
	# Prepare to update `event_db`
	sub_event_db$num_alive = sub_event_db$num_alive - deaths
	sub_event_db$num_anthro_death = sub_event_db$num_anthro_death + deaths
	sub_event_db$change_id = sub_event_db$change_id + 1
	sub_event_db$time = time
	
	rbind(event_db, sub_event_db)
	}

###########################################################
# STAGE ADVANCEMENT FUNCTIONS
###########################################################

egg_to_larvae=function(event_db, time)
	{
	# Load in latest portion of `event_db`
	sub_event_db = event_db[(event_db$time==time & 
	                        (event_db$change_id==max(event_db$change_id[(event_db$time == time) & (event_db$stage=='egg')])),]
	                        
	# Update `event_db`
	sub_event_db$stage=as.vector(sub_event_db$stage)
	sub_event_db$stage[sub_event_db$stage == 'egg']='larvae'
	sub_event_db$change_id = sub_event_db$change_id + 1
	sub_event_db$time = time
	
	rbind(event_db, sub_event_db)
	}

larvae_to_juvenile=function(event_db, time)
	{
	# Load in latest portion of `event_db`
	sub_event_db = event_db[(event_db$time==time & 
	                        (event_db$change_id==max(event_db$change_id[(event_db$time == time) & (event_db$stage=='larvae')])),]

	# Update `event_db`
	sub_event_db$stage=as.vector(sub_event_db$stage)
	sub_event_db$stage[sub_event_db$stage == 'larvae']='juvenile'
	sub_event_db$change_id = sub_event_db$change_id + 1
	sub_event_db$time = time
	
	rbind(event_db, sub_event_db)
	}

juvenile_to_adult=function(event_db, time)
	{
	# Load in latest portion of `event_db`
	sub_event_db = event_db[(event_db$time==time & 
	                        (event_db$change_id==max(event_db$change_id[(event_db$time == time) & (event_db$stage=='juvenile')])),]

	# Update `event_db`
	sub_event_db$stage=as.vector(sub_event_db$stage)
	sub_event_db$stage[sub_event_db$stage == 'juvenile']='adult'
	sub_event_db$change_id = sub_event_db$change_id + 1
	sub_event_db$time = time
	
	rbind(event_db, sub_event_db)
	}

###########################################################
# MOVEMENT (LOCATION, STAGE)
###########################################################

# To be completed...