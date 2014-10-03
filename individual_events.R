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
# INITIALIZATION - LIBRARIES, PATHS, REQUIRED DATA
###########################################################
library(reshape)

setwd("/Users/justin/Dropbox/Projects/[in progress] Fisheries ABM/")

# Spawning map
spawning_map = as.matrix(read.csv("masks/spawning.csv", header=F))
colnames(spawning_map) = NULL
spawning_melt = melt(spawning_map)
spawning_melt=spawning_melt[(is.na(spawning_melt$value)==FALSE),]

###########################################################
# SPAWNING (LIST OF BROOD SIZES, SPAWNING MAP)
###########################################################

spawning=function(eggs, spawning_melt, time, event_db){
	# Number of spawning `sites`
	sites=dim(spawning_melt)[1]
	
	# Assign sites to `eggs`
	site_assignment = apply((rmultinom(length(eggs), 1, rep(1/sites, sites)) == 1), 2, which)

	# Output data.frame, event_db == NULL if new simulation
	if(event_db == NULL){
		data.frame('agent_id'=1:length(eggs), 
				   'stage'='egg', 
				   'location'=site_assignment, 
				   'birth_d'=time, 
				   'num_alive'=eggs, 
				   'num_natural_death'=0, 
				   'num_anthro_death'=0)}
	else{
		rbind(event_db,
		data.frame('agent_id'=(max(event_db$agent_id)+1):(max(event_db$agent_id)+length(eggs)),
		 		   'stage'='egg', 
		 		   'location'=site_assignment, 
		 		   'birth_d'=time, 
		 		   'num_alive'=eggs, 
		 		   'num_natural_death'=0, 
		 		   'num_anthro_death'=0))}}
	

###########################################################
# NATURAL MORTALITY (LOCATION, STAGE)
###########################################################

###########################################################
# OTHER ANTHROPOGENIC MORTALITY (LOCATION, STAGE)
###########################################################

###########################################################
# STAGE ADVANCEMENT (STAGE, TIME)
###########################################################

###########################################################
# MOVEMENT (LOCATION, STAGE)
###########################################################
