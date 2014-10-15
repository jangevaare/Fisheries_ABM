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
# SPAWNING (LIST OF BROOD SIZES, SPAWNING MAP)
###########################################################

spawning=function(eggs, spawning_melt, time, event_db){
	# Number of spawning `sites`
	sites=dim(spawning_melt)[1]
	
	# Assign sites to `eggs`
	site_assignment = apply((rmultinom(length(eggs), 1, rep(1/sites, sites)) == 1), 2, which)

	if(is.data.frame(event_db)){
		rbind(event_db,
		data.frame('agent_id'=(max(event_db$agent_id)+1):(max(event_db$agent_id)+length(eggs)),
				   'change_id'=max(event_db$change_id),
				   'time'=time,
		 		   'stage'=as.vector(rep('egg', length(eggs))),
		 		   'location_id'=rownames(spawning_melt)[site_assignment],
				   'location_x'=spawning_melt$X2[site_assignment], 
				   'location_y'=spawning_melt$X1[site_assignment],
		 		   'birth_d'=time, 
		 		   'num_alive'=eggs, 
		 		   'num_natural_death'=0, 
		 		   'num_anthro_death'=0))}
	else{
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
				   'num_anthro_death'=0)}}
	
###########################################################
# NATURAL MORTALITY
###########################################################

nat_mortality = function(event_db, habitat_melt, time, nat_mortality_rates){
	# Load in latest portion of `event_db`, the last changed
	# at the specified time
	sub_event_db = event_db[event_db$time==time & event_db$change_id==max(event_db[event_db$time==time,]$change_id) & event_db$stage !='adult',]
	
	rate_link_fun = function(mortality_rates, melt, event_db){
		which_stage_fun = function(stage){
			which(c('egg', 'larvae', 'juvenile') == stage)
			}
		
		which_rate_fun = function(location_id){
			melt$value[rownames(melt)==location_id]
			}
		
		rate_num = sapply(event_db$location_id, which_rate_fun)
		stage_num = sapply(event_db$stage, which_stage_fun)
				
		rate_link_helper_fun = function(i){			
			mortality_rates[[stage_num[i]]][rate_num[i]]
			}
			
		sapply(1: length(rate_num), rate_link_helper_fun)
		}
		
	mortality_rate = rate_link_fun(nat_mortality_rates, habitat_melt, sub_event_db)

	# Define a function to probabilistically cause death 
	# within each brood (agent)
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

anthro_mortality=function(event_db, habitat_melt, time, anthro_mortality_rates){
	# Load in latest portion of `event_db`, the last changed
	# at the specified time
	sub_event_db = event_db[event_db$time==time & event_db$change_id==max(event_db[event_db$time==time,]$change_id) & event_db$stage !='adult',]
	
	# Link agents to mortality rates specific to location
	# and stage
	
	rate_link_fun = function(mortality_rates, melt, event_db){
		which_stage_fun = function(stage){
			which(c('egg', 'larvae', 'juvenile') == stage)
			}
		
		which_rate_fun = function(location_id){
			melt$value[rownames(melt)==location_id]
			}
		
		rate_num = sapply(event_db$location_id, which_rate_fun)
		stage_num = sapply(event_db$stage, which_stage_fun)
				
		rate_link_helper_fun = function(i){			
			mortality_rates[[stage_num[i]]][rate_num[i]]
			}
			
		sapply(1: length(rate_num), rate_link_helper_fun)
		}
		
	mortality_rate = rate_link_fun(anthro_mortality_rates, anthro_melt, sub_event_db)

	# Define a function to probabilistically cause death 
	# within each brood (agent)
	binomial_death=function(x){
		rbinom(1, sub_event_db$num_alive[x], mortality_rate[x])}
	
	deaths=sapply(1:length(mortality_rate), FUN = binomial_death)
	
	# Prepare to update `event_db`
	sub_event_db$num_alive = sub_event_db$num_alive - deaths
	sub_event_db$num_natural_death = sub_event_db$num_anthro_death + deaths
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
	                        (event_db$change_id==max(event_db$change_id[event_db$time == time])) & event_db$stage !='adult'),]
	                        
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
	                        (event_db$change_id==max(event_db$change_id[event_db$time == time])) & event_db$stage !='adult'),]

	# Update `event_db`
	sub_event_db$stage=as.vector(sub_event_db$stage)
	sub_event_db$stage[sub_event_db$stage == 'larvae']='juvenile'
	sub_event_db$change_id = sub_event_db$change_id + 1
	sub_event_db$time = time + 1 # Since this occurs at the end of each year...
	
	rbind(event_db, sub_event_db)
	}

juvenile_to_adult=function(event_db, time)
	{
	# Load in latest portion of `event_db`
	sub_event_db = event_db[(event_db$time==time & 
	                        (event_db$change_id==max(event_db$change_id[event_db$time == time])) & event_db$stage !='adult'),]

	# Update `event_db`
	sub_event_db$stage=as.vector(sub_event_db$stage)
	sub_event_db$stage[sub_event_db$stage == 'juvenile']='adult'
	sub_event_db$change_id = sub_event_db$change_id + 1
	sub_event_db$time[sub_event_db$stage == 'adult'] = time
	
	rbind(event_db, sub_event_db)
	}

###########################################################
# MOVEMENT (LOCATION, STAGE)
###########################################################

# To be completed...