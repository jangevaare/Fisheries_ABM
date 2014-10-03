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
setwd("/Users/justin/Dropbox/Projects/[in progress] Fisheries ABM/")

library(reshape)

spawning = read.csv("masks/spawning.csv", header=T)




###########################################################
# SPAWNING (LIST OF BROOD SIZEs, SPAWNING MAP)
###########################################################


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
