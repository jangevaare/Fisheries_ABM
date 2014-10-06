# Simple Matrix Model Test
# Justin Angevaare and Nik Krsic
# July 2014

# Proportion of fish sexually mature at each age class
prop_mature = c(0, 0.1, 0.5, 0.9, 1, 1)

# Eggs produced by a female of each age class, if sexually mature
egg_num = c(0, 7500, 15000, 20000, 22500, 25000)

# Track 6 age classes for 100 years
N=matrix(nrow=6, ncol=100, 0)

# Initial age distribution
N[,1]=c(100000000, 300000, 200000, 150000, 100000, 800000)

# Transition matrix is 6x6...
transition=matrix(nrow=6, ncol=6, 0)

# Assume 50% of individuals are female...
transition[1,]=prop_mature*egg_num*0.5

# Egg/Larvae survivorship calculated as 2.5 recruited fish/200000 average eggs, may expect it a bit higher since recruitment would usually refer to the age 2 class, not the age 1. 
transition[2,1] = 0.0002

transition[3,2] = 0.35
transition[4,3] = 0.5
transition[5,4] = 0.55
transition[6,5] = 0.5
transition[6,6] = 0.35

for(t in 2:100)
	{
	N[,t]=transition%*%N[,t-1]	
	}
	}


plot(y=colSums(N), x=1:100, type='l')


# Now lets try a more stochastic implementation

# Create a carrying capacity function for age 1 fish surviving until age 2 (when density dependent effects are most likely to occur)
# The maximum survivorship is equal to the max adult survivorship

logit_K=function(x)
	{
	# Adjust the constants below if you'd like...
	# The multiplier will effect the steep-ness of population correction
	# The constant will effect the optimal population for the environment	
	x2 = -0.00003*x + 3.75
	ans=exp(x2)/(exp(x2) +1)
	ans2=(ans * 0.45)
	ans2
	}
	
# Played with constants until plot looked about right...	
par(mfrow=c(1,2))
plot(x=seq(from=0, to=200000, by=100), y=logit_K(seq(from=0, to=200000, by=100)), type='l', ylab='Survivability into Age 2', xlab='Age 1 Population')
plot(x=seq(from=0, to=200000, by=100), y=seq(from=0, to=200000, by=100)*logit_K(seq(from=0, to=200000, by=100)), type='l', xlab='Age 1 Population', ylab='Population promoted to Age 2')


# Initial age distribution
N_stoch=matrix(nrow=6, ncol=500, 0)
N_stoch[,1]=c(rpois(1, 10000000), rpois(1, 30000), rpois(1, 20000), rpois(1, 15000), rpois(1, 10000), rpois(1, 8000))

for(t in 2:500)
	{
	neweggs=0
	# How many eggs produced by each age class? Add it to the total
	# The number of sexually mature females in each age class is a binomial R.V.
	# The number of eggs produced by each sexually mature female in each age class is a Poisson R.V.
	for(a in 2:6)
		{
		neweggs=neweggs+rbinom(1, N_stoch[a,t-1], 0.5*prop_mature[a])*rpois(1, egg_num[a])
		}
	N_stoch[1,t] = neweggs
	N_stoch[2,t] = rbinom(1, N_stoch[1,t-1], 0.0002)
	# Density dependence below...
	N_stoch[3,t] = rbinom(1, N_stoch[2,t-1], logit_K(N_stoch[2,t-1]))
	N_stoch[4,t] = rbinom(1, N_stoch[3,t-1], 0.45)
	N_stoch[5,t] = rbinom(1, N_stoch[4,t-1], 0.40)
	# Also account for the fact that this is a '5+' age category
	N_stoch[6,t] = rbinom(1, N_stoch[5,t-1], 0.35) + rbinom(1, N_stoch[6,t-1], 0.2)
	}

plot(y=colSums(N_stoch)[100:500], x=100:500, type='l')
plot(y=colSums(N_stoch)[400:500], x=400:500, type='l')