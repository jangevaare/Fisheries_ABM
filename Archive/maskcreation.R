setwd("/Users/justin/Dropbox/Projects/[in progress] Fisheries ABM/")
hgrid=read.csv("Blank.csv", header=F)
hgrid=cbind(-1, hgrid, -1)
hgrid=rbind(-1, hgrid, -1)

# Develop habitat preferability map
# Find borders, give a 1
for(i in 2:(dim(hgrid)[1]-1))
	{
	for(j in 2:(dim(hgrid)[2]-1))
		{
		if(hgrid[i,j] == 0 & (hgrid[i-1,j] == -1 | hgrid[i+1,j] == -1 | hgrid[i,j-1] == -1 | hgrid[i,j+1] == -1)){hgrid[i,j] = 1}	
		}	
	}
	
# Find borders of border, give a 2
for(i in 2:(dim(hgrid)[1]-1))
	{
	for(j in 2:(dim(hgrid)[2]-1))
		{
		if(hgrid[i,j] == 0 & (hgrid[i-1,j] == 1 | hgrid[i+1,j] == 1 | hgrid[i,j-1] == 1 | hgrid[i,j+1] == 1)){hgrid[i,j] = 2}	
		}	
	}
	
# Everything else, give it a 3.
hgrid[hgrid==0]=3

# Set -1's to NULL
hgrid[hgrid==-1]=NA

write.table(hgrid, "Habitat.csv", row.names=F, col.names=F, sep=",")

hgrid[hgrid>0]=0
write.table(hgrid, "Blank.csv", row.names=F, col.names=F, sep=",")

hgrid2=hgrid
hgrid2[25, 34]=1
hgrid2[hgrid2==0]=NA
write.table(hgrid2, "Anthro.csv", row.names=F, col.names=F, sep=",")

hgrid3=read.csv("Spawning.csv", header=F)
hgrid3[hgrid3==0]=NA
write.table(hgrid3, "Spawning.csv", row.names=F, col.names=F, sep=",")

