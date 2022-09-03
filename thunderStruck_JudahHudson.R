
# ***FUNCTIONS***

#function
shotDistribution<- function(team, teamfga){
	numNC3Shots = 0
	numC3Shots = 0
	num2PTShots = 0

	numTeamShots = teamfga 

	# get x & y coordinates for each shot
	teamShots = team[2:3]

	#iterate through team shot
	for (i in 1:numTeamShots) {
		currentShot = teamShots[i,1:2]
		x = currentShot$x
		y = currentShot$y
	
		#find shot zone
		if(y>7.8 & (x^2 + y^2 >= 23.75^2)) {
			#NC3
			numNC3Shots = numNC3Shots + 1
		} else if(y<=7.8 & abs(x)>=22) {
			#C3
			numC3Shots = numC3Shots + 1
		} else {
			#2PT
			num2PTShots = num2PTShots + 1
		}
	
	} #end for loop


	# ASSIGN SHOT DISTRIBUTION
	#NC3
	nc3 = numNC3Shots/teamfga
	#C3
	c3 = numC3Shots/teamfga
	#2PT
	twoPT = num2PTShots/teamfga

	res=list(NC3=nc3, C3=c3, all2PT=twoPT)

} #end function


#function get team from zone
#eg: getTeam("Team A", zone_c3)
getTeam<-function(teamName, zone) {
	indexTeam <- zone$team == teamName
	teamInZone = zone[indexTeam, 1:4]
	teamInZone
}


#function find FGA for a zone
find_FGA<-function(zone) {
	FGA = length(zone$fgmade)
}


#function find FGM
find_FGM<-function(zone, zone_fga) {
	FGM = 0
	for(i in 1:zone_fga) {
		if(zone$fgmade[i] == 1) {
			FGM = FGM + 1
		}
	}
	FGM
}
	

#function didScore
didScore<-function(zone, zone_fga) {
	score = 0
	#iterate through FieldGoalsAttempted
	for(i in 1:zone_fga) {
		#if scored
		if(zone$fgmade[i] == 1) {
			score = score + 1
		}
	}
	score
}


#bonus: this will work for all zones, including 2PT
#function find 3PM
find_3PM<-function(zone, zone_fga) {
	three3PM = 0
	#iterate through FieldGoalsAttempted
	for(i in 1:zone_fga) {
		#if scored
		if(zone$fgmade[i] == 1) {
			#determine if 3PT
			x = zone$x[i]
			y = zone$y[i]
			if(y>7.8 & (x^2 + y^2 >= 23.75^2)) {
				#NC3
				three3PM = three3PM + 1
			} else if(y<=7.8 & abs(x)>=22) {
				#C3
				three3PM = three3PM + 1
			} else {
				#2PT
			}
		}
	}
	three3PM
}


#function find eFG%
find_eFG<- function(fgm, three3pm, fga) {
	EFG = (fgm + (0.5 * three3pm)) / fga
	EFG
}




# ***CALLS***

#read in table
shots_data = read.csv("C:/Users/sanford/Documents/Coding/jobPrep/02_okc thunder_data/shots_data.csv", header = TRUE)


# PARSE SHOTS_DATA -> SEPARATE ZONES
#initialize zone variables
#NC3
zone_nc3<-as.data.frame(matrix(nrow=0,ncol=4))
colnames(zone_nc3)<-c("team","x","y","fgmade")
#C3
zone_c3<-as.data.frame(matrix(nrow=0,ncol=4))
colnames(zone_c3)<-c("team","x","y","fgmade")
#2PT
zone_2pt<-as.data.frame(matrix(nrow=0,ncol=4))
colnames(zone_2pt)<-c("team","x","y","fgmade")


#iterate through shots_data
for(i in 1:nrow(shots_data)) {
	#find each zone
	x = shots_data$x[i]
	y = shots_data$y[i]
	if(y>7.8 & (x^2 + y^2 >= 23.75^2)) {
		#NC3
		#save zone into variable
		zone_nc3<-rbind(zone_nc3, shots_data[i, 1:4])
	} else if(y<=7.8 & abs(x)>=22) {
		#C3
		zone_c3<-rbind(zone_c3, shots_data[i, 1:4])
	} else {
		zone_2pt<-rbind(zone_2pt, shots_data[i, 1:4])
	}
}


#variables for zones by teams
#NC3
 #teamA
  nc3_teamA = getTeam("Team A", zone_nc3)
 #teamB
  nc3_teamB = getTeam("Team B", zone_nc3)

#C3
 #teamA
  c3_teamA = getTeam("Team A", zone_c3)
 #teamB
  c3_teamB = getTeam("Team B", zone_c3)

#two2PT
 #teamA
  two2pt_teamA = getTeam("Team A", zone_2pt)
 #teamB
  two2pt_teamB = getTeam("Team B", zone_2pt)


#FGA
 #NC3
  #teamA
   nc3_teamA_fga = find_FGA(nc3_teamA)
  #teamB
   nc3_teamB_fga = find_FGA(nc3_teamB)

 #C3
  #teamA
   c3_teamA_fga = find_FGA(c3_teamA)
  #teamB
   c3_teamB_fga = find_FGA(c3_teamB)

 #2PT
  #teamA
   two2pt_teamA_fga = find_FGA(two2pt_teamA)
  #teamB
   two2pt_teamB_fga = find_FGA(two2pt_teamB)


#FGM
 #NC3
  #teamA
   nc3_teamA_fgm = find_FGM(nc3_teamA, nc3_teamA_fga)
  #teamB
   nc3_teamB_fgm = find_FGM(nc3_teamB, nc3_teamB_fga)

 #C3
  #teamA
   c3_teamA_fgm = find_FGM(c3_teamA, c3_teamA_fga)
  #teamB
   c3_teamB_fgm = find_FGM(c3_teamB, c3_teamB_fga)

 #2PT
  #teamA
   two2pt_teamA_fgm = find_FGM(two2pt_teamA, two2pt_teamA_fga)
  #teamB
   two2pt_teamB_fgm = find_FGM(two2pt_teamB, two2pt_teamB_fga)


#3PM
 #NC3
  #teamA
   nc3_teamA_3pm = didScore(nc3_teamA, nc3_teamA_fga) 
  #teamB
   nc3_teamB_3pm = didScore(nc3_teamB, nc3_teamB_fga)

 #C3
  #teamA
   c3_teamA_3pm = didScore(c3_teamA, c3_teamA_fga)
  #teamB
   c3_teamB_3pm = didScore(c3_teamB, c3_teamB_fga)

 #2PT
  #teamA
   two2pt_teamA_3pm = 0
  #teamB
   two2pt_teamB_3pm = 0


#saving results into variables
#NC3
 #teamA
  #shot distribution
	nc3_teamA_shotDistribution = teamA_shotDistResults$NC3
  #eFG#
	nc3_teamA_eFG = find_eFG(nc3_teamA_fgm, nc3_teamA_3pm, nc3_teamA_fga)
 #teamB
  #shot distribution
	nc3_teamB_shotDistribution = teamB_shotDistResults$NC3
  #eFG#
	nc3_teamB_eFG = find_eFG(nc3_teamB_fgm, nc3_teamB_3pm, nc3_teamB_fga)

#C3
 #teamA
  #shot distribution
	c3_teamA_shotDistribution = teamA_shotDistResults$C3
  #eFG#
	c3_teamA_eFG = find_eFG(c3_teamA_fgm, c3_teamA_3pm, c3_teamA_fga)
 #teamB
  #shot distribution
	c3_teamB_shotDistribution = teamB_shotDistResults$C3
  #eFG#
	c3_teamB_eFG = find_eFG(c3_teamB_fgm, c3_teamB_3pm, c3_teamB_fga)

#2PT
 #teamA
  #shot distribution
	two2pt_teamA_shotDistribution = teamA_shotDistResults$all2PT
  #eFG#
	two2pt_teamA_eFG = find_eFG(two2pt_teamA_fgm, two2pt_teamA_3pm, two2pt_teamA_fga)
 #teamB
  #shot distribution
	two2pt_teamB_shotDistribution = teamB_shotDistResults$all2PT
  #eFG#
	two2pt_teamB_eFG = find_eFG(two2pt_teamB_fgm, two2pt_teamB_3pm, two2pt_teamB_fga)




###DELIVERABLES###

nc3_teamA_shotDistribution
nc3_teamA_eFG

nc3_teamB_shotDistribution
nc3_teamB_eFG


c3_teamA_shotDistribution
c3_teamA_eFG

c3_teamB_shotDistribution
c3_teamB_eFG


two2pt_teamA_shotDistribution
two2pt_teamA_eFG

two2pt_teamB_shotDistribution
two2pt_teamB_eFG




