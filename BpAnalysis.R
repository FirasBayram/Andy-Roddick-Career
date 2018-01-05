#save break points in a dataframe
bpts <- read.csv("break points data.csv",stringsAsFactors = FALSE)
#sort matches by match ID
bpts <- bpts[order(as.numeric(bpts$MatchID)),]
row.names(bpts)<- 1:nrow(bpts)
#distinguish between unreturned serve and return winner
bpts$shot[bpts$state=="b" & bpts$shot==2]<- -2
head(bpts)
#add match ID column to be
rod_all$MatchID <- row.names(rod_all)
#merge the two dataframes by Match ID
bpts <- merge(bpts,rod_all,by = "MatchID")

rod_all$MatchID<- NULL
#saving break points in a csv file
write.csv(bpts,"extended break points.csv",row.names = FALSE)
#calculate number of matches used
length(unique(bpts$MatchID))
#number of break points faced 
sum(rod_all$rod_bpfaced)
#save break points saved in a separated dataframe
bptsaved <- bpts[bpts$state=="s",]
#save break points broken in a separated dataframe
bptbroken <- bpts[bpts$state=="b",]
sum(bptsaved$shot>4)
sum(bptbroken$shot>4)
length(unique(bpts$tourney_date))
agg <- aggregate (bpts$MatchID, by = list( set = bpts$set, shot = bpts$shot, serve = bpts$serve), FUN = length)
head(agg)
agg2 <- aggregate(bpts$MatchID, by = list(set = bpts$set, state = bpts$state),length)
rod_all_won[rod_all_won$loser_rank==1,]
library(dplyr)
grouped_bpts <- select(bpts,coach,state,serve) %>% group_by(coach,state,serve)
grouped_bpts
summarizedbpts <- summarise(grouped_bpts,count=n())
summarizedbpts
