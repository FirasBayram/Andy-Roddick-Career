# Saving All Andy Roddick matches in a data frame
full_matches <- read.csv("Andy Roddick all results - Walkovers excluded.csv",stringsAsFactors = FALSE)
full_matches <- full_matches[order(as.numeric(full_matches$tourney_date)),]
row.names(full_matches)<- 1:825

#Adding Andy's coach column
full_matches$coach <- NA
full_matches$coach[1:181] <- "Tarik Benhabiles"
full_matches$coach[182:328] <- "Brad Gilbert"
full_matches$coach[329:405] <- "Dean Goldfine"
full_matches$coach[445:552] <- "Jimmy Connors"
full_matches$coach[608:825] <- "Larry Stefanki"
full_matches$coach[is.na(full_matches$coach)] <- "John Roddick"
# total number of matches 
full_mtchs_ttl <- nrow(full_matches)
#saving all Andy Roddick matches won in a logical vector and data frame
mtchs_won <- full_matches$winner_name=="Andy Roddick"
mtchs_won_df <- full_matches[mtchs_won,]
mtchs_won_ttl <- sum(mtchs_won)
#saving all Andy Roddick matches lost in a logical vector and data frame
mtchs_lost <- full_matches$loser_name=="Andy Roddick"
mtchs_lost_df <- full_matches[mtchs_lost,]
mtchs_lost_ttl <- sum(mtchs_lost)

## Andy Roddick  Matches which include stats
rod_all <- full_matches[full_matches$w_svpt!=0,]

## Deriving new columns

##Adding first serve percentage column to the table
rod_all["rod_first_sv_pctg"] <- 0
##Adding first serve won percentage column to the table
rod_all["rod_first_sv_won_pctg"] <-0
##Adding service games won percentage column to the table
rod_all["rod_sv_gms_won_pctg"]<-0

for ( j in 1 : nrow(rod_all) ) 
{
  if ( rod_all [j,11] == "Andy Roddick" ) 
  {
    rod_all [j,51] <- round ( rod_all [j,35] / rod_all [j,34], digits = 2 ) 
    rod_all [j,52] <- round ( rod_all [j,36] / rod_all [j,35], digits = 2 )
    rod_all [j,53] <- round ( 1- ( ( rod_all[j,40] - rod_all[j,39] )/ rod_all[j,38]), digits = 2 )
  }
  else { 
    rod_all [j,51] <- round ( rod_all [j,44] / rod_all [j,43], digits = 2 )
    rod_all[j,52] <- round (rod_all [j,45] / rod_all [j,44], digits = 2 )
    rod_all[j,53] <- round ( 1- ( ( rod_all[j,49] - rod_all[j,48]) / rod_all [j,47] ), digits = 2 ) 
  }
}



#saving Andy's aces number in a new column
rod_all$rod_aces <- 0 
for ( i in 1:803) {if(rod_all[i,11]=="Andy Roddick"){rod_all[i,54] <- rod_all[i,32]}
  else rod_all[i,54] <- rod_all[i,41]}

#saving Andy's double faults number in a new column
rod_all$rod_dfs <- 0 
for ( i in 1:803) {if(rod_all[i,11]=="Andy Roddick"){rod_all[i,55] <- rod_all[i,33]}
  else rod_all[i,55] <- rod_all[i,42]}
#saving Andy's break points faced number in a new column
rod_all$rod_bpfaced <- 0 
for ( i in 1:803) {if(rod_all[i,11]=="Andy Roddick"){rod_all[i,56] <- rod_all[i,40]}
  else rod_all[i,56] <- rod_all[i,49]}

row.names(rod_all) <- 1:803
rod_all_won <- rod_all[rod_all$winner_name=="Andy Roddick",]
rod_all_lost<- rod_all[rod_all$winner_name!="Andy Roddick",]



#saving the dataframes in csv files
write.csv(rod_all,"Andy Roddick all matches stats.csv",row.names = FALSE)
write.csv(rod_all_lost,"Andy Roddick all lost matches stats.csv", row.names = FALSE)
write.csv(rod_all_won,"Andy Roddick all won matches stats.csv", row.names = FALSE)


#saving all Andy Roddick finals in a logical vector and data frame
finals <- rod_all$round=="F" & !grepl("^Davis Cup." ,rod_all$tourney_name)
finals_ttl <- sum(finals)
finals_df <- rod_all[finals,]
write.csv(finals_df,"finals.csv")
#saving all Andy Roddick won finals in a logical vector and data frame
finals_won <- finals_df$winner_name=="Andy Roddick"
finals_won_ttl <- sum(finals_won)
finals_won_df <- finals_df[finals_won,]
write.csv(finals_won_df,"finals won.csv")
#saving all Andy Roddick lost finals in a logical vector and data frame
finals_lost <- finals_df$winner_name!="Andy Roddick"
finals_lost_ttl <- sum(finals_lost)
finals_lost_df <- finals_df[finals_lost,]
write.csv(finals_lost_df,"finals lost.csv")
##Aces calculations
#total number of aces 
aces_mtchs_won <- sum(mtchs_won_df$w_ace)
aces_mtchs_lost <- sum(mtchs_lost_df$l_ace) 
aces <- sum(rod_all$rod_aces)

#Andy Roddick matches with no aces 
## 0 matches with no ace when Andy won the match
sum(rod_all$rod_aces==0)
#save matches with no aces in a logical vector and data frame 
no_aces <- sum(rod_all$rod_aces==0)
no_aces_df <- rod_all[rod_all$rod_aces==0,]
write.csv(no_aces_df,"No aces matches.csv")


##Double faults calculations
#total number of double faults
dfs_mtchs_won <- sum(mtchs_won_df$w_df)
dfs_mtchs_lost <- sum(mtchs_lost_df$l_df) 
dfs <- dfs_mtchs_lost+ dfs_mtchs_won


# Saving matches for each surface in data frames
grass_mtchs <- read.csv("grass.csv", stringsAsFactors = FALSE)
grass_mtchs_won<- grass_mtchs[grass_mtchs$winner_name=="Andy Roddick",]
grass_mtchs_lost<- grass_mtchs[grass_mtchs$winner_name!="Andy Roddick",]
hard_mtchs <- read.csv("hard.csv", stringsAsFactors = FALSE)
hard_mtchs_won<- hard_mtchs[hard_mtchs$winner_name=="Andy Roddick",]
hard_mtchs_lost<- hard_mtchs[hard_mtchs$winner_name!="Andy Roddick",]
carpet_mtchs <- read.csv("carpet.csv", stringsAsFactors = FALSE)
carpet_mtchs_won<- carpet_mtchs[carpet_mtchs$winner_name=="Andy Roddick",]
carpet_mtchs_lost<- carpet_mtchs[carpet_mtchs$winner_name!="Andy Roddick",]
clay_mtchs <- read.csv("clay.csv", stringsAsFactors = FALSE)
clay_mtchs_won<- clay_mtchs[clay_mtchs$winner_name=="Andy Roddick",]
clay_mtchs_lost<- clay_mtchs[clay_mtchs$winner_name!="Andy Roddick",]

## creating data frame according to Andy Roddick records on each surface
by_surface <- data.frame(surface="", wins=0, losses=0, aces=0,dfs=0,bp_faced=0,first_sv_pctg=0,first_sv_won_pctg=0,sv_gms_won_pctg=0,row.names = NULL,stringsAsFactors = FALSE)
by_surface[1,]<- c("hard",nrow(hard_mtchs_won),nrow(hard_mtchs_lost),
                   sum(hard_mtchs_won$w_ace)+sum(hard_mtchs_lost$l_ace),
                   sum(hard_mtchs_won$w_df)+sum(hard_mtchs_lost$l_df),
                   sum(hard_mtchs_won$w_bpFaced)+sum(hard_mtchs_lost$l_bpFaced),
                   round((sum(hard_mtchs_won$w_1stIn)+sum(hard_mtchs_lost$l_1stIn))/(sum(hard_mtchs_won$w_svpt)+sum(hard_mtchs_lost$l_svpt)),digits = 2),
                   round((sum(hard_mtchs_won$w_1stWon)+sum(hard_mtchs_lost$l_1stWon))/(sum(hard_mtchs_won$w_1stIn)+sum(hard_mtchs_lost$l_1stIn)),digits = 2),
                   round(1-(((sum(hard_mtchs_won$w_bpFaced)+sum(hard_mtchs_lost$l_bpFaced))-
                              (sum(hard_mtchs_won$w_bpSaved)+sum(hard_mtchs_lost$l_bpSaved)))
                            /(sum(hard_mtchs_won$w_SvGms)+sum(hard_mtchs_lost$l_SvGms))),digits = 2)
)
by_surface[2,]<- c("grass",nrow(grass_mtchs_won),nrow(grass_mtchs_lost),
           sum(grass_mtchs_won$w_ace)+sum(grass_mtchs_lost$l_ace),
           sum(grass_mtchs_won$w_df)+sum(grass_mtchs_lost$l_df),
           sum(grass_mtchs_won$w_bpFaced)+sum(grass_mtchs_lost$l_bpFaced),
           round((sum(grass_mtchs_won$w_1stIn)+sum(grass_mtchs_lost$l_1stIn))/(sum(grass_mtchs_won$w_svpt)+sum(grass_mtchs_lost$l_svpt)),digits = 2),
           round((sum(grass_mtchs_won$w_1stWon)+sum(grass_mtchs_lost$l_1stWon))/(sum(grass_mtchs_won$w_1stIn)+sum(grass_mtchs_lost$l_1stIn)),digits = 2),
           round(1-(((sum(grass_mtchs_won$w_bpFaced)+sum(grass_mtchs_lost$l_bpFaced))-(sum(grass_mtchs_won$w_bpSaved)+sum(grass_mtchs_lost$l_bpSaved)))/(sum(grass_mtchs_won$w_SvGms)+sum(grass_mtchs_lost$l_SvGms))),digits = 2)
           )

by_surface[3,]<- c("clay",nrow(clay_mtchs_won),nrow(clay_mtchs_lost),
                    sum(clay_mtchs_won$w_ace)+sum(clay_mtchs_lost$l_ace),
                    sum(clay_mtchs_won$w_df)+sum(clay_mtchs_lost$l_df),
                    sum(clay_mtchs_won$w_bpFaced)+sum(clay_mtchs_lost$l_bpFaced),
                    round((sum(clay_mtchs_won$w_1stIn)+sum(clay_mtchs_lost$l_1stIn))/(sum(clay_mtchs_won$w_svpt)+sum(clay_mtchs_lost$l_svpt)),digits = 2),
                    round((sum(clay_mtchs_won$w_1stWon)+sum(clay_mtchs_lost$l_1stWon))/(sum(clay_mtchs_won$w_1stIn)+sum(clay_mtchs_lost$l_1stIn)),digits = 2),
                   round(1-(((sum(clay_mtchs_won$w_bpFaced)+sum(clay_mtchs_lost$l_bpFaced))-(sum(clay_mtchs_won$w_bpSaved)+sum(clay_mtchs_lost$l_bpSaved)))/(sum(clay_mtchs_won$w_SvGms)+sum(clay_mtchs_lost$l_SvGms))),digits = 2)
)
by_surface[4,]<- c("carpet",nrow(carpet_mtchs_won),nrow(carpet_mtchs_lost),
                   sum(carpet_mtchs_won$w_ace)+sum(carpet_mtchs_lost$l_ace),
                   sum(carpet_mtchs_won$w_df)+sum(carpet_mtchs_lost$l_df),
                   sum(carpet_mtchs_won$w_bpFaced)+sum(carpet_mtchs_lost$l_bpFaced),
                   round((sum(carpet_mtchs_won$w_1stIn)+sum(carpet_mtchs_lost$l_1stIn))/(sum(carpet_mtchs_won$w_svpt)+sum(carpet_mtchs_lost$l_svpt)),digits = 2),
                   round((sum(carpet_mtchs_won$w_1stWon)+sum(carpet_mtchs_lost$l_1stWon))/(sum(carpet_mtchs_won$w_1stIn)+sum(carpet_mtchs_lost$l_1stIn)),digits = 2),
                   round(1-(((sum(carpet_mtchs_won$w_bpFaced)+sum(carpet_mtchs_lost$l_bpFaced))-(sum(carpet_mtchs_won$w_bpSaved)+sum(carpet_mtchs_lost$l_bpSaved)))/(sum(carpet_mtchs_won$w_SvGms)+sum(carpet_mtchs_lost$l_SvGms))),digits = 2)
)

## convert columns 2 to 8 to numeric values
for (i in 2:ncol(by_surface)){
by_surface[,i] <- as.numeric(by_surface[,i])
}



# Saving matches for each surface which include stats in data frames
grs_sts_count <- nrow(grass_mtchs[grass_mtchs$w_svpt!=0,])
grass_mtchs_won_stats <- grass_mtchs_won[grass_mtchs_won$w_svpt!=0,]
grass_mtchs_lost_stats <- grass_mtchs_lost[grass_mtchs_lost$w_svpt!=0,]

hrd_sts_count <- nrow(hard_mtchs[hard_mtchs$w_svpt!=0,])
hard_mtchs_won_stats <- hard_mtchs_won[hard_mtchs_won$w_svpt!=0,]
hard_mtchs_lost_stats <- hard_mtchs_lost[hard_mtchs_lost$w_svpt!=0,]

crpt_sts_count <- nrow(carpet_mtchs[carpet_mtchs$w_svpt!=0,])
carpet_mtchs_won_stats <- carpet_mtchs_won[carpet_mtchs_won$w_svpt!=0,]
carpet_mtchs_lost_stats <- carpet_mtchs_lost[carpet_mtchs_lost$w_svpt!=0,]

cly_sts_count <- nrow(clay_mtchs[clay_mtchs$w_svpt!=0,])
clay_mtchs_won_stats <- clay_mtchs_won[clay_mtchs_won$w_svpt!=0,]
clay_mtchs_lost_stats <- clay_mtchs_lost[clay_mtchs_lost$w_svpt!=0,]

## creating data frame according to Andy Roddick records on each surface which include stats
by_surface_stats <- data.frame(surface="", wins=0, losses=0, aces=0,dfs=0,bp_faced=0,first_sv_pctg=0,first_sv_won_pctg=0,sv_gms_won_pctg=0,row.names = NULL,stringsAsFactors = FALSE)
by_surface_stats[1,]<- c("hard",nrow(hard_mtchs_won_stats),nrow(hard_mtchs_lost_stats),
                   sum(hard_mtchs_won_stats$w_ace)+sum(hard_mtchs_lost_stats$l_ace),
                   sum(hard_mtchs_won_stats$w_df)+sum(hard_mtchs_lost_stats$l_df),
                   sum(hard_mtchs_won_stats$w_bpFaced)+sum(hard_mtchs_lost_stats$l_bpFaced),
                   round((sum(hard_mtchs_won_stats$w_1stIn)+sum(hard_mtchs_lost_stats$l_1stIn))/(sum(hard_mtchs_won_stats$w_svpt)+sum(hard_mtchs_lost_stats$l_svpt)),digits = 2),
                   round((sum(hard_mtchs_won_stats$w_1stWon)+sum(hard_mtchs_lost_stats$l_1stWon))/(sum(hard_mtchs_won_stats$w_1stIn)+sum(hard_mtchs_lost_stats$l_1stIn)),digits = 2),
                   round(1-(((sum(hard_mtchs_won_stats$w_bpFaced)+sum(hard_mtchs_lost_stats$l_bpFaced))-
                               (sum(hard_mtchs_won_stats$w_bpSaved)+sum(hard_mtchs_lost_stats$l_bpSaved)))
                            /(sum(hard_mtchs_won_stats$w_SvGms)+sum(hard_mtchs_lost_stats$l_SvGms))),digits = 2)
)
by_surface_stats[2,]<- c("grass",nrow(grass_mtchs_won_stats),nrow(grass_mtchs_lost_stats),
                   sum(grass_mtchs_won_stats$w_ace)+sum(grass_mtchs_lost_stats$l_ace),
                   sum(grass_mtchs_won_stats$w_df)+sum(grass_mtchs_lost_stats$l_df),
                   sum(grass_mtchs_won_stats$w_bpFaced)+sum(grass_mtchs_lost_stats$l_bpFaced),
                   round((sum(grass_mtchs_won_stats$w_1stIn)+sum(grass_mtchs_lost_stats$l_1stIn))/(sum(grass_mtchs_won_stats$w_svpt)+sum(grass_mtchs_lost_stats$l_svpt)),digits = 2),
                   round((sum(grass_mtchs_won_stats$w_1stWon)+sum(grass_mtchs_lost_stats$l_1stWon))/(sum(grass_mtchs_won_stats$w_1stIn)+sum(grass_mtchs_lost_stats$l_1stIn)),digits = 2),
                   round(1-(((sum(grass_mtchs_won_stats$w_bpFaced)+sum(grass_mtchs_lost_stats$l_bpFaced))-(sum(grass_mtchs_won_stats$w_bpSaved)+sum(grass_mtchs_lost_stats$l_bpSaved)))/(sum(grass_mtchs_won_stats$w_SvGms)+sum(grass_mtchs_lost_stats$l_SvGms))),digits = 2)
)

by_surface_stats[3,]<- c("clay",nrow(clay_mtchs_won_stats),nrow(clay_mtchs_lost_stats),
                   sum(clay_mtchs_won_stats$w_ace)+sum(clay_mtchs_lost_stats$l_ace),
                   sum(clay_mtchs_won_stats$w_df)+sum(clay_mtchs_lost_stats$l_df),
                   sum(clay_mtchs_won_stats$w_bpFaced)+sum(clay_mtchs_lost_stats$l_bpFaced),
                   round((sum(clay_mtchs_won_stats$w_1stIn)+sum(clay_mtchs_lost_stats$l_1stIn))/(sum(clay_mtchs_won_stats$w_svpt)+sum(clay_mtchs_lost_stats$l_svpt)),digits = 2),
                   round((sum(clay_mtchs_won_stats$w_1stWon)+sum(clay_mtchs_lost_stats$l_1stWon))/(sum(clay_mtchs_won_stats$w_1stIn)+sum(clay_mtchs_lost_stats$l_1stIn)),digits = 2),
                   round(1-(((sum(clay_mtchs_won_stats$w_bpFaced)+sum(clay_mtchs_lost_stats$l_bpFaced))-(sum(clay_mtchs_won_stats$w_bpSaved)+sum(clay_mtchs_lost_stats$l_bpSaved)))/(sum(clay_mtchs_won_stats$w_SvGms)+sum(clay_mtchs_lost_stats$l_SvGms))),digits = 2)
)
by_surface_stats[4,]<- c("carpet",nrow(carpet_mtchs_won_stats),nrow(carpet_mtchs_lost_stats),
                   sum(carpet_mtchs_won_stats$w_ace)+sum(carpet_mtchs_lost_stats$l_ace),
                   sum(carpet_mtchs_won_stats$w_df)+sum(carpet_mtchs_lost_stats$l_df),
                   sum(carpet_mtchs_won_stats$w_bpFaced)+sum(carpet_mtchs_lost_stats$l_bpFaced),
                   round((sum(carpet_mtchs_won_stats$w_1stIn)+sum(carpet_mtchs_lost_stats$l_1stIn))/(sum(carpet_mtchs_won_stats$w_svpt)+sum(carpet_mtchs_lost_stats$l_svpt)),digits = 2),
                   round((sum(carpet_mtchs_won_stats$w_1stWon)+sum(carpet_mtchs_lost_stats$l_1stWon))/(sum(carpet_mtchs_won_stats$w_1stIn)+sum(carpet_mtchs_lost_stats$l_1stIn)),digits = 2),
                   round(1-(((sum(carpet_mtchs_won_stats$w_bpFaced)+sum(carpet_mtchs_lost_stats$l_bpFaced))-(sum(carpet_mtchs_won_stats$w_bpSaved)+sum(carpet_mtchs_lost_stats$l_bpSaved)))/(sum(carpet_mtchs_won_stats$w_SvGms)+sum(carpet_mtchs_lost_stats$l_SvGms))),digits = 2)
)
for (i in 2:ncol(by_surface)){
  by_surface_stats[,i] <- as.numeric(by_surface_stats[,i])
}

## Andy Roddick hard court matches stats compared to all players stats

Roddick_vs_all_hard <- data.frame(player="", aces_avg=0,dfs_avg=0,bp_faced_avg=0,
                                  first_sv_pctg=0,first_sv_won_pctg=0,
                                  sv_gms_won_pctg_avg=0,
                                  row.names = NULL,stringsAsFactors = FALSE)
Roddick_vs_all_hard[1,] <- c("Roddick",round(by_surface_stats[1,4]/hrd_sts_count,digits = 2),round(by_surface_stats[1,5]/hrd_sts_count,digits = 2),
                        round(by_surface_stats[1,6]/hrd_sts_count,digits = 2), by_surface_stats[1,7],
                        by_surface_stats[1,8],by_surface_stats[1,9])

Roddick_vs_all_hard[2,] <- c("Player won", round(mean(hard_all$w_ace),digits = 2),
                             round(mean(hard_all$w_df),digits = 2),
                             round(mean(hard_all$w_bpFaced),digits = 2),
                             round(sum(hard_all$w_1stIn)/sum(hard_all$w_svpt),digits = 2),
                             round(sum(hard_all$w_1stWon)/sum(hard_all$w_1stIn),digits = 2),
                             round(1-((sum(hard_all$w_bpFaced)-
                                         sum(hard_all$w_bpSaved))
                                      /sum(hard_all$w_SvGms)),digits = 2))

Roddick_vs_all_hard[3,] <- c("Player lost", round(mean(hard_all$l_ace),digits = 2),
                             round(mean(hard_all$l_df),digits = 2),
                             round(mean(hard_all$l_bpFaced),digits = 2),
                             round(sum(hard_all$l_1stIn)/sum(hard_all$l_svpt),digits = 2),
                             round(sum(hard_all$l_1stWon)/sum(hard_all$l_1stIn),digits = 2),
                             round(1-((sum(hard_all$l_bpFaced)-
                                         sum(hard_all$l_bpSaved))
                                      /sum(hard_all$l_SvGms)),digits = 2))

## Andy Roddick grass court matches stats compared to all players stats

Roddick_vs_all_grass <- data.frame(player="", aces_avg=0,dfs_avg=0,bp_faced_avg=0,
                                  first_sv_pctg=0,first_sv_won_pctg=0,
                                  sv_gms_won_pctg_avg=0,
                                  row.names = NULL,stringsAsFactors = FALSE)
Roddick_vs_all_grass[1,] <- c("Roddick",round(by_surface_stats[2,4]/grs_sts_count,digits = 2),
                              round(by_surface_stats[2,5]/grs_sts_count,digits = 2),
                             round(by_surface_stats[2,6]/grs_sts_count,digits = 2), by_surface_stats[2,7],
                             by_surface_stats[2,8],by_surface_stats[2,9])
Roddick_vs_all_grass[2,] <- c("Player won", round(mean(grass_all$w_ace),digits = 2),
                             round(mean(grass_all$w_df),digits = 2),
                             round(mean(grass_all$w_bpFaced),digits = 2),
                             round(sum(grass_all$w_1stIn)/sum(grass_all$w_svpt),digits = 2),
                             round(sum(grass_all$w_1stWon)/sum(grass_all$w_1stIn),digits = 2),
                             round(1-((sum(grass_all$w_bpFaced)-
                                         sum(grass_all$w_bpSaved))
                                      /sum(grass_all$w_SvGms)),digits = 2))

Roddick_vs_all_grass[3,] <- c("Player lost", round(mean(grass_all$l_ace),digits = 2),
                             round(mean(grass_all$l_df),digits = 2),
                             round(mean(grass_all$l_bpFaced),digits = 2),
                             round(sum(grass_all$l_1stIn)/sum(grass_all$l_svpt),digits = 2),
                             round(sum(grass_all$l_1stWon)/sum(grass_all$l_1stIn),digits = 2),
                             round(1-((sum(grass_all$l_bpFaced)-
                                         sum(grass_all$l_bpSaved))
                                      /sum(grass_all$l_SvGms)),digits = 2))

## Andy Roddick clay court matches stats compared to all players stats

Roddick_vs_all_clay <- data.frame(player="", aces_avg=0,dfs_avg=0,bp_faced_avg=0,
                                   first_sv_pctg=0,first_sv_won_pctg=0,
                                   sv_gms_won_pctg_avg=0,
                                   row.names = NULL,stringsAsFactors = FALSE)
Roddick_vs_all_clay[1,] <- c("Roddick",round(by_surface_stats[3,4]/cly_sts_count,digits = 2),
                              round(by_surface_stats[3,5]/cly_sts_count,digits = 2),
                              round(by_surface_stats[3,6]/cly_sts_count,digits = 2), by_surface_stats[3,7],
                              by_surface_stats[3,8],by_surface_stats[3,9])
Roddick_vs_all_clay[2,] <- c("Player won", round(mean(clay_all$w_ace),digits = 2),
                              round(mean(clay_all$w_df),digits = 2),
                              round(mean(clay_all$w_bpFaced),digits = 2),
                              round(sum(clay_all$w_1stIn)/sum(clay_all$w_svpt),digits = 2),
                              round(sum(clay_all$w_1stWon)/sum(clay_all$w_1stIn),digits = 2),
                              round(1-((sum(clay_all$w_bpFaced)-
                                          sum(clay_all$w_bpSaved))
                                       /sum(clay_all$w_SvGms)),digits = 2))

Roddick_vs_all_clay[3,] <- c("Player lost", round(mean(clay_all$l_ace),digits = 2),
                              round(mean(clay_all$l_df),digits = 2),
                              round(mean(clay_all$l_bpFaced),digits = 2),
                              round(sum(clay_all$l_1stIn)/sum(clay_all$l_svpt),digits = 2),
                              round(sum(clay_all$l_1stWon)/sum(clay_all$l_1stIn),digits = 2),
                              round(1-((sum(clay_all$l_bpFaced)-
                                          sum(clay_all$l_bpSaved))
                                       /sum(clay_all$l_SvGms)),digits = 2))

## Andy Roddick hard carpet matches stats compared to all players stats

Roddick_vs_all_carpet <- data.frame(player="", aces_avg=0,dfs_avg=0,bp_faced_avg=0,
                                   first_sv_pctg=0,first_sv_won_pctg=0,
                                   sv_gms_won_pctg_avg=0,
                                   row.names = NULL,stringsAsFactors = FALSE)
Roddick_vs_all_carpet[1,] <- c("Roddick",round(by_surface_stats[4,4]/crpt_sts_count,digits = 2),
                              round(by_surface_stats[4,5]/crpt_sts_count,digits = 2),
                              round(by_surface_stats[4,6]/crpt_sts_count,digits = 2), by_surface_stats[4,7],
                              by_surface_stats[4,8],by_surface_stats[4,9])
Roddick_vs_all_carpet[2,] <- c("Player won", round(mean(carpet_all$w_ace),digits = 2),
                              round(mean(carpet_all$w_df),digits = 2),
                              round(mean(carpet_all$w_bpFaced),digits = 2),
                              round(sum(carpet_all$w_1stIn)/sum(carpet_all$w_svpt),digits = 2),
                              round(sum(carpet_all$w_1stWon)/sum(carpet_all$w_1stIn),digits = 2),
                              round(1-((sum(carpet_all$w_bpFaced)-
                                          sum(carpet_all$w_bpSaved))
                                       /sum(carpet_all$w_SvGms)),digits = 2))

Roddick_vs_all_carpet[3,] <- c("Player lost", round(mean(carpet_all$l_ace),digits = 2),
                              round(mean(carpet_all$l_df),digits = 2),
                              round(mean(carpet_all$l_bpFaced),digits = 2),
                              round(sum(carpet_all$l_1stIn)/sum(carpet_all$l_svpt),digits = 2),
                              round(sum(carpet_all$l_1stWon)/sum(carpet_all$l_1stIn),digits = 2),
                              round(1-((sum(carpet_all$l_bpFaced)-
                                          sum(carpet_all$l_bpSaved))
                                       /sum(carpet_all$l_SvGms)),digits = 2))


## creating new numeric vector which holds Andy Roddick opponent's rank
rnk_vec <- vector('numeric')
for ( q in 1:nrow(rod_all)) {
  if(rod_all[q,11]=="Andy Roddick"){rnk_vec[q] <-  rod_all[q,26]}
  else {rnk_vec[q] <-  rod_all[q,16]}
}

## creating new numeric vector which holds Andy Roddick aces number
aces_vec <- vector('numeric')
for ( q in 1:nrow(rod_all)) {
  if(rod_all[q,11]=="Andy Roddick"){aces_vec[q] <-  rod_all[q,32]}
    else {aces_vec[q] <-  rod_all[q,41]}
}

## creating new numeric vector which holds Andy Roddick double faults number
dfs_vec <- vector('numeric')
for ( q in 1:nrow(rod_all)) {
  if(rod_all[q,11]=="Andy Roddick"){dfs_vec[q] <-  rod_all[q,33]}
  else {dfs_vec[q] <-  rod_all[q,42]}
}

#saving Roddick's first serve percentage in a vector
first_srv_pctg_vec <- rod_all[,51]
#saving match duration in a vector
minutes_vec <- rod_all$minutes

## creating new numeric vector which holds surface weight
surface_vec <- vector('numeric')
for ( q in 1:nrow(rod_all)) {
  if(rod_all[q,3]=="Hard"){surface_vec[q] <-  2*0.76}
  else if(rod_all[q,3]=="Grass"){surface_vec[q] <-  2*0.8}
  else  if(rod_all[q,3]=="Clay"){surface_vec[q] <-  2*0.64}
  else  {surface_vec[q] <-  2*0.71}
}


## creating data frame contains the previous factors in Andy Roddick matches
factors_table <- data.frame(winner="",opponent_rank =rnk_vec ,aces= aces_vec,double_faults = dfs_vec, 
                            surface = surface_vec,first_serve_percentage=first_srv_pctg_vec,minutes= minutes_vec, stringsAsFactors = FALSE)
for (k in 1:803)
{
  factors_table[k,1]<-rod_all[k,11]
}

# Opponent's ability - minutes ratio
factors_table$plab <- 0
# Andy Roddick serve efficiency ratio 
factors_table$srv <- 0
# Final result of the fonction
factors_table$result <- 0

#calculating each component of the mathematical model and aggregate the outcome in result column
for(k in 1:803)
{
factors_table[k,8] <- round(4*factors_table[k,2]/(3*log(factors_table[k,7])),2)
factors_table[k,9] <- round(((factors_table[k,3]*(factors_table[k,6]))-5.6*factors_table[k,4])/
    (factors_table[k,5]),2)
  res <- (0.122)*(factors_table[k,8]+factors_table[k,9])
  factors_table[k,10]<- round(res,digits = 2)
}

# The mean of the models outcome column
mean(factors_table$result)
# The mean of the players ability column
mean(factors_table$plab)
# The mean of the serve efficiency column
mean(factors_table$srv)

# calculate the models precision of predicting the outcome (won/lost) of the matches
won_pred_vec <- factors_table$result>1& factors_table$winner=="Andy Roddick"
lost_pred_vec <- factors_table$result<=1& factors_table$winner!="Andy Roddick"
won_prec_pctg <- sum(won_pred_vec)*(100/594)
lost_prec_pctg <- sum(lost_pred_vec)* (100/209)

#saving matches won correctly predicted into data frame
won_prdctd <- factors_table[won_pred_vec,]
#saving matches lost correctly predicted into data frame
lost_prdctd <- factors_table[lost_pred_vec,]

summary(factors_table)
cor(rod_all$rod_bpfaced,rod_all$rod_dfs)
cor(rod_all$rod_bpfaced,rod_all$rod_first_sv_won_pctg)
t.test(rod_all_won$w_ace,rod_all_lost$l_ace)

frst_srv_coch <- c(mean(rod_all$rod_first_sv_pctg[rod_all$coach=="Tarik Benhabiles"])
       ,mean(rod_all$rod_first_sv_pctg[rod_all$coach=="Brad Gilbert"])
       ,mean(rod_all$rod_first_sv_pctg[rod_all$coach=="Dean Goldfine"])
        ,mean(rod_all$rod_first_sv_pctg[rod_all$coach=="John Roddick"])
       ,mean(rod_all$rod_first_sv_pctg[rod_all$coach=="Jimmy Connors"])
       ,mean(rod_all$rod_first_sv_pctg[rod_all$coach=="Larry Stefanki"]))

#plot(frst_srv_coch,type = "l",ylim = range(0.55),xlab = "coach",ylab = "First Serve percentage", 
 #    main = "Andy Roddick First Serve In percentage by coach", sub="plot showing the percentages for every coach")

#plot(frst_srv_coch,type = "o",ylim = range(0.55),xlab = "coach",ylab = "First Serve percentage", 
 #    main = "Andy Roddick First Serve In percentage by coach", sub="plot showing the percentages for every coach")

frst_srv_coch_mtchs_won <- c (mean(rod_all_won$rod_first_sv_pctg[rod_all_won$coach=="Tarik Benhabiles"])
                            ,mean(rod_all_won$rod_first_sv_pctg[rod_all_won$coach=="Brad Gilbert"])
                            ,mean(rod_all_won$rod_first_sv_pctg[rod_all_won$coach=="Dean Goldfine"])
                            ,mean(rod_all_won$rod_first_sv_pctg[rod_all_won$coach=="John Roddick"])
                            ,mean(rod_all_won$rod_first_sv_pctg[rod_all_won$coach=="Jimmy Connors"])
                            ,mean(rod_all_won$rod_first_sv_pctg[rod_all_won$coach=="Larry Stefanki"]))
plot(frst_srv_coch_mtchs_won,type = "l",ylim = range(0.55),xlab = "coach",ylab = "First Serve percentage", 
      main = "Andy Roddick First Serve In percentage by coach", sub="plot showing the percentages for every coach")

frst_srv_coch_mtchs_lost <- c (mean(rod_all_lost$rod_first_sv_pctg[rod_all_lost$coach=="Tarik Benhabiles"])
                              ,mean(rod_all_lost$rod_first_sv_pctg[rod_all_lost$coach=="Brad Gilbert"])
                              ,mean(rod_all_lost$rod_first_sv_pctg[rod_all_lost$coach=="Dean Goldfine"])
                              ,mean(rod_all_lost$rod_first_sv_pctg[rod_all_lost$coach=="John Roddick"])
                              ,mean(rod_all_lost$rod_first_sv_pctg[rod_all_lost$coach=="Jimmy Connors"])
                              ,mean(rod_all_lost$rod_first_sv_pctg[rod_all_lost$coach=="Larry Stefanki"]))
plot(frst_srv_coch_mtchs_lost,type = "l",ylim = range(0.55),xlab = "coach",ylab = "First Serve percentage", 
     main = "Andy Roddick First Serve In percentage by coach", sub="plot showing the percentages for every coach",col="blue")
lines(frst_srv_coch_mtchs_won, col="red")
legend("topleft",
       c("Matches Lost","Matches Won"),
       fill=c("blue","red")
)
