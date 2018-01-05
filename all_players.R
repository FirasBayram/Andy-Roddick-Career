# Loading all ATP matches during Andy Roddick career in data frames

all_2000 <- read.csv("tennisdata-master/atp_matches_2000.csv",stringsAsFactors = FALSE) 
all_2001 <- read.csv("tennisdata-master/atp_matches_2001.csv",stringsAsFactors = FALSE) 
all_2002 <- read.csv("tennisdata-master/atp_matches_2002.csv",stringsAsFactors = FALSE) 
all_2003 <- read.csv("tennisdata-master/atp_matches_2003.csv",stringsAsFactors = FALSE) 
all_2004 <- read.csv("tennisdata-master/atp_matches_2004.csv",stringsAsFactors = FALSE) 
all_2005 <- read.csv("tennisdata-master/atp_matches_2005.csv",stringsAsFactors = FALSE) 
all_2006 <- read.csv("tennisdata-master/atp_matches_2006.csv",stringsAsFactors = FALSE) 
all_2007 <- read.csv("tennisdata-master/atp_matches_2007.csv",stringsAsFactors = FALSE) 
all_2008 <- read.csv("tennisdata-master/atp_matches_2008.csv",stringsAsFactors = FALSE) 
all_2009 <- read.csv("tennisdata-master/atp_matches_2009.csv",stringsAsFactors = FALSE) 
all_2010 <- read.csv("tennisdata-master/atp_matches_2010.csv",stringsAsFactors = FALSE)
#remove unwanted columns from the file above
all_2010$atp_key <-   NULL
all_2010$w_ln <- NULL
all_2010$l_ln <- NULL
all_2011 <- read.csv("tennisdata-master/atp_matches_2011.csv",stringsAsFactors = FALSE) 
all_2012 <- read.csv("tennisdata-master/atp_matches_2012.csv",stringsAsFactors = FALSE) 
#remove unwanted columns from the file above
all_2012$atp_key <-   NULL
all_2012$w_ln <- NULL
all_2012$l_ln <- NULL
all_2012$W.LN <- NULL
all_2012$W.Key <- NULL
all_2012$L.LN <- NULL
all_2012$L.Key <- NULL
# combine all the matches in one data frame after making all data frame columns match the others
all_mtchs <- do.call("rbind", list(all_2000,all_2001 ,all_2002,all_2003,all_2004,all_2005,all_2006,
                            all_2007,all_2008,all_2009,all_2010,all_2011,all_2012))
write.csv(all_mtchs,"all.csv", row.names = FALSE)


#remove the matches which don't have recorded stats
nrow(all_mtchs[all_mtchs$l_svpt ==0,])
all_mtchs$w_svpt <-  as.numeric(all_mtchs$w_svpt)
all_mtch_filtered <- subset(all_mtchs,all_mtchs$w_svpt!=0)
write.csv(all_mtch_filtered,"all_filtered.csv", row.names = FALSE)

# Creating data frame highlighting players performance on serve
grass_all <- all_mtch_filtered[all_mtch_filtered$surface=="Grass",]
hard_all <- all_mtch_filtered[all_mtch_filtered$surface=="Hard",]
clay_all <- all_mtch_filtered[all_mtch_filtered$surface=="Clay",]
carpet_all <- all_mtch_filtered[all_mtch_filtered$surface=="Carpet",]
all_by_surface <- data.frame(surface="", matches=0,  aces=0,dfs=0,bp_faced=0,
                             first_sv_pctg=0,first_sv_won_pctg=0,sv_gms_won_pctg=0,
                             row.names = NULL,stringsAsFactors = FALSE)
all_by_surface[1,]<- c("hard",nrow(hard_all),
                   sum(hard_all$w_ace)+sum(hard_all$l_ace),
                   sum(hard_all$w_df)+sum(hard_all$l_df),
                   sum(hard_all$w_bpFaced)+sum(hard_all$l_bpFaced),
                   round((sum(hard_all$w_1stIn)+sum(hard_all$l_1stIn))/(sum(hard_all$w_svpt)+sum(hard_all$l_svpt)),digits = 2),
                   round((sum(hard_all$w_1stWon)+sum(hard_all$l_1stWon))/(sum(hard_all$w_1stIn)+sum(hard_all$l_1stIn)),digits = 2),
                   round(1-(((sum(hard_all$w_bpFaced)+sum(hard_all$l_bpFaced))-
                               (sum(hard_all$w_bpSaved)+sum(hard_all$l_bpSaved)))
                            /(sum(hard_all$w_SvGms)+sum(hard_all$l_SvGms))),digits = 2))
all_by_surface[2,]<- c("grass",nrow(grass_all),
                   sum(grass_all$w_ace)+sum(grass_all$l_ace),
                   sum(grass_all$w_df)+sum(grass_all$l_df),
                   sum(grass_all$w_bpFaced)+sum(grass_all$l_bpFaced),
                   round((sum(grass_all$w_1stIn)+sum(grass_all$l_1stIn))/(sum(grass_all$w_svpt)+sum(grass_all$l_svpt)),digits = 2),
                   round((sum(grass_all$w_1stWon)+sum(grass_all$l_1stWon))/(sum(grass_all$w_1stIn)+sum(grass_all$l_1stIn)),digits = 2),
                   round(1-(((sum(grass_all$w_bpFaced)+sum(grass_all$l_bpFaced))-
                               (sum(grass_all$w_bpSaved)+sum(grass_all$l_bpSaved)))
                            /(sum(grass_all$w_SvGms)+sum(grass_all$l_SvGms))),digits = 2))

all_by_surface[3,]<- c("clay",nrow(clay_all),
                   sum(clay_all$w_ace)+sum(clay_all$l_ace),
                   sum(clay_all$w_df)+sum(clay_all$l_df),
                   sum(clay_all$w_bpFaced)+sum(clay_all$l_bpFaced),
                   round((sum(clay_all$w_1stIn)+sum(clay_all$l_1stIn))/(sum(clay_all$w_svpt)+sum(clay_all$l_svpt)),digits = 2),
                   round((sum(clay_all$w_1stWon)+sum(clay_all$l_1stWon))/(sum(clay_all$w_1stIn)+sum(clay_all$l_1stIn)),digits = 2),
                   round(1-(((sum(clay_all$w_bpFaced)+sum(clay_all$l_bpFaced))-(sum(clay_all$w_bpSaved)+sum(clay_all$l_bpSaved)))/(sum(clay_all$w_SvGms)+sum(clay_all$l_SvGms))),digits = 2))
all_by_surface[4,]<- c("carpet",nrow(carpet_all),
                   sum(carpet_all$w_ace)+sum(carpet_all$l_ace),
                   sum(carpet_all$w_df)+sum(carpet_all$l_df),
                   sum(carpet_all$w_bpFaced)+sum(carpet_all$l_bpFaced),
                   round((sum(carpet_all$w_1stIn)+sum(carpet_all$l_1stIn))/(sum(carpet_all$w_svpt)+sum(carpet_all$l_svpt)),digits = 2),
                   round((sum(carpet_all$w_1stWon)+sum(carpet_all$l_1stWon))/(sum(carpet_all$w_1stIn)+sum(carpet_all$l_1stIn)),digits = 2),
                   round(1-(((sum(carpet_all$w_bpFaced)+sum(carpet_all$l_bpFaced))-(sum(carpet_all$w_bpSaved)+sum(carpet_all$l_bpSaved)))/(sum(carpet_all$w_SvGms)+sum(carpet_all$l_SvGms))),digits = 2))
for (i in 2:ncol(all_by_surface)){
  all_by_surface[,i] <- as.numeric(all_by_surface[,i])
}
