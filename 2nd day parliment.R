prlmt <- read.csv(file = "D:/Data Science 2017/Manipal Academy of Data Science/MGADS TERM2/2 Data Visualization/parliament.csv")
prlmt
str(prlmt)

library(dplyr)
#total no of electors
length(unique(prlmt$YEAR))

#total n0o of parties

length(unique(prlmt$PARTY))

#total no of parties by year
 
prty <- prlmt %>% group_by(YEAR) %>% summarize(parties = length(unique(PARTY)))
prty

#total no of candidates from inc yearwise

yw <- prlmt %>% group_by(YEAR) %>% filter(PARTY=="INC") %>% summarize(count=n())
yw

# total no of candidates who won from inc yearwise

yon <- prlmt %>% group_by(YEAR) %>% filter(PARTY=="INC",Position==1) %>% summarize(count=n())
yon

# bar & Line plots
library(ggplot2)
tj <- ggplot(yw , aes(x= YEAR, y= count)) + geom_bar(stat = 'identity') 
tj

tj1 <- ggplot(yw , aes(x= YEAR, y= count)) + geom_line(stat = 'identity') 
tj1

tj <- tj + coord_flip()
tj

#top 10 parties by wins no 1 position order
tt <- prlmt %>% group_by(PARTY) %>% filter(Position==1) %>% summarize(wins = n()) %>% arrange(desc(wins)) %>% head(10)
tt
tt1 <- tt$PARTY
tt1

#parties top names

tp <- prlmt %>% filter((PARTY %in% tt1))
tp


#dim party

dim(tp)

#top party wins
tpw <- tp %>% filter(Position==1) %>% group_by(YEAR,PARTY) %>% summarize(wins=n())
tpw

#ggplot

ggparl <- ggplot(tpw, aes(x= YEAR,y=wins, fill=PARTY)) + geom_bar(stat = 'identity')
ggparl

#state wise no of wins by top 10 parties for the year 2009

state <- tp %>% group_by(STATE,PARTY) %>% filter(YEAR==2009, Position==1) %>% summarize(winss = n()) %>% head(10)
state

#top 10 state wise no of wins by top 10 parties for the year 2009

top10state <- prlmt  %>% filter(YEAR==2009) %>% group_by(STATE,PC) %>% summarize(ELECTORS=min(ELECTORS))
top10state

state <- top10state %>% group_by(STATE) %>%  summarize(ELECTORS = sum(ELECTORS))
state

stat_sum <- state %>% group_by(STATE) %>% summarize(ELECTORS = sum(ELECTORS)) %>% arrange(-ELECTORS) %>% head(10)
stat_sum

top_state <- prlmt %>% filter( STATE %in% stat_sum$STATE)
top_state

state_party <- top_state %>% filter(YEAR==2009, Position==1, PARTY %in% tt1) %>% group_by(STATE,PARTY) %>% summarize(wins=n())
state_party
nrow(state_party)

#ggplot

ggplot(state_party,aes(x=STATE,y=wins,fill=PARTY)) + geom_bar(stat = 'identity')


#for karnataka compute year wise total votes & % votes
# year   total votes total electors % votes

kr <- prlmt %>% filter(STATE=="KARNATAKA")
kr
kr_summary <- kr %>% group_by(YEAR,PC) %>% summarize(VOTES=sum(VOTES,na.rm=TRUE),
                      ELECTORS=min(ELECTORS)) %>% group_by(YEAR) %>% summarize(VOTES=sum(VOTES),
                                    ELECTORS = sum(ELECTORS),per_votes= VOTES/ELECTORS*100)
kr_summary
kr_summary$YEAR = as.factor(kr_summary$YEAR)

gg <- ggplot(kr_summary,aes(x=YEAR,y=ELECTORS,fill=-per_votes)) + geom_bar(stat = 'identity')
gg
 

