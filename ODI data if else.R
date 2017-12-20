odi <- read.csv(file = "D:/Data Science 2017/Manipal Academy of Data Science/MGADS TERM2/2 Data Visualization/odi-batting.csv")
odi

library(dplyr)
odi <- odi %>% mutate(century = if_else(Runs>=100 & Runs < 200 , 1,0))
odi
View(odi)

odi <- odi %>% mutate(h.century = if_else(Runs >=50 & Runs <100 , 1,0))
odi
View(odi)

odi <- odi %>% mutate(dkls = if_else(Runs==0,1,0))
odi
View(odi)

odi <- odi %>% mutate(ninty =if_else(Runs > 90 & Runs <100, 1,0))
odi

View(odi)



# top ten players by total numbers

pl <- odi %>% group_by(Player) %>% summarize(totalruns = sum(Runs)) %>% arrange(desc(totalruns))
pl
a<-pl[1:10,]
View(a)

#top ten players by total centuries

pc <- odi %>% group_by(Player) %>% summarize(totalcunturies = sum(century)) %>% arrange(desc(totalcunturies))
pc
b<- pc[1:10,]
b

# year wise toatl runs by sachin R Tendulkar

odi <- odi %>% mutate(Year= format(as.Date(odi $MatchDate,'%m-%d-%Y'),'%Y'))
odi
ps <- odi %>% filter(Player=="Sachin R Tendulkar") %>% group_by(Year) %>% summarize(totalruns = sum(Runs))
ps
c<- ps[1:10,]
c

#Filter for india and indentify top ten players in terms number of matches they have played so far

pi <- odi %>% filter(Country== "India") %>% group_by(Player) %>% summarize(count=n()) %>% arrange(desc(count))

pi
d<- pi[1:10,]
d

# Plot cumulative number of centuries scored by sachin R tendulkar across year
pn <- odi %>% 
e <- plot()
