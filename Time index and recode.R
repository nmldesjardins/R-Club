library(foreign)

pdr2<-read.spss("PDR Wave 2.sav", to.data.frame=T)
pdr4<-read.spss("PDR Wave 4.sav", to.data.frame=T)
head(pdr2)
summary(pdr2)


## generate a time variable (indexes each call for each family)
library(dplyr)
pdr2_time<-pdr2 %>%
group_by(FAMILY) %>%  #do the count by family
arrange(YEAR,MONTH,DAY) %>% #sort by date
mutate(callindex=1:n()) #create call index that's 1:end for each family

head(pdr2_time)

### create composite score of the kid bex (cols 8:47)

library(car)
?recode

## recode all of the kid behavior items into numeric variables in one go
# ignore difference between "occurred, stressed" and "occurred, not stressed"
# (if it occurred at all, it gets a 1; if it didn't occur, it's a 0)
# the defaults for as.XX.result are both TRUE; if you leave it that way,
# it will return character variables.

pdr2_time[,8:47]<-sapply(pdr2_time[,8:47],
                        function(x) 
{x<-recode(x,"'DID NOT OCCUR'='0'; else = '1'", 
           as.factor.result=F, as.numeric.result=T)}) 

head(pdr2_time)
str(pdr2_time)

## now create the composite (sum) variable
pdr2_time$bextot<-rowSums(pdr2_time[,8:47],na.rm=F)

summary(pdr2_time$bextot)

