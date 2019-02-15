#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANUAL THRESHOLD FUNCTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x<-migration %>% filter(id.yr.season==a) %>% mutate(Day=as.Date(DateTime)) %>%
  	mutate(DateTime=ymd_hms(DateTime))
		

  ### ~~~~~~~~~ 1. DEFINE START AND END DATES WITH SIMPLE THRESHOLDS ~~~~~~~~~~~~~~~~ ###
  ## MIGRATION STARTS WHEN DIST TO HOME CONTINUOUSLY INCREASES
  
  dailyhomedist<- x  %>% group_by(Day) %>%
    summarise(away=max(home_dist))
  
  ### find the first day where home_dist is greater than on any day before, and where home_dist is greater on any day afterwards
  THRESH_start<-NA
  for (d in 2:(dim(dailyhomedist)[1]-1)){
    maxbef<-max(dailyhomedist$away[1:(d-1)])
    minaft<-min(dailyhomedist$away[(d+1):dim(dailyhomedist)[1]])
    dmax<-d
    if(is.na(THRESH_start)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<minaft){THRESH_start<-dailyhomedist$Day[d]} 
    }
    if(is.na(THRESH_start)==FALSE) break
  }  # end loop over every day in the data set
  
  
  ### going backwards, find the first day where home_dist is smaller than on any day afterwards, and where home_dist is smaller on any day before
  THRESH_end<-NA
  for (d in (dim(dailyhomedist)[1]):dmax){
    maxbef<-max(dailyhomedist$away[dmax:(d-1)])
    minaft<-min(dailyhomedist$away[(d):dim(dailyhomedist)[1]])
    if(is.na(THRESH_end)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<=minaft){THRESH_end<-dailyhomedist$Day[d]} 
    }
    
    # if(is.na(start)==FALSE & is.na(end)==TRUE) {     ## prevent that the end is defined before the start
    #   if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]>=minaft){end<-dailyhomedist$Day[d]}
    # }
    if(is.na(THRESH_end)==FALSE) break
  }  # end loop over every day in the data set


mig_time<-interval(start=THRESH_start,end=THRESH_end)
x<- x %>% mutate(MIG=if_else(Day %within% mig_time,"migrating","stationary")) %>%
mutate(MIG=if_else(is.na(MIG),"stationary",MIG))

xmig<- x %>% filter(MIG=="migrating")
xlim<-c(min(x$long)-1,max(x$long)+1)
ylim<-c(min(x$lat)-1,max(x$lat)+1)

### TO AVOID SUMMARY LATER ON COLLAPSING WE FILL IN NONSENSE VALUES INSTEAD IF NA
if(is.na(THRESH_start)==TRUE) {THRESH_start<-ymd("1899-01-01")}
if(is.na(THRESH_end)==TRUE) {THRESH_end<-ymd("1899-01-01")}