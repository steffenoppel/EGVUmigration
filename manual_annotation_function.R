#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANUAL ANNOTATION FUNCTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  StartDate <- readStartDate()
  EndDate <- readEndDate()
  
  ### CAPTURE OUTPUT FOR CALIBRATION 
  THRESH_calib<-data.frame('id.yr.season'=a) %>%
    mutate(start=if_else(is.null(StartDate),THRESH_start,StartDate)) %>%
    mutate(end=if_else(is.null(EndDate),THRESH_end,EndDate))
  
  
  ### ~~~~~~~~~ 5. SAVE DATA AND CLEAN UP ~~~~~~~~~~~~~~~~ ###
    mig_dates<-rbind(mig_dates,THRESH_calib)
    fwrite(mig_dates,"EGVU_migration_dates_manually_classified_PART3.csv")
    rm(THRESH_end,THRESH_start,x,xmig,xlim,ylim,mig_time,distgraph,THRESH_calib)
    
    print(sprintf("finished with migration journey %s",a))