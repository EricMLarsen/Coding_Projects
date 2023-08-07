############################################################
####### Query NWIS for WEST CRACK data 
### CREATED 8-3-23, C. RUMSEY (Beginning Code)
### Modified and Adapted by E. Larsen 8/4/2023
#######################################

### install packages (only need to do once)
#install.packages("dataRetrieval","EGRET")
#install.packages('dplyr')
#load necessary packages
library(dataRetrieval)
library(EGRET)
library(lubridate)

#Clear the Console
cat("\014")


#####################################################
#### File Directory DATA given for saving purposes
#####################################################

#Designate Path to Desired Directory
file_path = 'C:\\Users\\Eric_Larsen\\Desktop\\R_Code\\'
filegivenname = 'USGS_Measurements.csv'
PULLDATE = gsub('-','',Sys.Date())
savelocation = paste(file_path, filegivenname, sep = '')

### Data Pull dates
### Dates need to be of format  "YYYY-MM-DD"
##      USE: "" for all time bounds
userinput_logic <- readline(prompt = 'User Input Dates?: (TRUE/FALSE)')

if(userinput_logic){
  print("Input start date: (i.e. YYYY-MM-DD)")
  dateBEGIN = readline()
  print("Input end date: (i.e. YYYY-MM-DD)")
  dateEND = readline()
}else{ #Pre determined dates given by the program
  dateBEGIN = "2023-05-01"
  dateEND = "2023-08-07"
}

################################################################################
#### Monthly Field Measurement DATA     ########################################
################################################################################

## Pull field measurements of discharge (ADCP)

  StoNmonth.cfs <- readNWISmeas("10010025",startDate= "",endDate= "") ### discharge units in cfs
  NtoSmonth.cfs <- readNWISmeas("10010026",startDate= "",endDate= "") ### discharge units in cfs

  #Refine the Discharge measurement data frames to give, date, elevation, discharge only
    StoNmonth.cfs <- StoNmonth.cfs[,c(2,4,9,10)]
    NtoSmonth.cfs <- NtoSmonth.cfs[,c(2,4,9,10)]
  
    colnames(StoNmonth.cfs) <-(c('site_no','measurement_dt','Gage_Height_S','SN_Q_CFS'))
    colnames(NtoSmonth.cfs) <-(c('site_no','measurement_dt','Gage_Height_N','NS_Q_CFS'))

  # merge into one file
    GSL.Q.Measurement <- merge(StoNmonth.cfs,NtoSmonth.cfs,by='measurement_dt',all=TRUE)
    colnames(GSL.Q.Measurement) <- (c('measurement_dt','site_no','Gage_Height_S','SN_Q_CFS','site_no','Gage_Height_N','NS_Q_CFS'))
  #View(GSL.Q.Measurement)
    # save file
      finame = 'Monthly_Q_Data_Pulled_'
      filegivenname = paste(finame,PULLDATE, sep='')
      filegivenname = paste(filegivenname,".csv",sep='')
      savelocation = paste(file_path, filegivenname, sep = '')
      write.csv(GSL.Q.Measurement,file=savelocation)
      #Monthly Data Successful
  
## Pull density data at West Crack

  parmCd <- c("72263") # parameter 72263 is density
  readNWISpCode(parmCd) ## info on parameter code
  
  densityStoN <- readNWISqw("10010025",parameterCd=parmCd,startDate= "",endDate= "")
  densityNtoS <- readNWISqw("10010026",parameterCd=parmCd,startDate= "",endDate= "")

  densityStoN <- densityStoN[,c('site_no','sample_dt','result_va')]
  densityNtoS <- densityNtoS[,c('site_no','sample_dt','result_va')]
  
  colnames(densityStoN) <- (c('site_no','measurement_dt','S_Density'))
  colnames(densityNtoS) <- (c('site_no','measurement_dt','N_Density'))
  
  density.combined = merge(densityStoN,densityNtoS,by = 'measurement_dt',all = TRUE)
  #View(density.combined)
  
  #Save Density Data
      finame = 'Monthly_Density_Data_Pulled_'
      filegivenname = paste(finame,PULLDATE, sep='')
      filegivenname = paste(filegivenname,".csv",sep='')
      savelocation = paste(file_path, filegivenname, sep = '')
      write.csv(density.combined,file=savelocation)
      
## Pull Specific Conductance data at West Crack

  parmCd <- c("00095") # parameter 00095 is specific conductance
  readNWISpCode(parmCd) ## info on parameter code
  
  SpCondStoN <- readNWISqw("10010025",parameterCd=parmCd,startDate= "",endDate= "")
  SpCondNtoS <- readNWISqw("10010026",parameterCd=parmCd,startDate= "",endDate= "")
  
  SpCondStoN <- SpCondStoN[,c('site_no','sample_dt','result_va')]
  SpCondNtoS <- SpCondNtoS[,c('site_no','sample_dt','result_va')]
  
  colnames(SpCondStoN) <- (c('site_no','measurement_dt','S_SpCond'))
  colnames(SpCondNtoS) <- (c('site_no','measurement_dt','N_SpCond'))

  SpCond.combined = merge(SpCondStoN,SpCondNtoS,by = 'measurement_dt',all = TRUE)
  #View(SpCond.combined)
  #View(SpCond.combined)

  #Save Density Data
    finame = 'Monthly_SpCond_Data_Pulled_'
    filegivenname = paste(finame,PULLDATE, sep='')
    filegivenname = paste(filegivenname,".csv",sep='')
    savelocation = paste(file_path, filegivenname, sep = '')
    write.csv(SpCond.combined,file=savelocation)
    
  
## Combine Monthly Measurements total and put together
      
  density.combined <- density.combined[,c(1,3,5)]
  #View(density.combined)
  GSL.Q.Measurement <- GSL.Q.Measurement[,c(1,3,4,6,7)]
  #View(GSL.Q.Measurement)
  Monthly.total <- merge(GSL.Q.Measurement,density.combined,by='measurement_dt',all=TRUE)
  SpCond.combined <- SpCond.combined[,c(1,3,5)]
  Monthly.total <- merge(GSL.Q.Measurement,SpCond.combined,by='measurement_dt',all=TRUE)
  
    #Save Data
      finame = 'Monthly_Measurements_Data_Pulled_'
      filegivenname = paste(finame,PULLDATE, sep='')
      filegivenname = paste(filegivenname,".csv",sep='')
      savelocation = paste(file_path, filegivenname, sep = '')
      write.csv(Monthly.total,file=savelocation)
      #Successful
    
#################################################################################
#####   15-min Interval data          ###########################################
#################################################################################
    
  
## Pull Cell Velocity Data

  parmCd = 72254
  Cell.all <- readNWISuv(c("10010025"),parameterCd = parmCd, startDate = dateBEGIN,endDate = dateEND)
  Cell.all <- Cell.all[,-c(5,7,9,11,13,15,17,19,21,23,25,26)]
  colnames(Cell.all) <- c('Agency','site_no','dateTime','cell1','cell4','cell2','cell3','cell7','cell10','cell5','cell9','cell8','cell6')
  Cell.all['cell0'] <- 0
  #Cell.velocities <- Cell.all[,c('cell1','cell2','cell3','cell4','cell5','cell6','cell7','cell8','cell9','cell10')]
  #View(Cell.all)
  Cell.all <- Cell.all[,c('Agency','site_no','dateTime','cell0','cell1','cell2','cell3','cell4','cell5','cell6','cell7','cell8','cell9','cell10')]
  #View(Cell.all)
  
  #Save Cell data
    finame = 'Cell_Velocity_Data_Pulled_'
    filegivenname = paste(finame,PULLDATE, sep='')
    filegivenname = paste(filegivenname,".csv",sep='')
    savelocation = paste(file_path, filegivenname, sep = '')
    write.csv(Cell.all,file=savelocation)
  

## Pull the wind speed and wind direction data
  
  parmCd = '00035'
  Wind.Speed <- readNWISuv(c("10010025"),parameterCd = parmCd,startDate =dateBEGIN,endDate = dateEND)
  Wind.Speed <- Wind.Speed[,c(3,4)]
  parmCd = '00036'
  Wind.Dir <- readNWISuv(c("10010025"),parameterCd = parmCd,startDate = dateBEGIN, endDate = dateEND)
  Wind.Dir <- Wind.Dir[,c(3,4)]
  Wind.total <- merge(Wind.Speed,Wind.Dir,by='dateTime',all=TRUE)
  Wind.total['site_no'] <- 10010025
  colnames(Wind.total) <- c('dateTime','Wind_Spd','Wind_Dir','site_no')
  Wind.total <- Wind.total[,c('site_no','dateTime','Wind_Spd','Wind_Dir')]
  
  #save wind data
    finame = 'Wind_Data_Pulled_'
    filegivenname = paste(finame,PULLDATE, sep='')
    filegivenname = paste(filegivenname,".csv",sep='')
    savelocation = paste(file_path, filegivenname, sep = '')
    write.csv(Wind.total,file=savelocation)
  
  
### Pull water surface elevation from gages north and south of the causeway
    
    WSEs.alone <- readNWISuv(c('10010024'),parameterCd='62614',startDate = dateBEGIN,endDate = dateEND)
    WSEb.alone <- readNWISuv(c('10010025'),parameterCd='62614',startDate = dateBEGIN,endDate = dateEND)
    WSEb.alone <- WSEb.alone[,c(2,3,4)]
    WSEn.alone <- readNWISuv(c('10010027'),parameterCd='62614',startDate = dateBEGIN,endDate = dateEND)
    #View(WSEn.alone)
    #View(WSEs.alone)
    WSEboth.combined = merge(WSEs.alone,WSEn.alone, by = 'dateTime',all.x = TRUE, all.y=TRUE)
    WSEall.combined = merge(WSEboth.combined,WSEb.alone,by = 'dateTime', all=TRUE)
    WSEall.combined <- WSEall.combined[,-c(5,6,7,10,11)]
    colnames(WSEall.combined) <- c('dateTime','Agency','site_no','WSES','site_no','WSEN','site_no','WSEB')

  #Save WSE Data
    finame = 'WSE_Data_Pulled_'
    filegivenname = paste(finame,PULLDATE, sep='')
    filegivenname = paste(filegivenname,".csv",sep='')
    savelocation = paste(file_path, filegivenname, sep = '')
    write.csv(WSEall.combined,file=savelocation)



################################################################################
####  Combining and Saving Full DATA pull information:  15 Minute Interval #####
################################################################################
  
  
### Save The data extracted from the data tables

  #Create SubSets of each data file to be used in one combined measurement table
    
    #Cell Data
    
      #Skipping over this section to make the Cell.all the base of the merge commands
    
    #Wind Data
    
      Wind.sub <- Wind.total[,c('dateTime','Wind_Spd','Wind_Dir')]
    
    #WSE Data
    
      WSE.sub <- WSEall.combined[,c('dateTime','WSEN','WSEB','WSES')]
    
  #Merge DATA
  
    Cell.all <- merge(Cell.all,Wind.sub,by='dateTime',all=TRUE)
    Cell.all <- merge(Cell.all,WSE.sub,by='dateTime',all=TRUE)
    
  #Save DATA
    
    finame = 'GSL_Condition_Data_Pulled_'
    filegivenname = paste(finame,PULLDATE, sep='')
    filegivenname = paste(filegivenname,".csv",sep='')
    savelocation = paste(file_path, filegivenname, sep = '')
    write.csv(Cell.all,file=savelocation)
  
  
  
  
  
  
  
  


