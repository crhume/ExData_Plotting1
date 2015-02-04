# Code for Plot 4 of Project 1 from Experimental Data Analysis

## Quick Instructions:
##  Source plotX.R  X=(1,2,3,4) depending which you want.
##  To "see" the plot, use function "drawPlot()"
##  To export the png file, use function "exportPlot()"

## First assure that correct packages are loaded
require("dplyr")
require("lubridate")

## Create function to load and process data, will only be used
##  if needed
loadProjectData <- function(){
    pdt <- 
        read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings="?") %>% 
        tbl_df() %>% # I prefer to use these data frame tables
        mutate(Date=dmy(Date)) %>% # Change the date to a lubridate object
        filter(Date >= ymd("2007-02-01"),Date <= ydm("2007-02-02")) %>% # reduce size
        mutate(Time=hms(Time)) %>% # Change the Time to a lubridate object
        mutate(DT=ymd_hms(Date+Time))  # Add a DateTime column for graphs 2-4
}

drawPlot <- function(){
    par(mfcol=c(2,2))
    with(kcz_crs04_prj1_temp_dt, {
        plot(DT,Global_active_power,type="l",ylab="Global Active Power (kilowatts)")
        plot(DT,Sub_metering_1,ylab="Energy sub metering",type="n")
        lines(DT,Sub_metering_1,col="black")
        lines(DT,Sub_metering_2,col="red")
        lines(DT,Sub_metering_3,col="blue")
        legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        plot(DT,Voltage,type="l",ylab="Voltage",xlab="datetime")
        plot(DT,Global_reactive_power,type="l",ylab="Global_reactive_power",xlab="datetime")
    })
}

exportPlot <- function(){
    png("./plot4.png",width=480,height=480,units="px",bg="transparent")
    par(mfcol=c(2,2))
    with(kcz_crs04_prj1_temp_dt, {
        plot(DT,Global_active_power,type="l",ylab="Global Active Power (kilowatts)")
        plot(DT,Sub_metering_1,ylab="Energy sub metering",type="n")
        lines(DT,Sub_metering_1,col="black")
        lines(DT,Sub_metering_2,col="red")
        lines(DT,Sub_metering_3,col="blue")
        legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        plot(DT,Voltage,type="l",ylab="Voltage",xlab="datetime")
        plot(DT,Global_reactive_power,type="l",ylab="Global_reactive_power",xlab="datetime")
    })
    dev.off()
}

## Test system for specifically named temp file (to eliminate having to 
##  re-process data)
if(!exists("kcz_crs04_prj1_temp_dt")) {
    ## Verify the existence of the file in the working directory before attempting to process file
    if(file.access("household_power_consumption.txt",mode=0)<0){
        print("The file <household_power_consumption.txt> is not present in your working directory")
        print("Please verify your working directory contains the correct file before proceeding")
        print("If you do not have a copy of the file, one can be obtained from the following URL")
        print(" https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip ")
    }
    else{ 
        kcz_crs04_prj1_temp_dt<- loadProjectData()
    }
}