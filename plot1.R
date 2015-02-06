plotHist <- function(){
  
  # for fread which is faster than read.x functions
  library("data.table")
  
  #set locale for weekdays in en
  Sys.setlocale("LC_ALL", "en_US")  
  
  #if file does not exist download zip from URL provided and unzip and store the csv file
  if(!file.exists("household_power_consumption.txt")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip")
    unzip("household_power_consumption.zip",overwrite = TRUE)
  }
  
  # if already created we are recreating
  if(file.exists("plot1.png")){
    file.remove("plot1.png")
  }
  
 # open png file for writing
  png('plot1.png')
 
  # read into DT variable
 meter_readings <- fread("household_power_consumption.txt",sep = ";",header = T,na.strings = "?",stringsAsFactors = T,colClasses = "character")
  
  # find subset of data for two day time period
  DT <- meter_readings[grep("^1/2/2007$|^2/2/2007$",meter_readings$Date),]
  
  # redefine column as numeric 
  DT$Global_active_power <- as.numeric(DT$Global_active_power)
  
  # frequency does not need scaling, plot 
   hist(DT$Global_active_power,col="red",main = "Global Active Power",xlab="Global Active Power (kilowatts)")
  
  dev.copy(png,file="plot1.png")
  dev.off
  
}