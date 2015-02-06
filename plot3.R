plotGraph3 <- function(){
  
  # for fread which is faster than read.x functions
  library("data.table")

  
  #if file does not exist download zip from URL provided and unzip and store the csv file
  if(!file.exists("household_power_consumption.txt")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip")
    unzip("household_power_consumption.zip",overwrite = TRUE)
  }
  
  # if already created we are recreating
  if(file.exists("plot3.png")){
    file.remove("plot3.png")
  }
  
  # open for writing
  png('plot3.png')
  # read into DT variable
  meter_readings <- read.table("household_power_consumption.txt",sep = ";",header = T,na.strings = "?",stringsAsFactors = T,colClasses = "character")
  
  # find subset of data for two day time period in feb 2007
  DT <- meter_readings[grep("^1/2/2007$|^2/2/2007$",meter_readings$Date),]
 
  # many meterings have ?,remove them
  DT <- na.omit(DT)
  
  # redefine columns as numeric 
  DT$Global_active_power <- as.numeric(DT$Global_active_power)
  DT$Sub_metering_1 <- as.numeric(DT$Sub_metering_1)
  
  #create new column combining date and time for continuity in the graph
  DT$Datetime <- strptime(paste(DT$Date, DT$Time), "%d/%m/%Y %H:%M:%S")
  
  # now plot against date time,label X axis with weekdays
  plot(DT$Datetime,format ="%a",DT$Sub_metering_1, type="l",ylab="Energy sub metering",xlab="")
  # provide red color for sub metering 2 curve
  lines(DT$Datetime,DT$Sub_metering_2, type="l", col="red")
 # provide blue color for sub metering 3 curve
  lines(DT$Datetime,DT$Sub_metering_3, type="l", col="blue")
  #provide legends to the top right corner
  legend("topright", legend=names(DT[7:9]), lty=1, col=c("black","red","blue"))
  
 dev.copy(png,file="plot3.png")
  dev.off
  
}