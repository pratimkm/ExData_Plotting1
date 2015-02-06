plotGraph4 <- function(){
  
  # for fread which is faster than read.x functions
  library("data.table")
  
  
  #if file does not exist download zip from URL provided and unzip and store the csv file
  if(!file.exists("household_power_consumption.txt")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip")
    unzip("household_power_consumption.zip",overwrite = TRUE)
  }
  
  # if already created we are recreating
  if(file.exists("plot4.png")){
    file.remove("plot4.png")
  }
  
  # open for writing
  png('plot4.png')
  # read into DT variable
  meter_readings <- read.table("household_power_consumption.txt",sep = ";",header = T,na.strings = "?",stringsAsFactors = T,colClasses = "character")
  
  # find subset of data for two day time period in feb 2007
  DT <- meter_readings[grep("^1/2/2007$|^2/2/2007$",meter_readings$Date),]
  
  # many meterings have ?,remove them
  DT <- na.omit(DT)
  
  # now set a 2x2 layout
  par(mfcol=c(2,2))
  
  # redefine columns as numeric 
  DT$Global_active_power <- as.numeric(DT$Global_active_power)
  DT$Sub_metering_1 <- as.numeric(DT$Sub_metering_1)
  DT$Sub_metering_2 <- as.numeric(DT$Sub_metering_2)
  DT$Sub_metering_3 <- as.numeric(DT$Sub_metering_3)
  DT$Voltage <- as.numeric(DT$Voltage)
  DT$Global_reactive_power <- as.numeric(DT$Global_reactive_power)
  
  #create new column combining date and time for continuity in the graph
  DT$Datetime <- strptime(paste(DT$Date, DT$Time), "%d/%m/%Y %H:%M:%S")
  
  ##make plot in top left
  plot(DT$Datetime,format ="%a", DT$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
  
  ##make plot in bottom left
  # now plot against date time,label X axis with weekdays
  plot(DT$Datetime,format ="%a",DT$Sub_metering_1, type="l",ylab="Energy sub metering",xlab="")
  # provide red color for sub metering 2 curve
  lines(DT$Datetime,DT$Sub_metering_2, type="l", col="red")
  # provide blue color for sub metering 3 curve
  lines(DT$Datetime,DT$Sub_metering_3, type="l", col="blue")
  

  ##make plot in top right
  plot(DT$Datetime,format ="%a", DT$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
  
  
  ##make plot in bottom right
  plot(DT$Datetime,format ="%a", DT$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global Reactive Power")
  
  
  dev.copy(png,file="plot4.png")
  dev.off
  
}