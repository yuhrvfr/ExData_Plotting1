# reset parameter in current session
resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

par(resetPar())


# Get the data from external file
mydata2<-read.table( "household_power_consumption.txt",sep=";",header=TRUE)

# option step to verify 2,075,259 rows as written in the problem
nrow(mydata2)

# Get the data from mydata2 only for the date ranges 01/02/2007 to 02/02/2007
myd3 <- mydata2[(mydata2$Date=="1/2/2007")|(mydata2$Date=="2/2/2007"),]

# Create a new column in myd3 concatenated between Date and Time
myd3$datetime <- paste(myd3$Date,myd3$Time)

# convert datetime into date format
myd3$datetime <- strptime(myd3$datetime, "%d/%m/%Y %H:%M:%S")

# Create a function f3 for the third plot 
f3 <- function(){
# convert Sub_metering_1 into number
# convert Sub_metering_2 into number
# convert Sub_metering_3 into number
# convert Date into character
# convert Time into character
myd3$Sub_metering_1 <- as.numeric(as.character(myd3$Sub_metering_1))
myd3$Sub_metering_2 <- as.numeric(as.character(myd3$Sub_metering_2))
myd3$Sub_metering_3 <- as.numeric(as.character(myd3$Sub_metering_3))
myd3$Date <- as.character(myd3$Date)
myd3$Time <- as.character(myd3$Time)

library(reshape2)

# restrict data content to atomic data tyep for reshape2
myd4 <- myd3[,c("Date","Time","Sub_metering_1","Sub_metering_2","Sub_metering_3")]

# Reshape data with id(Date,Time) and variable(Sub_metering_X)/value(value of variable)
mydmelt <- melt(myd4, id=c("Date","Time"), measure.vars=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

# convert Date time into variable of type date time
mydmelt$datetime <- paste(mydmelt$Date,mydmelt$Time)
mydmelt$datetime <- strptime(mydmelt$datetime, "%d/%m/%Y %H:%M:%S")

# Plot empty window
plot(mydmelt$datetime, mydmelt$value, type="n",ylab="Energy sub metering",xlab="")

# points the sub metering 1 in black over the 2 days
points(mydmelt$datetime[mydmelt$variable=="Sub_metering_1"],mydmelt$value[mydmelt$variable=="Sub_metering_1"],col="black",type="l")

# points the sub metering 2 in red over the 2 days
points(mydmelt$datetime[mydmelt$variable=="Sub_metering_2"],mydmelt$value[mydmelt$variable=="Sub_metering_2"],col="red",type="l")

# points the sub metering 3 in blue over the 2 days
points(mydmelt$datetime[mydmelt$variable=="Sub_metering_3"],mydmelt$value[mydmelt$variable=="Sub_metering_3"],col="blue",type="l")

#legend("topright",legend="Sub_metering_1", type="l",col="black")

legend( x="topright", 
        legend=c("Sub_metering_1  ","Sub_metering_2  ","Sub_metering_3  "),
        col=c("black","red","blue"), lwd=1, lty=c(1,1,1), 
        pch=c(NA,NA,NA),
        bty="n" )
}


# Convert the data type for Global_active_power, Voltage and Global_reactive_power
myd3$Voltage <- as.numeric(as.character(myd3$Voltage))
myd3$Global_reactive_power <- as.numeric(as.character(myd3$Global_reactive_power))
myd3$Global_active_power <- as.numeric(as.character(myd3$Global_active_power))


# Multi-plot 2 X 2
par(mfrow=c(2,2))

# Control margin
par("mar"=c(4,4,4,1))


# First Plot
with(myd3,plot(datetime,Global_active_power, type="l",ylab="Global Active Power",xlab=""))

#Second plot
with(myd3,plot(datetime,Voltage, type="l",ylab="Voltage",xlab="datetime"))

# Third plot refer function above
f <- f3()

# Fourth plot
with(myd3,plot(datetime,Global_reactive_power, type="l",ylab="Global_reactive_power",xlab="datetime"))

# Copy output to png file
dev.copy(png,file="plot4.png")
dev.off()
