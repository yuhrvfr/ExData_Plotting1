# Get the data from external file
mydata2<-read.table( "household_power_consumption.txt",sep=";",header=TRUE)

# option step to verify 2,075,259 rows as written in the problem
nrow(mydata2)

# Get the data from mydata2 only for the date ranges 01/02/2007 to 02/02/2007
myd3 <- mydata2[(mydata2$Date=="1/2/2007")|(mydata2$Date=="2/2/2007"),]

# reset parameter in current session
resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

# Single-plot 1 X 1
# par(mfrow=c(1,1))

#Control Margin
# par("mar"=c(4,4,4,1))

par(resetPar())

# Draw the histogram
hist(as.numeric(as.character(myd3$Global_active_power)),col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")

# Copy the output to plot1.png file
dev.copy(png,file="plot1.png")
dev.off()
