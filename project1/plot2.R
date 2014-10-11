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
# convert Global_Active_Power into number
myd3$Global_active_power <- as.numeric(as.character(myd3$Global_active_power))
myd3$datetime <- strptime(myd3$datetime, "%d/%m/%Y %H:%M:%S")

# Single-plot 1 X 1
par(mfrow=c(1,1))

# plot the datetime correlated with Global_active_power
with(myd3,plot(datetime,Global_active_power, type="l",ylab="Global Active Power (kilowatts)",xlab=""))

dev.copy(png,file="plot2.png")
dev.off()

