#Assignemtn 1 
mydata <-read.csv("household_power_consumption.txt", sep=";", header=T)

md<-as.Date(strptime(mydata$Date,format="%d/%m/%Y"))  
mt<-as.Date(strptime(mydata$Time,format="%H:%M:%S"))
s_date<-as.Date("2007-02-01")
e_date<-as.Date("2007-02-02")
mytable<-subset(mydata,md==s_date|md==e_date)

mytable$Global_active_power<-as.numeric(mytable$Global_active_power)
mytable$Global_reactive_power<-as.numeric(mytable$Global_reactive_power)
mytable$Voltage<-as.numeric(mytable$Voltage)
mytable$Sub_metering_1<-as.numeric(mytable$Sub_metering_1)
mytable$Sub_metering_2<-as.numeric(mytable$Sub_metering_2)
mytable$Sub_metering_3<-as.numeric(mytable$Sub_metering_3)

DateTime<-paste(mytable$Date, mytable$Time)
my_dt = strptime(DateTime,format = "%d/%m/%Y %H:%M:%S")


#functions
p1 <-function(){
        with (mytable,hist(Global_active_power, main="Global active power", col="red", xlab="Global Active Power (kilowatts)"))
                           
}

p2 <-function(){
        with(mytable, plot(my_dt,Global_active_power,type="l", ylab="Global Active Power (kilowatts)",xlab=""))
}



p3 <-function(){
        with(mytable, plot(my_dt,Sub_metering_1,type="l", ylab="Energy sub metering",xlab=""))
        lines(my_dt, mytable$Sub_metering_2, type="l", col="red")
        lines(my_dt, mytable$Sub_metering_3, type="l", col="blue")
        legend("topright",  c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black", "red", "blue"), lty=1, bty="n")
   
}

p4 <- function(){
        with(mytable,plot(my_dt,Voltage, type="l", xlab="daytime", ylab="Voltage"))
}

p5 <- function(){
        with(mytable,plot(my_dt,Global_reactive_power, type="l", xlab="daytime", ylab="Global_reactive_power"))
}


p6 <-function(){
       par(mfcol=c(2,2)) 
        p2()
        p3()
        p4()
        p5()
}