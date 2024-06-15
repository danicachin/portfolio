#Read in the File
# setwd('/Users/danica/OneDrive/Desktop/5140')

#load turo data
df<-readRDS(file.choose())


library(car)
library(moments)

#Filtering dataframe to New Jersey data only
nj.df<-df[df$car.state == "nj",]

print(sum(is.na(nj.df$car.trip.price)))
nj.df<-na.omit(nj.df)

#Loading in Library to compute Skewness
library("e1071")

#User-Defined Function to compute descriptive statistics and store in dataframe
compute.stats <- function(x,var){
  mean<- mean(x, na.rm = TRUE)
  min <-min(x, na.rm = TRUE)
  first_qt <- quantile(x, probs =0.25, na.rm = TRUE)
  second_qt <- quantile(x, probs =0.5, na.rm = TRUE)
  thirdqt <- quantile(x, probs =0.75, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  skewness <- skewness(x, type = 2, na.rm = TRUE)
  iqr <- IQR(x,na.rm = TRUE)
  
  stats.df <- data.frame("Variable" = var, 
                         'Min' = min, 
                         'Max'= max,
                         'mean'=mean,
                         'First quartile'= first_qt,
                         'Second quartile' = second_qt,
                         'Third quartile'= thirdqt,
                         'Standard deviation'= sd,
                         'Skewness'= skewness,
                         'Inter Quartile Range'= iqr)
  rownames(stats.df) <- NULL
  
  return(stats.df)
}




#extracting quantitative variables only from new jersey dataframe
quant_df<-nj.df[ , c("car.deliver.airport.num", 
                   "car.deliver.hotel.num",
                   "car.deliver.train.station.num",
                   "car.displayed.turo.review.num",
                   "car.displayed.turo.review.num.past.12m",
                   "car.displayed.turo.review.num.past.18m", 
                   "car.displayed.turo.review.num.past.6m",
                   "car.displayed.user.review.num",
                   "car.displayed.user.review.num.past.12m", 
                   "car.displayed.user.review.num.past.18m",
                   "car.displayed.user.review.num.past.6m",
                   "car.extra.mile.fee", 
                   "car.extra.num",
                   "car.faq.num",
                   "car.miles.included",
                   "car.photo.num",
                   "car.self.pickup.avg.price",
                   "car.trip.price",
                   "host.car.num",
                   "host.tenure.in.weeks")]   


#applying function to each column
df1<-compute.stats(quant_df$car.deliver.airport.num,"car.deliver.airport.num")
df2<-compute.stats(quant_df$car.deliver.hotel.num,"car.deliver.hotel.num")
df3<-compute.stats(quant_df$car.deliver.train.station.num,"car.deliver.train.station.num")
df4<-compute.stats(quant_df$car.displayed.turo.review.num, "car.displayed.turo.review.num")
df5<-compute.stats(quant_df$car.displayed.turo.review.num.past.12m, "car.displayed.turo.review.num.past.12m")
df6<-compute.stats(quant_df$car.displayed.turo.review.num.past.18m, "car.displayed.turo.review.num.past.18m")
df7<-compute.stats(quant_df$car.displayed.turo.review.num.past.6m, "car.displayed.turo.review.num.past.6m")
df8<-compute.stats(quant_df$car.displayed.user.review.num, "car.displayed.user.review.num")
df9<-compute.stats(quant_df$host.car.num, "host.car.num")
df11<-compute.stats(quant_df$car.displayed.user.review.num.past.12m, "car.displayed.user.review.num.past.12m")
df12<-compute.stats(quant_df$car.displayed.user.review.num.past.18m, "car.displayed.user.review.num.past.18m")
df13<-compute.stats(quant_df$car.displayed.user.review.num.past.6m, "car.displayed.user.review.num.past.6m")
df14<-compute.stats(quant_df$car.extra.mile.fee, "car.extra.mile.fee")
df15<-compute.stats(quant_df$car.extra.num, "car.extra.num")
df16<-compute.stats(quant_df$car.faq.num, "car.faq.num")
df17<-compute.stats(quant_df$car.miles.included, "car.miles.included")
df18<-compute.stats(quant_df$car.self.pickup.avg.price, "car.miles.included")
df19<-compute.stats(quant_df$car.photo.num, "car.photo.num")
df20<-compute.stats(quant_df$car.self.pickup.avg.price, "car.self.pickup.avg.price")
df21<-compute.stats(quant_df$car.trip.price, "car.trip.price")
df22<-compute.stats(quant_df$host.tenure.in.weeks, "host.tenure.in.weeks")

#look up 3 dimesional arrays


#appending each dataframe
stat.summary<-rbind(df1, df2)      
stat.summary<-rbind(stat.summary,df4) 
stat.summary<-rbind(stat.summary,df5) 
stat.summary<-rbind(stat.summary,df6) 
stat.summary<-rbind(stat.summary,df7)
stat.summary<-rbind(stat.summary,df8) 
stat.summary<-rbind(stat.summary,df9) 
stat.summary<-rbind(stat.summary,df11) 
stat.summary<-rbind(stat.summary,df12) 
stat.summary<-rbind(stat.summary,df13) 
stat.summary<-rbind(stat.summary,df14)
stat.summary<-rbind(stat.summary,df15)
stat.summary<-rbind(stat.summary,df16) 
stat.summary<-rbind(stat.summary,df17) 
stat.summary<-rbind(stat.summary,df18) 
stat.summary<-rbind(stat.summary,df19)
stat.summary<-rbind(stat.summary,df20) 
stat.summary<-rbind(stat.summary,df21) 
stat.summary<-rbind(stat.summary,df22)




#reseting indices
rownames(stat.summary) <- NULL

#calling the final dataframe
stat.summary


#comparing values in dataframe to summary function
summary(quant_df)

#####################################################################
#####-- Car City frequency/Relative Freq/Percent Freq/Barchart--#####
#####################################################################

car.city.freq<-data.frame(table(nj.df$car.city))
names(car.city.freq)[2]<-"Freq"
names(car.city.freq)[1]<-"car.city"

car.city.freq<-car.city.freq[car.city.freq$Freq != 0,]

# Relative Frequency

car.city.rel.freq <- data.frame(table(nj.df$car.city)/sum(car.city.freq$Freq))
names(car.city.rel.freq )[2]<-"Rel.Freq"
names(car.city.rel.freq )[1]<-"car.city"
car.city.rel.freq<-car.city.rel.freq[car.city.rel.freq$Rel.Freq != 0,]
car.city.rel.freq

car.city.freq$Rel.Freq<-car.city.rel.freq$Rel.Freq
car.city.freq

#Percent Frequency

car.city.percent.freq <- data.frame(table(nj.df$car.city)/sum(car.city.freq$Freq)*100)
names(car.city.percent.freq )[2]<-"Percent.Freq"
names(car.city.percent.freq )[1]<-"car.city"
car.city.percent.freq

car.city.percent.freq<-car.city.percent.freq[car.city.percent.freq$Percent.Freq != 0,]
car.city.freq$Percent.Freq<-car.city.percent.freq$Percent.Freq
rownames(car.city.freq) <- NULL
car.city.freq


#Barchart

barplot(car.city.freq$Freq, names.arg = car.city.freq$car.city,
        xlab = "City", ylab = "Frequency", main = "Car City Frequency")


###################################################################################
#####-- Car.deliver.to.you.num frequency/Relative Freq/Percent Freq/Barchart--#####
###################################################################################

car.deliver.to.you.num.freq<-data.frame(table(nj.df$car.deliver.to.you.num))
names(car.deliver.to.you.num.freq)[2]<-"Freq"
names(car.deliver.to.you.num.freq)[1]<-"car.deliver.to.you.num"

car.deliver.to.you.num.freq<-car.deliver.to.you.num.freq[car.deliver.to.you.num.freq$Freq != 0,]
car.deliver.to.you.num.freq


# Relative Frequency

car.deliver.to.you.num.rel.freq <- data.frame(table(nj.df$car.deliver.to.you.num)/sum(car.deliver.to.you.num.freq$Freq))
names(car.deliver.to.you.num.rel.freq )[2]<-"Rel.Freq"
names(car.deliver.to.you.num.rel.freq )[1]<-"car.city"
car.deliver.to.you.num.rel.freq<-car.deliver.to.you.num.rel.freq[car.deliver.to.you.num.rel.freq$Rel.Freq != 0,]
car.deliver.to.you.num.rel.freq

car.deliver.to.you.num.freq$Rel.Freq<-car.deliver.to.you.num.rel.freq$Rel.Freq
car.deliver.to.you.num.freq

#Percent Frequency

car.deliver.to.you.num.percent.freq <- data.frame(table(nj.df$car.deliver.to.you.num)/sum(car.deliver.to.you.num.freq$Freq)*100)
names(car.deliver.to.you.num.percent.freq )[2]<-"Percent.Freq"
names(car.deliver.to.you.num.percent.freq )[1]<-"car.deliver.to.you.num"
car.deliver.to.you.num.percent.freq

car.deliver.to.you.num.percent.freq<-car.deliver.to.you.num.percent.freq[car.deliver.to.you.num.percent.freq$Percent.Freq != 0,]
car.deliver.to.you.num.freq$Percent.Freq<-car.deliver.to.you.num.percent.freq$Percent.Freq
rownames(car.deliver.to.you.num.freq) <- NULL
car.deliver.to.you.num.freq


#Barchart

barplot(car.deliver.to.you.num.freq$Freq, names.arg = car.deliver.to.you.num.freq$car.city,
        xlab = "Yes or No", ylab = "Frequency", main = "Car Delivery to Customer")

######################################################################
#####-- Car.doors frequency/Relative Freq/Percent Freq/Barchart--#####
######################################################################

car.door.freq<-data.frame(table(nj.df$car.doors))
names(car.door.freq)[2]<-"Freq"
names(car.door.freq)[1]<-"car.door"

car.door.freq<-car.door.freq[car.door.freq$Freq != 0,]
car.door.freq


# Relative Frequency

car.door.rel.freq <- data.frame(table(nj.df$car.doors)/sum(car.door.freq$Freq))
names(car.door.rel.freq )[2]<-"Rel.Freq"
names(car.door.rel.freq )[1]<-"car.door"
car.door.rel.freq<-car.door.rel.freq[car.door.rel.freq$Rel.Freq != 0,]
car.door.rel.freq

car.door.freq$Rel.Freq<-car.door.rel.freq$Rel.Freq
car.door.freq

#Percent Frequency

car.door.percent.freq <- data.frame(table(nj.df$car.doors)/sum(car.door.freq$Freq)*100)
names(car.door.percent.freq )[2]<-"Percent.Freq"
names(car.door.percent.freq )[1]<-"car.door"
car.door.percent.freq

car.door.percent.freq<-car.door.percent.freq[car.door.percent.freq$Percent.Freq != 0,]
car.door.freq$Percent.Freq<-car.door.percent.freq$Percent.Freq
rownames(car.door.freq) <- NULL
car.door.freq



#Barchart

barplot(car.door.freq$Freq, names.arg = car.door.freq$car.door,
        xlab = "Frequency", ylab = "# of Car Doors", main = "Car Door Frequency", col="#69b3a2", horiz=T, las=1)



##################################################################################
#####-- Car.extra.beach.gear frequency/Relative Freq/Percent Freq/Barchart --#####
##################################################################################

car.extra.beach.gear.freq<-data.frame(table(nj.df$car.extra.beach.gear))
names(car.extra.beach.gear.freq)[2]<-"Freq"
names(car.extra.beach.gear.freq)[1]<-"car.extra.beach.gear"

car.extra.beach.gear.freq<-car.extra.beach.gear.freq[car.extra.beach.gear.freq$Freq != 0,]
car.extra.beach.gear.freq


# Relative Frequency

car.extra.beach.gear.rel.freq <- data.frame(table(nj.df$car.extra.beach.gear)/sum(car.extra.beach.gear.freq$Freq))
names(car.extra.beach.gear.rel.freq )[2]<-"Rel.Freq"
names(car.extra.beach.gear.rel.freq )[1]<-"car.extra.beach.gear"
car.extra.beach.gear.rel.freq<-car.extra.beach.gear.rel.freq[car.extra.beach.gear.rel.freq$Rel.Freq != 0,]
car.extra.beach.gear.rel.freq

car.extra.beach.gear.freq$Rel.Freq<-car.extra.beach.gear.rel.freq$Rel.Freq
car.extra.beach.gear.freq

#Percent Frequency

car.extra.beach.gear.percent.freq <- data.frame(table(nj.df$car.extra.beach.gear)/sum(car.extra.beach.gear.freq$Freq)*100)
names(car.extra.beach.gear.percent.freq )[2]<-"Percent.Freq"
names(car.extra.beach.gear.percent.freq )[1]<-"car.extra.beach.gear"
car.extra.beach.gear.percent.freq

car.extra.beach.gear.percent.freq<-car.extra.beach.gear.percent.freq[car.extra.beach.gear.percent.freq$Percent.Freq != 0,]
car.extra.beach.gear.freq$Percent.Freq<-car.extra.beach.gear.percent.freq$Percent.Freq
rownames(car.extra.beach.gear.freq) <- NULL
car.extra.beach.gear.freq


#Barchart

barplot(car.extra.beach.gear.freq$Freq, names.arg = car.extra.beach.gear.freq$car.extra.beach.gear,
        xlab = "Extra Beach Gear Provided T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")

#########################################################################################
#####-- Car.extra.child.safety.seat frequency/Relative Freq/Percent Freq/Barchart --#####
#########################################################################################


car.extra.beach.gear.freq<-data.frame(table(nj.df$car.extra.beach.gear))
names(car.extra.beach.gear.freq)[2]<-"Freq"
names(car.extra.beach.gear.freq)[1]<-"car.extra.beach.gear"

car.extra.beach.gear.freq<-car.extra.beach.gear.freq[car.extra.beach.gear.freq$Freq != 0,]
car.extra.beach.gear.freq


# Relative Frequency

car.extra.beach.gear.rel.freq <- data.frame(table(nj.df$car.extra.beach.gear)/sum(car.extra.beach.gear.freq$Freq))
names(car.extra.beach.gear.rel.freq )[2]<-"Rel.Freq"
names(car.extra.beach.gear.rel.freq )[1]<-"car.extra.beach.gear"
car.extra.beach.gear.rel.freq<-car.extra.beach.gear.rel.freq[car.extra.beach.gear.rel.freq$Rel.Freq != 0,]
car.extra.beach.gear.rel.freq

car.extra.beach.gear.freq$Rel.Freq<-car.extra.beach.gear.rel.freq$Rel.Freq
car.extra.beach.gear.freq

#Percent Frequency

car.extra.beach.gear.percent.freq <- data.frame(table(nj.df$car.extra.beach.gear)/sum(car.extra.beach.gear.freq$Freq)*100)
names(car.extra.beach.gear.percent.freq )[2]<-"Percent.Freq"
names(car.extra.beach.gear.percent.freq )[1]<-"car.extra.beach.gear"
car.extra.beach.gear.percent.freq

car.extra.beach.gear.percent.freq<-car.extra.beach.gear.percent.freq[car.extra.beach.gear.percent.freq$Percent.Freq != 0,]
car.extra.beach.gear.freq$Percent.Freq<-car.extra.beach.gear.percent.freq$Percent.Freq
rownames(car.extra.beach.gear.freq) <- NULL
car.extra.beach.gear.freq


#Barchart

barplot(car.extra.beach.gear.freq$Freq, names.arg = car.extra.beach.gear.freq$car.extra.beach.gear,
        xlab = "Extra Beach Gear Provided T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")

##############################################################################
#####-- Car.extra.cooler frequency/Relative Freq/Percent Freq/Barchart --#####
##############################################################################


car.extra.cooler.freq<-data.frame(table(nj.df$car.extra.cooler))
names(car.extra.cooler.freq)[2]<-"Freq"
names(car.extra.cooler.freq)[1]<-"car.extra.cooler"

car.extra.cooler.freq<-car.extra.cooler.freq[car.extra.cooler.freq$Freq != 0,]
car.extra.cooler.freq


# Relative Frequency

car.extra.cooler.rel.freq <- data.frame(table(nj.df$car.extra.cooler)/sum(car.extra.cooler.freq$Freq))
names(car.extra.cooler.rel.freq )[2]<-"Rel.Freq"
names(car.extra.cooler.rel.freq )[1]<-"car.extra.cooler"
car.extra.cooler.rel.freq<-car.extra.cooler.rel.freq[car.extra.cooler.rel.freq$Rel.Freq != 0,]
car.extra.cooler.rel.freq

car.extra.cooler.freq$Rel.Freq<-car.extra.cooler.rel.freq$Rel.Freq
car.extra.cooler.freq

#Percent Frequency

car.extra.cooler.percent.freq <- data.frame(table(nj.df$car.extra.cooler)/sum(car.extra.cooler.freq$Freq)*100)
names(car.extra.cooler.percent.freq )[2]<-"Percent.Freq"
names(car.extra.cooler.percent.freq )[1]<-"car.extra.cooler"
car.extra.cooler.percent.freq

car.extra.cooler.percent.freq<-car.extra.cooler.percent.freq[car.extra.cooler.percent.freq$Percent.Freq != 0,]
car.extra.cooler.freq$Percent.Freq<-car.extra.cooler.percent.freq$Percent.Freq
rownames(car.extra.cooler.freq) <- NULL
car.extra.cooler.freq


#Barchart

barplot(car.extra.cooler.freq$Freq, names.arg = car.extra.cooler.freq$car.extra.cooler,
        xlab = "Extra cooler Provided T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")



####################################################################################
#####-- Car.extra.one.way.trip frequency/Relative Freq/Percent Freq/Barchart --#####
####################################################################################


car.extra.one.way.trip.freq<-data.frame(table(nj.df$car.extra.one.way.trip))
names(car.extra.one.way.trip.freq)[2]<-"Freq"
names(car.extra.one.way.trip.freq)[1]<-"car.extra.one.way.trip"

car.extra.one.way.trip.freq<-car.extra.one.way.trip.freq[car.extra.one.way.trip.freq$Freq != 0,]
car.extra.one.way.trip.freq

# Relative Frequency

car.extra.one.way.trip.rel.freq <- data.frame(table(nj.df$car.extra.one.way.trip)/sum(car.extra.one.way.trip.freq$Freq))
names(car.extra.one.way.trip.rel.freq )[2]<-"Rel.Freq"
names(car.extra.one.way.trip.rel.freq )[1]<-"car.extra.one.way.trip"
car.extra.one.way.trip.rel.freq<-car.extra.one.way.trip.rel.freq[car.extra.one.way.trip.rel.freq$Rel.Freq != 0,]
car.extra.one.way.trip.rel.freq

car.extra.one.way.trip.freq$Rel.Freq<-car.extra.one.way.trip.rel.freq$Rel.Freq
car.extra.one.way.trip.freq

#Percent Frequency

car.extra.one.way.trip.percent.freq <- data.frame(table(nj.df$car.extra.one.way.trip)/sum(car.extra.one.way.trip.freq$Freq)*100)
names(car.extra.one.way.trip.percent.freq )[2]<-"Percent.Freq"
names(car.extra.one.way.trip.percent.freq )[1]<-"car.extra.cooler"
car.extra.one.way.trip.percent.freq

car.extra.one.way.trip.percent.freq<-car.extra.one.way.trip.percent.freq[car.extra.one.way.trip.percent.freq$Percent.Freq != 0,]
car.extra.one.way.trip.freq$Percent.Freq<-car.extra.one.way.trip.percent.freq$Percent.Freq
rownames(car.extra.one.way.trip.freq) <- NULL
car.extra.one.way.trip.freq


#Barchart

barplot(car.extra.one.way.trip.freq$Freq, names.arg = car.extra.one.way.trip.freq$car.extra.one.way.trip,
        xlab = "Extra one way trip T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")



###############################################################################
#####-- Car.extra.pet.fee frequency/Relative Freq/Percent Freq/Barchart --#####
###############################################################################


car.extra.pet.fee.freq<-data.frame(table(nj.df$car.extra.pet.fee))
names(car.extra.pet.fee.freq)[2]<-"Freq"
names(car.extra.pet.fee.freq)[1]<-"car.extra.pet.fee"

car.extra.pet.fee.freq<-car.extra.pet.fee.freq[car.extra.pet.fee.freq$Freq != 0,]
car.extra.pet.fee.freq

# Relative Frequency

car.extra.pet.fee.rel.freq <- data.frame(table(nj.df$car.extra.pet.fee)/sum(car.extra.pet.fee.freq$Freq))
names(car.extra.pet.fee.rel.freq )[2]<-"Rel.Freq"
names(car.extra.pet.fee.rel.freq )[1]<-"car.extra.pet.fee"
car.extra.pet.fee.rel.freq<-car.extra.pet.fee.rel.freq[car.extra.pet.fee.rel.freq$Rel.Freq != 0,]
car.extra.pet.fee.rel.freq

car.extra.pet.fee.freq$Rel.Freq<-car.extra.pet.fee.rel.freq$Rel.Freq
car.extra.pet.fee.freq

#Percent Frequency

car.extra.pet.fee.percent.freq <- data.frame(table(nj.df$car.extra.pet.fee)/sum(car.extra.pet.fee.freq$Freq)*100)
names(car.extra.pet.fee.percent.freq )[2]<-"Percent.Freq"
names(car.extra.pet.fee.percent.freq )[1]<-"car.extra.pet.fee"
car.extra.pet.fee.percent.freq

car.extra.pet.fee.percent.freq<-car.extra.pet.fee.percent.freq[car.extra.pet.fee.percent.freq$Percent.Freq != 0,]
car.extra.pet.fee.freq$Percent.Freq<-car.extra.pet.fee.percent.freq$Percent.Freq
rownames(car.extra.pet.fee.freq) <- NULL
car.extra.pet.fee.freq


#Barchart

barplot(car.extra.pet.fee.freq$Freq, names.arg = car.extra.pet.fee.freq$car.extra.pet.fee,
        xlab = "Extra pet fee T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")


###################################################################################
#####-- Car.extra.phone.mount frequency/Relative Freq/Percent Freq/Barchart --#####
###################################################################################

car.extra.phone.mount.freq<-data.frame(table(nj.df$car.extra.phone.mount))
names(car.extra.phone.mount.freq)[2]<-"Freq"
names(car.extra.phone.mount.freq)[1]<-"car.extra.phone.mount"

car.extra.phone.mount.freq<-car.extra.phone.mount.freq[car.extra.phone.mount.freq$Freq != 0,]
car.extra.phone.mount.freq

# Relative Frequency

car.extra.phone.mount.rel.freq <- data.frame(table(nj.df$car.extra.phone.mount)/sum(car.extra.phone.mount.freq$Freq))
names(car.extra.phone.mount.rel.freq )[2]<-"Rel.Freq"
names(car.extra.phone.mount.rel.freq )[1]<-"car.extra.phone.mount"
car.extra.phone.mount.rel.freq<-car.extra.phone.mount.rel.freq[car.extra.phone.mount.rel.freq$Rel.Freq != 0,]
car.extra.phone.mount.rel.freq

car.extra.phone.mount.freq$Rel.Freq<-car.extra.phone.mount.rel.freq$Rel.Freq
car.extra.phone.mount.freq

#Percent Frequency

car.extra.phone.mount.percent.freq <- data.frame(table(nj.df$car.extra.phone.mount)/sum(car.extra.phone.mount.freq$Freq)*100)
names(car.extra.phone.mount.percent.freq )[2]<-"Percent.Freq"
names(car.extra.phone.mount.percent.freq )[1]<-"car.extra.phone.mount"
car.extra.phone.mount.percent.freq

car.extra.phone.mount.percent.freq<-car.extra.phone.mount.percent.freq[car.extra.phone.mount.percent.freq$Percent.Freq != 0,]
car.extra.phone.mount.freq$Percent.Freq<-car.extra.phone.mount.percent.freq$Percent.Freq
rownames(car.extra.phone.mount.freq) <- NULL
car.extra.phone.mount.freq


#Barchart

barplot(car.extra.phone.mount.freq$Freq, names.arg = car.extra.phone.mount.freq$car.extra.phone.mount,
        xlab = "Extra phone mount T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")


####################################################################################
#####-- Car.extra.portable.gps frequency/Relative Freq/Percent Freq/Barchart --#####
####################################################################################

car.extra.portable.gps.freq<-data.frame(table(nj.df$car.extra.portable.gps))
names(car.extra.portable.gps.freq)[2]<-"Freq"
names(car.extra.portable.gps.freq)[1]<-"car.extra.portable.gps"

car.extra.portable.gps.freq<-car.extra.portable.gps.freq[car.extra.portable.gps.freq$Freq != 0,]
car.extra.portable.gps.freq

# Relative Frequency

car.extra.portable.gps.rel.freq <- data.frame(table(nj.df$car.extra.portable.gps)/sum(car.extra.portable.gps.freq$Freq))
names(car.extra.portable.gps.rel.freq )[2]<-"Rel.Freq"
names(car.extra.portable.gps.rel.freq )[1]<-"car.extra.portable.gps"
car.extra.portable.gps.rel.freq<-car.extra.portable.gps.rel.freq[car.extra.portable.gps.rel.freq$Rel.Freq != 0,]
car.extra.portable.gps.rel.freq

car.extra.portable.gps.freq$Rel.Freq<-car.extra.portable.gps.rel.freq$Rel.Freq
car.extra.portable.gps.freq


#Percent Frequency
car.extra.portable.gps.percent.freq <- data.frame(table(nj.df$car.extra.portable.gps)/sum(car.extra.portable.gps.freq$Freq)*100)
names(car.extra.portable.gps.percent.freq )[2]<-"Percent.Freq"
names(car.extra.portable.gps.percent.freq )[1]<-"car.extra.portable.gps"
car.extra.portable.gps.percent.freq

car.extra.portable.gps.percent.freq<-car.extra.portable.gps.percent.freq[car.extra.portable.gps.percent.freq$Percent.Freq != 0,]
car.extra.portable.gps.freq$Percent.Freq<-car.extra.portable.gps.percent.freq$Percent.Freq
rownames(car.extra.portable.gps.freq) <- NULL
car.extra.portable.gps.freq


#Barchart

barplot(car.extra.portable.gps.freq$Freq, names.arg = car.extra.portable.gps.freq$car.extra.portable.gps,
        xlab = "Extra portable GPS T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")

##########################################################################################
#####-- Car.extra.post.trip.cleaning frequency/Relative Freq/Percent Freq/Barchart --#####
##########################################################################################

car.extra.post.trip.cleaning.freq<-data.frame(table(nj.df$car.extra.post.trip.cleaning))
names(car.extra.post.trip.cleaning.freq)[2]<-"Freq"
names(car.extra.post.trip.cleaning.freq)[1]<-"car.extra.post.trip.cleaning"

car.extra.post.trip.cleaning.freq<-car.extra.post.trip.cleaning.freq[car.extra.post.trip.cleaning.freq$Freq != 0,]
car.extra.post.trip.cleaning.freq

# Relative Frequency

car.extra.post.trip.cleaning.rel.freq <- data.frame(table(nj.df$car.extra.post.trip.cleaning)/sum(car.extra.post.trip.cleaning.freq$Freq))
names(car.extra.post.trip.cleaning.rel.freq )[2]<-"Rel.Freq"
names(car.extra.post.trip.cleaning.rel.freq )[1]<-"car.extra.post.trip.cleaning"
car.extra.post.trip.cleaning.rel.freq<-car.extra.post.trip.cleaning.rel.freq[car.extra.post.trip.cleaning.rel.freq$Rel.Freq != 0,]
car.extra.post.trip.cleaning.rel.freq

car.extra.post.trip.cleaning.freq$Rel.Freq<-car.extra.post.trip.cleaning.rel.freq$Rel.Freq
car.extra.post.trip.cleaning.freq


#Percent Frequency
car.extra.post.trip.cleaning.percent.freq <- data.frame(table(nj.df$car.extra.post.trip.cleaning)/sum(car.extra.post.trip.cleaning.freq$Freq)*100)
names(car.extra.post.trip.cleaning.percent.freq )[2]<-"Percent.Freq"
names(car.extra.post.trip.cleaning.percent.freq )[1]<-"car.extra.post.trip.cleaning"
car.extra.post.trip.cleaning.percent.freq

car.extra.post.trip.cleaning.percent.freq<-car.extra.post.trip.cleaning.percent.freq[car.extra.post.trip.cleaning.percent.freq$Percent.Freq != 0,]
car.extra.post.trip.cleaning.freq$Percent.Freq<-car.extra.post.trip.cleaning.percent.freq$Percent.Freq
rownames(car.extra.post.trip.cleaning.freq) <- NULL
car.extra.post.trip.cleaning.freq


#Barchart

barplot(car.extra.post.trip.cleaning.freq$Freq, names.arg = car.extra.post.trip.cleaning.freq$car.extra.post.trip.cleaning,
        xlab = "Extra Post Trip Cleaning T/F", ylab = "Frequency", main = "Frequency", col="#69b3a2")

#############################################################
### car.extra.prepaid.ev.recharge frequency/Relative Freq ###
#############################################################

car.extra.prepaid.ev.recharge.freq <- data.frame(table(nj.df$car.extra.prepaid.ev.recharge))
names(car.extra.prepaid.ev.recharge.freq)[2]<-"Freq"
names(car.extra.prepaid.ev.recharge.freq)[1]<-"car.extra.prepaid.ev.recharge"

car.extra.prepaid.ev.recharge.freq<-car.extra.prepaid.ev.recharge.freq[car.extra.prepaid.ev.recharge.freq$Freq != 0,]
car.extra.prepaid.ev.recharge.freq

#Relative Frequency and percentage freq
car.extra.prepaid.ev.recharge.freq$Rel.Freq <- car.extra.prepaid.ev.recharge.freq$Freq / sum(car.extra.prepaid.ev.recharge.freq$Freq)
car.extra.prepaid.ev.recharge.freq$Pct.Freq <- 100.00 * car.extra.prepaid.ev.recharge.freq$Rel.Freq
car.extra.prepaid.ev.recharge.freq

#barchart 
barplot(car.extra.prepaid.ev.recharge.freq$Freq, names.arg = car.extra.prepaid.ev.recharge.freq$car.extra.prepaid.ev.recharge,
        xlab = "Prepaid EV Recharge", ylab = "Frequency", main = "Bar Chart of Prepaid EV Recharge Availability")

########################################################
### car.extra.prepaid.refuel frequency/Relative Freq ###
########################################################


car.extra.prepaid.refuel.freq <- data.frame(table(nj.df$car.extra.prepaid.refuel))
names(car.extra.prepaid.refuel.freq)[2]<-"Freq"
names(car.extra.prepaid.refuel.freq)[1]<-"car.extra.prepaid.refuel"

car.extra.prepaid.refuel.freq<-car.extra.prepaid.refuel.freq[car.extra.prepaid.refuel.freq$Freq != 0,]
car.extra.prepaid.refuel.freq

#Relative Frequency and percentage freq
car.extra.prepaid.refuel.freq$Rel.Freq <- car.extra.prepaid.refuel.freq$Freq / sum(car.extra.prepaid.refuel.freq$Freq)
car.extra.prepaid.refuel.freq$Pct.Freq <- 100.00 * car.extra.prepaid.refuel.freq$Rel.Freq
car.extra.prepaid.refuel.freq

#barchart 
barplot(car.extra.prepaid.refuel.freq$Freq, names.arg = car.extra.prepaid.refuel.freq$car.extra.prepaid.refuel,
        xlab = "Prepaid Refuel", ylab = "Frequency", main = "Bar Chart of Prepaid Refuel Availability")

##################################################
### car.extra.stroller frequency/Relative Freq ###
##################################################

car.extra.stroller.freq <- data.frame(table(nj.df$car.extra.stroller))
names(car.extra.stroller.freq)[2]<-"Freq"
names(car.extra.stroller.freq)[1]<-"car.extra.stroller"

car.extra.stroller.freq<-car.extra.stroller.freq[car.extra.stroller.freq$Freq != 0,]
car.extra.stroller.freq

#Relative Frequency and percentage freq
car.extra.stroller.freq$Rel.Freq <- car.extra.stroller.freq$Freq / sum(car.extra.stroller.freq$Freq)
car.extra.stroller.freq$Pct.Freq <- 100.00 * car.extra.stroller.freq$Rel.Freq
car.extra.stroller.freq

#barchart 
barplot(car.extra.stroller.freq$Freq, names.arg = car.extra.stroller.freq$car.extra.stroller,
        xlab = "Stroller", ylab = "Frequency", main = "Bar Chart of Stroller Availability")

###########################################################
### car.extra.unlimited.mileage frequency/Relative Freq ###
###########################################################

car.extra.unlimited.mileage.freq <- data.frame(table(nj.df$car.extra.unlimited.mileage))
names(car.extra.unlimited.mileage.freq)[2]<-"Freq"
names(car.extra.unlimited.mileage.freq)[1]<-"car.extra.unlimited.mileage"

car.extra.unlimited.mileage.freq<-car.extra.unlimited.mileage.freq[car.extra.unlimited.mileage.freq$Freq != 0,]
car.extra.unlimited.mileage.freq

#Relative Frequency and percentage freq
car.extra.unlimited.mileage.freq$Rel.Freq <- car.extra.unlimited.mileage.freq$Freq / sum(car.extra.unlimited.mileage.freq$Freq)
car.extra.unlimited.mileage.freq$Pct.Freq <- 100.00 * car.extra.unlimited.mileage.freq$Rel.Freq
car.extra.unlimited.mileage.freq

#barchart 
barplot(car.extra.unlimited.mileage.freq$Freq, names.arg = car.extra.unlimited.mileage.freq$car.extra.unlimited.mileage,
        xlab = "Unlimited Mileage", ylab = "Frequency", main = "Bar Chart of Unlimited Mileage Availability")

################################################
### car.instant.book frequency/Relative Freq ###
################################################

car.instant.book.freq <- data.frame(table(nj.df$car.instant.book))
names(car.instant.book.freq)[2]<-"Freq"
names(car.instant.book.freq)[1]<-"car.instant.book"

car.instant.book.freq<-car.instant.book.freq[car.instant.book.freq$Freq != 0,]
car.instant.book.freq

#Relative Frequency and percentage freq
car.instant.book.freq$Rel.Freq <- car.instant.book.freq$Freq / sum(car.instant.book.freq$Freq)
car.instant.book.freq$Pct.Freq <- 100.00 * car.instant.book.freq$Rel.Freq
car.instant.book.freq

#barchart 
barplot(car.instant.book.freq$Freq, names.arg = car.instant.book.freq$car.instant.book,
        xlab = "Instant Booking", ylab = "Frequency", main = "Bar Chart of Instant Booking Availability")

#############################################
### car.insurance frequency/Relative Freq ###
#############################################

car.insurance.freq <- data.frame(table(nj.df$car.insurance))
names(car.insurance.freq)[2]<-"Freq"
names(car.insurance.freq)[1]<-"car.insurance"

car.insurance.freq<-car.insurance.freq[car.insurance.freq$Freq != 0,]
car.insurance.freq

#Relative Frequency and percentage freq
car.insurance.freq$Rel.Freq <- car.insurance.freq$Freq / sum(car.insurance.freq$Freq)
car.insurance.freq$Pct.Freq <- 100.00 * car.insurance.freq$Rel.Freq
car.insurance.freq

#barchart 
barplot(car.insurance.freq$Freq, names.arg = car.insurance.freq$car.insurance,
        xlab = "Car Insurance", ylab = "Frequency", main = "Bar Chart of Car Insurance Availability")

########################################
### car.make frequency/Relative Freq ###
########################################

car.make.freq <- data.frame(table(nj.df$car.make))
names(car.make.freq)[2]<-"Freq"
names(car.make.freq)[1]<-"car.make"

car.make.freq<-car.make.freq[car.make.freq$Freq != 0,]
car.make.freq

#Relative Frequency and percentage freq
car.make.freq$Rel.Freq <- car.make.freq$Freq / sum(car.make.freq$Freq)
car.make.freq$Pct.Freq <- 100.00 * car.make.freq$Rel.Freq
car.make.freq

#barchart 
barplot(car.make.freq$Freq, names.arg = car.make.freq$car.make,
        xlab = "Car Make", ylab = "Frequency", main = "Bar Chart of Car Make Availability")

###################################################
### car.model frequency/Relative Freq/ Barchart ###
###################################################

car.model.freq <- data.frame(table(nj.df$car.model))
names(car.model.freq)[2]<-"Freq"
names(car.model.freq)[1]<-"car.model"

car.model.freq<-car.model.freq[car.model.freq$Freq != 0,]
car.model.freq

#Relative Frequency and percentage freq
car.model.freq$Rel.Freq <- car.model.freq$Freq / sum(car.model.freq$Freq)
car.model.freq$Pct.Freq <- 100.00 *car.model.freq$Rel.Freq
car.model.freq

#barchart 
barplot(car.model.freq$Freq, names.arg = car.model.freq$car.model,
        xlab = "Car Model", ylab = "Frequency", main = "Bar Chart of Car Model Availability")

############################################################
### car.photo.verified frequency/Relative Freq/ Barchart ###
############################################################

car.photo.verified.freq <- data.frame(table(nj.df$car.photo.verified))
names(car.photo.verified.freq)[2]<-"Freq"
names(car.photo.verified.freq)[1]<-"car.photo.verified"

car.photo.verified.freq<-car.photo.verified.freq[car.photo.verified.freq$Freq != 0,]
car.photo.verified.freq

#Relative Frequency and percentage freq
car.photo.verified.freq$Rel.Freq <- car.photo.verified.freq$Freq / sum(car.photo.verified.freq$Freq)
car.photo.verified.freq$Pct.Freq <- 100.00 *car.photo.verified.freq$Rel.Freq
car.photo.verified.freq

#barchart 
barplot(car.photo.verified.freq$Freq, names.arg = car.photo.verified.freq$car.photo.verified,
        xlab = "Car Photo Verificatiion", ylab = "Frequency", main = "Bar Chart of Car Photo Verification Availability")

###################################################
### car.power frequency/Relative Freq/ Barchart ###
###################################################

car.power.freq <- data.frame(table(nj.df$car.power))
names(car.power.freq)[2]<-"Freq"
names(car.power.freq)[1]<-"car.power"

car.power.freq<-car.power.freq[car.power.freq$Freq != 0,]
car.power.freq

#Relative Frequency and percentage freq
car.power.freq$Rel.Freq <- car.power.freq$Freq / sum(car.power.freq$Freq)
car.power.freq$Pct.Freq <- 100.00 *car.power.freq$Rel.Freq
car.power.freq

#barchart 
barplot(car.power.freq$Freq, names.arg = car.power.freq$car.power,
        xlab = "Car Power", ylab = "Frequency", main = "Bar Chart of Car Power Type Availability")

#########################################################
### car.rental.type frequency/Relative Freq/ Barchart ###
#########################################################

car.rental.type.freq <- data.frame(table(nj.df$car.rental.type))
names(car.rental.type.freq)[2]<-"Freq"
names(car.rental.type.freq)[1]<-"car.rental.type"

car.rental.type.freq<-car.rental.type.freq[car.rental.type.freq$Freq != 0,]
car.rental.type.freq

#Relative Frequency and percentage freq
car.rental.type.freq$Rel.Freq <- car.rental.type.freq$Freq / sum(car.rental.type.freq$Freq)
car.rental.type.freq$Pct.Freq <- 100.00 *car.rental.type.freq$Rel.Freq
car.rental.type.freq

#barchart 
barplot(car.rental.type.freq$Freq, names.arg = car.rental.type.freq$car.rental.type,
        xlab = "Car Rental Type", ylab = "Frequency", main = "Bar Chart of Car Rental Type Availability")

#########################################################
### Car Self Pickup frequency/Relative Freq/ Barchart ###
#########################################################

car.self.pickup.num.freq<-data.frame(table(nj.df$car.self.pickup.num))
names(car.self.pickup.num.freq)[2]<-"Freq"
names(car.self.pickup.num.freq)[1]<-"car.self.pickup.num"

car.self.pickup.num.freq<-car.self.pickup.num.freq[car.self.pickup.num.freq$Freq != 0,]
car.self.pickup.num.freq

# Relative Frequency

car.self.pickup.num.rel.freq <- data.frame(table(nj.df$car.self.pickup.num)/sum(car.self.pickup.num.freq$Freq))
names(car.self.pickup.num.rel.freq )[2]<-"Rel.Freq"
names(car.self.pickup.num.rel.freq )[1]<-"car.self.pickup.num"
car.self.pickup.num.rel.freq <-car.self.pickup.num.rel.freq [car.self.pickup.num.rel.freq$Rel.Freq != 0,]
car.self.pickup.num.rel.freq

car.self.pickup.num.freq$Rel.Freq<-car.self.pickup.num.rel.freq$Rel.Freq
car.self.pickup.num.freq

#Percent Frequency

car.self.pickup.num.percent.freq <- data.frame(table(nj.df$car.self.pickup.num)/sum(car.self.pickup.num.freq$Freq)*100)
names(car.self.pickup.num.freq )[2]<-"Percent.Freq"
names(car.self.pickup.num.freq )[1]<-"car.self.pickup.num"
car.self.pickup.num.percent.freq

car.self.pickup.num.percent.freq <-car.self.pickup.num.percent.freq[car.self.pickup.num.percent.freq$Percent.Freq != 0,]
car.self.pickup.num.freq$Percent.Freq<-car.self.pickup.num.percent.freq$Percent.Freq
rownames(car.self.pickup.num.freq) <- NULL
car.self.pickup.num.freq


#Barchart

barplot(car.self.pickup.num.freq$Freq, names.arg = car.self.pickup.num.freq$car.self.pickup.num,
        xlab = "Car Self Pick Up", ylab = "Frequency", main = "Car Self Pick Up Frequency")

#################################################################################
#####-- Car State (Location) frequency/Relative Freq/Percent Freq/Barchart--#####
#################################################################################

car.state.freq<-data.frame(table(nj.df$car.state))
names(car.state.freq)[2]<-"Freq"
names(car.state.freq)[1]<-"car.state"

car.state.freq<-car.state.freq[car.state.freq$Freq != 0,]
car.state.freq


# Relative Frequency

car.state.rel.freq <- data.frame(table(nj.df$car.state)/sum(car.state.freq$Freq))
names(car.state.rel.freq )[2]<-"Rel.Freq"
names(car.state.rel.freq )[1]<-"car.state"
car.state.rel.freq<-car.state.rel.freq[car.state.rel.freq$Rel.Freq != 0,]
car.state.rel.freq

car.state.freq$Rel.Freq<-car.state.rel.freq$Rel.Freq
car.state.freq

#Percent Frequency

car.state.percent.freq <- data.frame(table(nj.df$car.state)/sum(car.state.freq$Freq)*100)
names(car.state.percent.freq )[2]<-"Percent.Freq"
names(car.state.percent.freq )[1]<-"car.state"
car.state.percent.freq

car.state.percent.freq<-car.state.percent.freq[car.state.percent.freq$Percent.Freq != 0,]
car.state.freq$Percent.Freq<-car.state.percent.freq$Percent.Freq
rownames(car.state.freq) <- NULL
car.state.freq

#Barchart

barplot(car.state.freq$Freq, names.arg = car.state.freq$car.state,
        xlab = "state", ylab = "Frequency", main = "Car State Frequency")

#############################################################################
#####-- Car Transmission frequency/Relative Freq/Percent Freq/Barchart--#####
#############################################################################

car.transmission.freq<-data.frame(table(nj.df$car.transmission))
names(car.transmission.freq)[2]<-"Freq"
names(car.transmission.freq)[1]<-"car.transmission"

car.transmission.freq<-car.transmission.freq[car.transmission.freq$Freq != 0,]
car.transmission.freq


# Relative Frequency

car.transmission.rel.freq <- data.frame(table(nj.df$car.transmission)/sum(car.transmission.freq$Freq))
names(car.transmission.rel.freq )[2]<-"Rel.Freq"
names(car.transmission.rel.freq )[1]<-"car.transmission"
car.transmission.rel.freq<-car.transmission.rel.freq[car.transmission.rel.freq$Rel.Freq != 0,]
car.transmission.rel.freq

car.transmission.freq$Rel.Freq<-car.transmission.rel.freq$Rel.Freq
car.transmission.freq

#Percent Frequency

car.transmission.percent.freq <- data.frame(table(nj.df$car.transmission)/sum(car.transmission.freq$Freq)*100)
names(car.transmission.percent.freq )[2]<-"Percent.Freq"
names(car.transmission.percent.freq )[1]<-"car.transmission"
car.transmission.percent.freq

car.transmission.percent.freq<-car.transmission.percent.freq[car.transmission.percent.freq$Percent.Freq != 0,]
car.transmission.freq$Percent.Freq<-car.transmission.percent.freq$Percent.Freq
rownames(car.transmission.freq) <- NULL
car.transmission.freq

#Barchart

barplot(car.transmission.freq$Freq, names.arg = car.transmission.freq$car.transmission,
        xlab = "Transmission Type", ylab = "Frequency", main = "Car Transmission Frequency")

###################################################################################################
#####-- Car Unlocking Feature Through Turo App frequency/Relative Freq/Percent Freq/Barchart--#####
###################################################################################################

car.turo.go.freq<-data.frame(table(nj.df$car.turo.go))
names(car.turo.go.freq)[2]<-"Freq"
names(car.turo.go.freq)[1]<-"car.turo.go"

car.turo.go.freq<-car.turo.go.freq[car.turo.go.freq$Freq != 0,]
car.turo.go.freq


# Relative Frequency

car.turo.go.rel.freq <- data.frame(table(nj.df$car.turo.go)/sum(car.turo.go.freq$Freq))
names(car.turo.go.rel.freq )[2]<-"Rel.Freq"
names(car.turo.go.rel.freq )[1]<-"car.turo.go"
car.turo.go.rel.freq<-car.turo.go.rel.freq[car.turo.go.rel.freq$Rel.Freq != 0,]
car.turo.go.rel.freq

car.turo.go.freq$Rel.Freq<-car.turo.go.rel.freq$Rel.Freq
car.turo.go.freq

#Percent Frequency

car.turo.go.percent.freq <- data.frame(table(nj.df$car.turo.go)/sum(car.turo.go.freq$Freq)*100)
names(car.turo.go.percent.freq )[2]<-"Percent.Freq"
names(car.turo.go.percent.freq )[1]<-"car.turo.go"
car.turo.go.percent.freq

car.turo.go.percent.freq<-car.turo.go.percent.freq[car.turo.go.percent.freq$Percent.Freq != 0,]
car.turo.go.freq$Percent.Freq<-car.turo.go.percent.freq$Percent.Freq
rownames(car.turo.go.freq) <- NULL
car.turo.go.freq

#Barchart

barplot(car.turo.go.freq$Freq, names.arg = car.turo.go.freq$car.turo.go,
        xlab = "Unlocked Through App", ylab = "Frequency", main = "Turo App Unlocked Frequency")

#####################################################################
#####-- Car Year frequency/Relative Freq/Percent Freq/Barchart--#####
#####################################################################

car.year.freq<-data.frame(table(nj.df$car.year))
names(car.year.freq)[2]<-"Freq"
names(car.year.freq)[1]<-"car.year"

car.year.freq<-car.year.freq[car.year.freq$Freq != 0,]
car.year.freq


# Relative Frequency

car.year.rel.freq <- data.frame(table(nj.df$car.year)/sum(car.year.freq$Freq))
names(car.year.rel.freq )[2]<-"Rel.Freq"
names(car.year.rel.freq )[1]<-"car.year"
car.year.rel.freq<-car.year.rel.freq[car.year.rel.freq$Rel.Freq != 0,]
car.year.rel.freq

car.year.freq$Rel.Freq<-car.year.rel.freq$Rel.Freq
car.year.freq

#Percent Frequency

car.year.percent.freq <- data.frame(table(nj.df$car.year)/sum(car.year.freq$Freq)*100)
names(car.year.percent.freq )[2]<-"Percent.Freq"
names(car.year.percent.freq )[1]<-"car.year"
car.year.percent.freq

car.year.percent.freq<-car.year.percent.freq[car.year.percent.freq$Percent.Freq != 0,]
car.year.freq$Percent.Freq<-car.year.percent.freq$Percent.Freq
rownames(car.year.freq) <- NULL
car.year.freq

#Barchart

barplot(car.year.freq$Freq, names.arg = car.year.freq$car.year,
        xlab = "Car Year", ylab = "Frequency", main = "Car Year Frequency")

###########################################################################
#####-- Host All Start frequency/Relative Freq/Percent Freq/Barchart--#####
###########################################################################

host.all.star.freq<-data.frame(table(nj.df$host.all.star))
names(host.all.star.freq)[2]<-"Freq"
names(host.all.star.freq)[1]<-"host.all.star"

host.all.star.freq<-host.all.star.freq[host.all.star.freq$Freq != 0,]
host.all.star.freq


# Relative Frequency

host.all.star.rel.freq <- data.frame(table(nj.df$host.all.star)/sum(host.all.star.freq$Freq))
names(host.all.star.rel.freq )[2]<-"Rel.Freq"
names(host.all.star.rel.freq )[1]<-"host.all.star"
host.all.star.rel.freq<-host.all.star.rel.freq[host.all.star.rel.freq$Rel.Freq != 0,]
host.all.star.rel.freq

host.all.star.freq$Rel.Freq<-host.all.star.rel.freq$Rel.Freq
host.all.star.freq

#Percent Frequency

host.all.star.percent.freq <- data.frame(table(nj.df$host.all.star)/sum(host.all.star.freq$Freq)*100)
names(host.all.star.percent.freq )[2]<-"Percent.Freq"
names(host.all.star.percent.freq )[1]<-"host.all.star"
host.all.star.percent.freq

host.all.star.percent.freq<-host.all.star.percent.freq[host.all.star.percent.freq$Percent.Freq != 0,]
host.all.star.freq$Percent.Freq<-host.all.star.percent.freq$Percent.Freq
rownames(host.all.star.freq) <- NULL
host.all.star.freq

#Barchart

barplot(host.all.star.freq$Freq, names.arg = host.all.star.freq$host.all.star,
        xlab = "All Star (Yes or No)", ylab = "Frequency", main = "Host All Star Frequency")

####################################################################################
#####-- Host Location Avaliable frequency/Relative Freq/Percent Freq/Barchart--#####
####################################################################################

host.location.available.freq<-data.frame(table(nj.df$host.location.available))
names(host.location.available.freq)[2]<-"Freq"
names(host.location.available.freq)[1]<-"host.location.available"

host.location.available.freq<-host.location.available.freq[host.location.available.freq$Freq != 0,]
host.location.available.freq


# Relative Frequency

host.location.available.rel.freq <- data.frame(table(nj.df$host.location.available)/sum(host.location.available.freq$Freq))
names(host.location.available.rel.freq )[2]<-"Rel.Freq"
names(host.location.available.rel.freq )[1]<-"host.location.available"
host.location.available.rel.freq<-host.location.available.rel.freq[host.location.available.rel.freq$Rel.Freq != 0,]
host.location.available.rel.freq

host.location.available.freq$Rel.Freq<-host.location.available.rel.freq$Rel.Freq
host.location.available.freq

#Percent Frequency

host.location.available.percent.freq <- data.frame(table(nj.df$host.location.available)/sum(host.location.available.freq$Freq)*100)
names(host.location.available.percent.freq )[2]<-"Percent.Freq"
names(host.location.available.percent.freq )[1]<-"host.location.available"
host.location.available.percent.freq

host.location.available.percent.freq<-host.location.available.percent.freq[host.location.available.percent.freq$Percent.Freq != 0,]
host.location.available.freq$Percent.Freq<-host.location.available.percent.freq$Percent.Freq
rownames(host.location.available.freq) <- NULL
host.location.available.freq

#Barchart

barplot(host.location.available.freq$Freq, names.arg = host.location.available.freq$host.location.available,
        xlab = "Location Available (Yes or No)", ylab = "Frequency", main = "Location Available Frequency")


###################################################################################
#####-- Host Verified to Drive frequency/Relative Freq/Percent Freq/Barchart--#####
###################################################################################


host.verified.approved.to.drive.freq<-data.frame(table(nj.df$host.verified.approved.to.drive))
names(host.verified.approved.to.drive.freq)[2]<-"Freq"
names(host.verified.approved.to.drive.freq)[1]<-"host.verified.approved.to.drive"

host.verified.approved.to.drive.freq<-host.verified.approved.to.drive.freq[host.verified.approved.to.drive.freq$Freq != 0,]
host.verified.approved.to.drive.freq


# Relative Frequency

host.verified.approved.to.drive.rel.freq <- data.frame(table(nj.df$host.verified.approved.to.drive)/sum(host.verified.approved.to.drive.freq$Freq))
names(host.verified.approved.to.drive.rel.freq )[2]<-"Rel.Freq"
names(host.verified.approved.to.drive.rel.freq )[1]<-"host.verified.approved.to.drive"
host.verified.approved.to.drive.rel.freq<-host.verified.approved.to.drive.rel.freq[host.verified.approved.to.drive.rel.freq$Rel.Freq != 0,]
host.verified.approved.to.drive.rel.freq

host.verified.approved.to.drive.freq$Rel.Freq<-host.verified.approved.to.drive.rel.freq$Rel.Freq
host.verified.approved.to.drive.freq

#Percent Frequency

host.verified.approved.to.drive.percent.freq <- data.frame(table(nj.df$host.verified.approved.to.drive)/sum(host.verified.approved.to.drive.freq$Freq)*100)
names(host.verified.approved.to.drive.percent.freq )[2]<-"Percent.Freq"
names(host.verified.approved.to.drive.percent.freq )[1]<-"host.verified.approved.to.drive"
host.verified.approved.to.drive.percent.freq

host.verified.approved.to.drive.percent.freq<-host.verified.approved.to.drive.percent.freq[host.verified.approved.to.drive.percent.freq$Percent.Freq != 0,]
host.verified.approved.to.drive.freq$Percent.Freq<-host.verified.approved.to.drive.percent.freq$Percent.Freq
rownames(host.verified.approved.to.drive.freq) <- NULL
host.verified.approved.to.drive.freq

#Barchart

barplot(host.verified.approved.to.drive.freq$Freq, names.arg = host.verified.approved.to.drive.freq$host.verified.approved.to.drive,
        xlab = "Host Approved to Drive", ylab = "Frequency", main = "Host Approved to Drive Frequency")

#################################################################################
#####-- Host Email Verified  frequency/Relative Freq/Percent Freq/Barchart--#####
#################################################################################


host.verified.email.freq<-data.frame(table(nj.df$host.verified.email))
names(host.verified.email.freq)[2]<-"Freq"
names(host.verified.email.freq)[1]<-"host.verified.email"

host.verified.email.freq<-host.verified.email.freq[host.verified.email.freq$Freq != 0,]
host.verified.email.freq


# Relative Frequency

host.verified.email.rel.freq <- data.frame(table(nj.df$host.verified.email)/sum(host.verified.email.freq$Freq))
names(host.verified.email.rel.freq )[2]<-"Rel.Freq"
names(host.verified.email.rel.freq )[1]<-"host.verified.email"
host.verified.email.rel.freq<-host.verified.email.rel.freq[host.verified.email.rel.freq$Rel.Freq != 0,]
host.verified.email.rel.freq

host.verified.email.freq$Rel.Freq<-host.verified.email.rel.freq$Rel.Freq
host.verified.email.freq

#Percent Frequency

host.verified.email.percent.freq <- data.frame(table(nj.df$host.verified.email)/sum(host.verified.email.freq$Freq)*100)
names(host.verified.email.percent.freq )[2]<-"Percent.Freq"
names(host.verified.email.percent.freq )[1]<-"host.verified.email"
host.verified.email.percent.freq

host.verified.email.percent.freq<-host.verified.email.percent.freq[host.verified.email.percent.freq$Percent.Freq != 0,]
host.verified.email.freq$Percent.Freq<-host.verified.email.percent.freq$Percent.Freq
rownames(host.verified.email.freq) <- NULL
host.verified.email.freq

#Barchart

barplot(host.verified.email.freq$Freq, names.arg = host.verified.email.freq$host.verified.email,
        xlab = "Host Email Verified", ylab = "Frequency", main = "Host Email Verified Frequency")


###################################################################################
#####-- Host Facebook Verified frequency/Relative Freq/Percent Freq/Barchart--#####
###################################################################################


host.verified.fb.freq<-data.frame(table(nj.df$host.verified.fb))
names(host.verified.fb.freq)[2]<-"Freq"
names(host.verified.fb.freq)[1]<-"host.verified.fb"

host.verified.fb.freq<-host.verified.fb.freq[host.verified.fb.freq$Freq != 0,]
host.verified.fb.freq


# Relative Frequency

host.verified.fb.rel.freq <- data.frame(table(nj.df$host.verified.fb)/sum(host.verified.fb.freq$Freq))
names(host.verified.fb.rel.freq )[2]<-"Rel.Freq"
names(host.verified.fb.rel.freq )[1]<-"host.verified.fb"
host.verified.fb.rel.freq<-host.verified.fb.rel.freq[host.verified.fb.rel.freq$Rel.Freq != 0,]
host.verified.fb.rel.freq

host.verified.fb.freq$Rel.Freq<-host.verified.fb.rel.freq$Rel.Freq
host.verified.fb.freq

#Percent Frequency

host.verified.fb.percent.freq <- data.frame(table(nj.df$host.verified.fb)/sum(host.verified.fb.freq$Freq)*100)
names(host.verified.fb.percent.freq )[2]<-"Percent.Freq"
names(host.verified.fb.percent.freq )[1]<-"host.verified.fb"
host.verified.fb.percent.freq

host.verified.fb.percent.freq<-host.verified.fb.percent.freq[host.verified.fb.percent.freq$Percent.Freq != 0,]
host.verified.fb.freq$Percent.Freq<-host.verified.fb.percent.freq$Percent.Freq
rownames(host.verified.fb.freq) <- NULL
host.verified.fb.freq

#Barchart

barplot(host.verified.fb.freq$Freq, names.arg = host.verified.fb.freq$host.verified.fb,
        xlab = "Host Facebook Verified", ylab = "Frequency", main = "Host Facebook Verified Frequency")


###############################################################################
####-- Host Phone Verified frequency/Relative Freq/Percent Freq/Barchart--#####
###############################################################################

host.verified.phone.freq<-data.frame(table(nj.df$host.verified.phone))
names(host.verified.phone.freq)[2]<-"Freq"
names(host.verified.phone.freq)[1]<-"host.verified.phone"

host.verified.phone.freq<-host.verified.phone.freq[host.verified.phone.freq$Freq != 0,]
host.verified.phone.freq


# Relative Frequency

host.verified.phone.rel.freq <- data.frame(table(nj.df$host.verified.phone)/sum(host.verified.phone.freq$Freq))
names(host.verified.phone.rel.freq )[2]<-"Rel.Freq"
names(host.verified.phone.rel.freq )[1]<-"host.verified.phone"
host.verified.phone.rel.freq<-host.verified.phone.rel.freq[host.verified.phone.rel.freq$Rel.Freq != 0,]
host.verified.phone.rel.freq

host.verified.phone.freq$Rel.Freq<-host.verified.phone.rel.freq$Rel.Freq
host.verified.phone.freq

#Percent Frequency

host.verified.phone.percent.freq <- data.frame(table(nj.df$host.verified.phone)/sum(host.verified.phone.freq$Freq)*100)
names(host.verified.phone.percent.freq )[2]<-"Percent.Freq"
names(host.verified.phone.percent.freq )[1]<-"host.verified.phone"
host.verified.phone.percent.freq

host.verified.phone.percent.freq<-host.verified.phone.percent.freq[host.verified.phone.percent.freq$Percent.Freq != 0,]
host.verified.phone.freq$Percent.Freq<-host.verified.phone.percent.freq$Percent.Freq
rownames(host.verified.phone.freq) <- NULL
host.verified.phone.freq

#Barchart

barplot(host.verified.phone.freq$Freq, names.arg = host.verified.phone.freq$host.verified.phone,
        xlab = "Host Phone Verified", ylab = "Frequency", main = "Host Phone Verified Frequency")




###############
###box plots###
###############


boxplot(nj.df$car.trip.price ~ nj.df$car.doors,
        xlab = "Car Doors", ylab = "car trip price",
        main = "Box Plot of car trip price by car doors")



boxplot(nj.df$car.trip.price ~ nj.df$car.year,
        xlab = "Car year", ylab = "car trip price",
        main = "Box Plot of car trip price by car model year")


################################################################################
### box plots of continous quanttitative variables before Removal of ouliers ###
################################################################################

boxplot(nj.df$car.extra.mile.fee, xlab = 'extra car mile fee')
boxplot(nj.df$car.miles.included, xlab = 'car.miles.included')
boxplot(nj.df$host.tenure.in.weeks, xlab = 'host.tenure.in.weeks')
boxplot(nj.df$car.trip.price, xlab = 'car.trip.price')
boxplot(nj.df$car.self.pickup.avg.price, xlab = 'car.self.pickup.avg.price')

####################################################
### box plot & computer ub & lb // Car.trip.price ##
####################################################

Q1 <- quantile(nj.df$car.trip.price, probs=c(.25, .75), na.rm = TRUE)

iqr <- IQR(nj.df$car.trip.price,na.rm = TRUE)
car.trip.price.lb<-Q1[1]-1.5*iqr
car.trip.price.ub<-Q1[2]+1.5*iqr
print(paste("1st Quantile: ", Q1[1], "3rd Quantile: ", Q1[2], "IQR: ",iqr))
print(paste("lb: ", car.trip.price.lb, "ub: ", car.trip.price.ub))
# trip.price.no.outliers<- subset(nj.df, nj.df$car.trip.price > lb & nj.df$car.trip.price < ub)
  
# boxplot((trip.price.no.outliers$car.trip.price))


###############################################################
###   box plot & computing ub & lb// host.tenure.in.weeks   ###
###############################################################

Q2 <- quantile(nj.df$host.tenure.in.weeks, probs=c(.25, .75), na.rm = TRUE)

iqr <- IQR(nj.df$host.tenure.in.weeks,na.rm  = TRUE)
host.tenure.lb<-Q2[1]-1.5*iqr
host.tenure.ub<-Q2[2]+1.5*iqr
print(paste("1st Quantile: ", Q2[1], "3rd Quantile: ", Q2[2], "IQR: ",iqr))
print(paste("lb: ", host.tenure.lb, "ub: ", host.tenure.ub))

# host.tenure.no.outliers<- subset(nj.df, nj.df$host.tenure.in.weeks > lb & nj.df$host.tenure.in.weeks < ub)



###############################################################
###   box plot & computing lb & ubs// car.miles.included    ###
###############################################################



Q3 <- quantile(nj.df$car.miles.included, probs=c(.25, .75), na.rm = TRUE)

iqr <- IQR(nj.df$car.miles.included,na.rm  = TRUE)
car.miles.lb<-Q3[1]-1.5*iqr
car.miles.ub<-Q3[2]+1.5*iqr
print(paste("lb: ", car.miles.lb, "ub: ", car.miles.ub, "IQR: ", iqr))
# car.miles.included.no.outliers<- subset(nj.df, nj.df$car.miles.included > lb & nj.df$car.miles.included < ub)




###################################################################
###   box plot & computing lb & ub/car.self.pickup.avg.price    ###
###################################################################

# no values but 0 in self.pickup.avg.price

#########################################################
### box plot & computing lb & ub's/car.extra.mile.fee ###
#########################################################

Q4 <- quantile(nj.df$car.extra.mile.fee, probs=c(.25, .75), na.rm = TRUE)

iqr <- IQR(nj.df$car.extra.mile.fee,na.rm  = TRUE)
car.extra.mile.lb<-Q4[1]-1.5*iqr
car.extra.mile.ub<-Q4[2]+1.5*iqr
print(paste("lb: ", car.extra.mile.lb, "ub: ", car.extra.mile.ub, "IQR: ", iqr))
# extra.mile.fee.no.outliers<- subset(nj.df, nj.df$car.extra.mile.fee > lb & nj.df$car.extra.mile.fee < ub)


####################################################################
################  REMOVE OUTLIERS FROM DATAFRAME ###################
####################################################################

nj.df.final<- subset(nj.df, nj.df$car.trip.price > car.trip.price.lb & nj.df$car.trip.price < car.trip.price.ub)
nj.df.final<- subset(nj.df.final, nj.df.final$host.tenure.in.weeks > host.tenure.lb & nj.df.final$host.tenure.in.weeks < host.tenure.ub)
nj.df.final<- subset(nj.df.final, nj.df.final$car.miles.included > car.miles.lb & nj.df.final$car.miles.included < car.miles.ub)
nj.df.final<- subset(nj.df.final, nj.df.final$car.extra.mile.fee > car.extra.mile.lb & nj.df$car.extra.mile.fee < car.extra.mile.ub)
nj.df.final<-na.omit(nj.df.final)



###################################################################
################    MLR MODEL  ####################################
###################################################################


nj.df.final$car.transmissionr<-factor(nj.df.final$car.transmission, c("Manual transmission", "Automatic transmission"))


nj.df.final$host.all.star<-ifelse(nj.df.final$host.all.star == FALSE, 0, 1)
nj.df.final$car.extra.post.trip.cleaning<-ifelse(nj.df.final$car.extra.post.trip.cleaning == FALSE, 0, 1)
nj.df.final$host.location.available<-ifelse(nj.df.final$host.location.available == FALSE, 0, 1)
nj.df.final$host.verified.phone<-ifelse(nj.df.final$host.verified.phone == FALSE, 0, 1)
nj.df.final$host.verified.email<-ifelse(nj.df.final$host.verified.email == FALSE, 0, 1)
nj.df.final$car.extra.pet.fee<-ifelse(nj.df.final$car.extra.pet.fee == FALSE, 0, 1)
nj.df.final$car.extra.unlimited.mileage<-ifelse(nj.df.final$car.extra.unlimited.mileage == FALSE, 0, 1)




# nj.df.final.lm<- lm(car.trip.price ~  car.displayed.user.review.num+host.tenure.in.weeks+ host.all.star +car.extra.post.trip.cleaning+car.extra.mile.fee, data = nj.df.final)
# nj.df.lm.summary <- summary(nj.df.final.lm)
# print(nj.df.lm.summary)

# predict(nj.df.final.lm, newdata = nj.df.final.new)
# predict(nj.df.final.lm, newdata = nj.df.final.new, interval = "confidence", level = 0.95)
# predict(nj.df.final.lm, newdata = nj.df.final.new, interval = "prediction", level = 0.95)
# 
# vif(nj.df.final.lm)

#####################################################################################
######################### Residual Analysis #########################################
#####################################################################################

# nj.df.final.dv.est <- nj.df.final.lm$fitted.values
# nj.df.final.res.std <- rstudent(nj.df.final.lm)
# plot(nj.df.final.res.std ~ nj.df.final.dv.est, pch = 19, xlab = "Fitted Value of DV",
#      ylab = "Standardized Residual", main = "Residual Analysis",
#      ylim = c(min(-3, min(nj.df.final.res.std)), max(3, max(nj.df.final.res.std))))
# abline(h = -1.5, lty = 3)
# abline(h = 1.5, lty = 3)

pairs(~car.trip.price+car.extra.mile.fee+car.miles.included+host.tenure.in.weeks,data=nj.df.final,
      main="Simple Scatterplot Matrix")


nj.df.final$car.rental.type<-factor(nj.df.final$car.rental.type, c("truck-rental", 
                                                                   "car-rental",
                                                                   "suv-rental",
                                                                   "van-rental",
                                                                   "minivan-rental"))



nj.df.final.lm<- lm(car.trip.price ~ 
                      host.car.num+
                      car.doors+
                      host.verified.email+
                      car.make+
                      car.displayed.user.review.num+
                      host.tenure.in.weeks+
                      car.extra.mile.fee+
                      host.all.star+
                      car.extra.post.trip.cleaning,
                      data = nj.df.final)


nj.df.lm.summary <- summary(nj.df.final.lm)
print(nj.df.lm.summary)

vif(nj.df.final.lm)


class(nj.df.final$car.model)
levels(nj.df.final$car.model)
