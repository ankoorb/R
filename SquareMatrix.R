# Reshaping 3 Column R Data Frame to a Square Matrix
setwd("/Users/Ankoor/Documents/Git/R")
temp <- read.csv("airportdata.csv", stringsAsFactors = TRUE)
temp <- temp[order(temp$Origin.Airport, temp$Destination.Airport),]
oriAirport <- temp[, 1]
desAirport <- temp[, 2]
flightTime <- temp[, 3]
Index <- which(desAirport == desAirport[1])
desAirportRange <- Index[2] - 1
oriAirportRange <- length(flightTime) / desAirportRange

distMatrix <- matrix(0, nrow = oriAirportRange, ncol = desAirportRange)
for (i in 1:oriAirportRange){
        for (j in 1:desAirportRange){
                distMatrix[i, j] <- flightTime[(i - 1) * desAirportRange + j]
        }
}

dist <- data.frame(distMatrix)
names(dist) <- c(levels(desAirport))        
row.names(dist) <- c(levels(oriAirport))
View(dist)


