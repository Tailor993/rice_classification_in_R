# This file is intended to decide rice type (Cammeo and Osmancik) based on basic aprameters
# @author Attila Szabo [Z4TUHK] <tailor993@gmail.com>
# @version 1.0

# Preconfig for Program Analysis -----------------------------------------------------

# for run time measurement
timeColumns = c("start_program","stop_program","start_fileReadAndPrepProcess","stop_fileReadAndPrepProcess", "start_usefulBasicCalc", "stop_usefulBasicCalc", "start_VisualAnalisys", "stop_VisualAnalisys", "start_ZTests", "stop_ZTests") 
time = data.frame(matrix(nrow = 1, ncol = length(timeColumns))) 
colnames(time) = timeColumns
time$start_program <- Sys.time()



# Identify the current working directory if I don't know why I can not laod the data
getwd()

# Dependency loading ----
library("GGally") # ggpairs()
library("jtools") # effect_plot()
library("rgl")    # oped3D()
library("stringr") # for later string operations
#installed.packages("partykit")
library("grid")
library("libcoin")
library("mvtnorm")
library("partykit")
#install.packages("caret",
#                 repos = "http://cran.r-project.org", 
#                 dependencies = c("Depends", "Imports", "Suggests"))
library("caret")

# File Read And PreProcessing ---------------------------------------------------------

time$start_fileReadAndPrepProcess <- Sys.time()
# Reading the Sample file which contains the collected date
#   - Header is TRUE since the first line hase the column names
#   - Sep is , since the file was too big to deal with character changing in Notpad++ and R can handle this with right configuration
#   - Dec is . becouse the source file is related to a country where they use , as a decimal separetor
riceData <- read.csv("Rice_Cammeo_Osmancik.csv", header=TRUE, sep = ",", dec=".")

# Create a numeric class to be more comperable
# - 1 means CAMMEO rice
# - 2 means OSMANCIK rice
for (i in 1:length(riceData$Class)) {
  riceData$ClassId[i] <- 0
  riceData$ClassLetter[i] <- "N"
  if( riceData$Class[i] == "Osmancik" ){
    riceData$ClassId[i] <- 2
    riceData$ClassLetter[i] <- "O"
  }
  if( riceData$Class[i] == "Cammeo" ){
    riceData$ClassId[i] <- 1
    riceData$ClassLetter[i] <- "C"
  }
}

# Create a list of dataframes
# Every element in this list represent a different type of rice
# In every riceType dataframe you will find the related propőerties
riceDataByCategory <- riceData
riceDataByCategory<-split(riceDataByCategory, riceDataByCategory$Class)

time$stop_fileReadAndPrepProcess <- Sys.time()
print("File Read and PreProcessing time: " )
time$stop_fileReadAndPrepProcess - time$start_fileReadAndPrepProcess 


# Calculation of useful basic data ---------------------------------------
time$start_usefulBasicCalc<- Sys.time()

# Setup dataframe for rice types to store the related calculated values together
# pcs - pieces - darab
# avg - avarage - atlag
# SD - standard deviation - szoras
# SE - standard error - standard hiba
columns = c("pcs",
            "areaSum",           "areaAvg",           "areaSD",           "areaSE", 
            "perimeterSum",      "perimeterAvg",      "perimeterSD",      "perimeterSE", 
            "majorAxisLengthSum","majorAxisLengthAvg","majorAxisLengthSD","majorAxisLengthSE",
            "minorAxisLengthSum","minorAxisLengthAvg","minorAxisLengthSD","minorAxisLengthSE",
            "eccentricitySum",   "eccentricityAvg",   "eccentricitySD",   "eccentricitySE",
            "convexAreaSum",     "convexAreaAvg",     "convexAreaSD",     "convexAreaSE",
            "extentSum",         "extentAvg",         "extentSD",         "extentSE"
) 
CammeoCalc = data.frame(matrix(nrow = 1, ncol = length(columns))) 
colnames(CammeoCalc) = columns
OsmanCalc = data.frame(matrix(nrow = 1, ncol = length(columns))) 
colnames(OsmanCalc) = columns

CammeoCalc$pcs <- length( riceDataByCategory$Cammeo$Area )

CammeoCalc$areaSum <- sum( riceDataByCategory$Cammeo$Area )
CammeoCalc$areaAvg <- CammeoCalc$areaSum / CammeoCalc$pcs
CammeoCalc$areaSD <- sd( riceDataByCategory$Cammeo$Area )
CammeoCalc$areaSE <- CammeoCalc$areaSD / ( sqrt( CammeoCalc$pcs )  )

CammeoCalc$perimeterSum <- sum( riceDataByCategory$Cammeo$Perimeter )
CammeoCalc$perimeterAvg <- CammeoCalc$perimeterSum / CammeoCalc$pcs
CammeoCalc$perimeterSD <- sd( riceDataByCategory$Cammeo$Perimeter )
CammeoCalc$perimeterSE <- CammeoCalc$perimeterSD / ( sqrt( CammeoCalc$pcs )  )

CammeoCalc$majorAxisLengthSum <- sum( riceDataByCategory$Cammeo$Major_Axis_Length )
CammeoCalc$majorAxisLengthAvg <- CammeoCalc$majorAxisLengthSum / CammeoCalc$pcs
CammeoCalc$majorAxisLengthSD <- sd( riceDataByCategory$Cammeo$Major_Axis_Length )
CammeoCalc$majorAxisLengthSE <- CammeoCalc$majorAxisLengthSD / ( sqrt( CammeoCalc$pcs )  )

CammeoCalc$minorAxisLengthSum <- sum( riceDataByCategory$Cammeo$Minor_Axis_Length )
CammeoCalc$minorAxisLengthAvg <- CammeoCalc$minorAxisLengthSum / CammeoCalc$pcs
CammeoCalc$minorAxisLengthSD <- sd( riceDataByCategory$Cammeo$Minor_Axis_Length )
CammeoCalc$minorAxisLengthSE <- CammeoCalc$minorAxisLengthSD / ( sqrt( CammeoCalc$pcs )  )

CammeoCalc$eccentricitySum <- sum( riceDataByCategory$Cammeo$Eccentricity )
CammeoCalc$eccentricityAvg <- CammeoCalc$eccentricitySum / CammeoCalc$pcs
CammeoCalc$eccentricitySD <- sd( riceDataByCategory$Cammeo$Eccentricity )
CammeoCalc$eccentricitySE <- CammeoCalc$eccentricitySD / ( sqrt( CammeoCalc$pcs )  )

CammeoCalc$convexAreaSum <- sum( riceDataByCategory$Cammeo$Convex_Area )
CammeoCalc$convexAreaAvg <- CammeoCalc$convexAreaSum / CammeoCalc$pcs
CammeoCalc$convexAreaSD <- sd( riceDataByCategory$Cammeo$Convex_Area )
CammeoCalc$convexAreaSE <- CammeoCalc$convexAreaSD / ( sqrt( CammeoCalc$pcs )  )

CammeoCalc$extentSum <- sum( riceDataByCategory$Cammeo$Extent )
CammeoCalc$extentAvg <- CammeoCalc$extentSum / CammeoCalc$pcs
CammeoCalc$extentSD <- sd( riceDataByCategory$Cammeo$Extent )
CammeoCalc$extentSE <- CammeoCalc$extentSD / ( sqrt( CammeoCalc$pcs )  )

OsmanCalc$pcs <- length( riceDataByCategory$Osmancik$Area )

OsmanCalc$areaSum <- sum( riceDataByCategory$Osmancik$Area )
OsmanCalc$areaAvg <- OsmanCalc$areaSum / OsmanCalc$pcs
OsmanCalc$areaSD <- sd( riceDataByCategory$Osmancik$Area )
OsmanCalc$areaSE <- OsmanCalc$areaSD / ( sqrt( OsmanCalc$pcs )  )

OsmanCalc$perimeterSum <- sum( riceDataByCategory$Osmancik$Perimeter )
OsmanCalc$perimeterAvg <- OsmanCalc$perimeterSum / OsmanCalc$pcs
OsmanCalc$perimeterSD <- sd( riceDataByCategory$Osmancik$Perimeter )
OsmanCalc$perimeterSE <- OsmanCalc$perimeterSD / ( sqrt( OsmanCalc$pcs )  )

OsmanCalc$majorAxisLengthSum <- sum( riceDataByCategory$Osmancik$Major_Axis_Length )
OsmanCalc$majorAxisLengthAvg <- OsmanCalc$majorAxisLengthSum / OsmanCalc$pcs
OsmanCalc$majorAxisLengthSD <- sd( riceDataByCategory$Osmancik$Major_Axis_Length )
OsmanCalc$majorAxisLengthSE <- OsmanCalc$majorAxisLengthSD / ( sqrt( OsmanCalc$pcs )  )

OsmanCalc$minorAxisLengthSum <- sum( riceDataByCategory$Osmancik$Minor_Axis_Length )
OsmanCalc$minorAxisLengthAvg <- OsmanCalc$minorAxisLengthSum / OsmanCalc$pcs
OsmanCalc$minorAxisLengthSD <- sd( riceDataByCategory$Osmancik$Minor_Axis_Length )
OsmanCalc$minorAxisLengthSE <- OsmanCalc$minorAxisLengthSD / ( sqrt( OsmanCalc$pcs )  )

OsmanCalc$eccentricitySum <- sum( riceDataByCategory$Osmancik$Eccentricity )
OsmanCalc$eccentricityAvg <- OsmanCalc$eccentricitySum / OsmanCalc$pcs
OsmanCalc$eccentricitySD <- sd( riceDataByCategory$Osmancik$Eccentricity )
OsmanCalc$eccentricitySE <- OsmanCalc$eccentricitySD / ( sqrt( OsmanCalc$pcs )  )

OsmanCalc$convexAreaSum <- sum( riceDataByCategory$Osmancik$Convex_Area )
OsmanCalc$convexAreaAvg <- OsmanCalc$convexAreaSum / OsmanCalc$pcs
OsmanCalc$convexAreaSD <- sd( riceDataByCategory$Osmancik$Convex_Area )
OsmanCalc$convexAreaSE <- OsmanCalc$convexAreaSD / ( sqrt( OsmanCalc$pcs )  )

OsmanCalc$extentSum <- sum( riceDataByCategory$Osmancik$Extent )
OsmanCalc$extentAvg <- OsmanCalc$extentSum / OsmanCalc$pcs
OsmanCalc$extentSD <- sd( riceDataByCategory$Osmancik$Extent )
OsmanCalc$extentSE <- OsmanCalc$extentSD / ( sqrt( OsmanCalc$pcs )  )



time$stop_usefulBasicCalc <- Sys.time()
print("Calculation of useful basic data  time: " )
time$stop_usefulBasicCalc - time$start_usefulBasicCalc 



# Visual analisys ---------------------------------------------------------
time$start_VisualAnalisys <- Sys.time()

# Draw all parameters to visualize to see relation between properties and classes
graphics.off()
# par(mfrow = c(4, 4))
plot(riceData$Area,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Area" ) # Terület
abline(v=OsmanCalc$areaAvg, col="red")
abline(v=CammeoCalc$areaAvg, col="blue")
# grid(nx=2, ny=nx)
plot(riceData$Perimeter,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Perimeter" ) # Kerület
abline(v=OsmanCalc$perimeterAvg , col="red")
abline(v=CammeoCalc$perimeterAvg , col="blue")
plot(riceData$Major_Axis_Length,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Major_Axis_Length" ) # Hosszabb oldal
plot(riceData$Minor_Axis_Length,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Minor_Axis_Length" ) # Rövidebb oldal
plot(riceData$Eccentricity,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Eccentricity" ) # Különösség?
plot(riceData$Convex_Area,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Convex_Area" ) # Konvex terület
plot(riceData$Extent,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Extent" ) # Terjedelem

plot(riceData$Area*riceData$Major_Axis_Length,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Area * MajorAxiseLength" ) # Terjedelem

plot(riceData$Area*riceData$Major_Axis_Length*riceData$Perimeter,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Area * MajorAxiseLength * Perimeter" ) # Terjedelem

# regression check by eye
graphics.off()
par(mfrow = c(2, 2))
#graphics.off()
plot(riceData$Area,riceData$Perimeter, col=factor(riceData$ClassId), main = "Area - Permit" ) # Terület - Kerület | LINEAR REGRESSION +
plot(riceData$Area,riceData$Major_Axis_Length, col=factor(riceData$ClassId), main = "Area - MAjor Axis Length" ) # Terület - Hosszab oldal | LINEAR REGRESSION +
plot(riceData$Area,riceData$Minor_Axis_Length, col=factor(riceData$ClassId), main = "Area - Minor Axis Length" ) # Terület - Rövidebb oldal |  REGRESSION +
plot(riceData$Area,riceData$Eccentricity, col=factor(riceData$ClassId), main = "Area - Eccentricity" ) # Terület -Különösség | NO 
plot(riceData$Area,riceData$Convex_Area, col=factor(riceData$ClassId), main = "Class by Area" ) # Terület - Terület convexitás | LINEAR REGRESSION +++ 
plot(riceData$Area,riceData$Extent, col=factor(riceData$ClassId), main = "Class by Area" ) # Terület -  terjedelem | NO


plot(riceData$Perimeter,riceData$Major_Axis_Length, col=factor(riceData$ClassId), main = "Class by Area" ) # Kerület - Hosszab oldal | LINEAR REGRESSION +
plot(riceData$Perimeter,riceData$Minor_Axis_Length, col=factor(riceData$ClassId), main = "Class by Area" ) # Kerület - Rövidebb oldal | REGRESSION +
plot(riceData$Perimeter,riceData$Eccentricity, col=factor(riceData$ClassId), main = "Class by Area" ) # Kerület - Különösség | NO 
plot(riceData$Perimeter,riceData$Convex_Area, col=factor(riceData$ClassId), main = "Class by Area" ) # Kerület - Terület convexitás | LINEAR REGRESSION +++ 
plot(riceData$Perimeter,riceData$Extent, col=factor(riceData$ClassId), main = "Class by Area" ) # Kerület -  terjedelem | NO


plot(riceData$Major_Axis_Length,riceData$Minor_Axis_Length, col=factor(riceData$ClassId), main = "Class by Area" ) # Hosszab oldal - Rövidebb oldal | NO
plot(riceData$Major_Axis_Length,riceData$Eccentricity, col=factor(riceData$ClassId), main = "Class by Area" ) # Hosszab oldal - Különösség | NO !!!!!
plot(riceData$Major_Axis_Length,riceData$Convex_Area, col=factor(riceData$ClassId), main = "Class by Area" ) # Hosszab oldal - Terület convexitás | LINEAR REGRESSION +
plot(riceData$Major_Axis_Length,riceData$Extent, col=factor(riceData$ClassId), main = "Class by Area" ) # Hosszab oldal -  terjedelem | NO


plot(riceData$Minor_Axis_Length,riceData$Eccentricity, col=factor(riceData$ClassId), main = "Class by Area" ) # Rövidebb oldal - Különösség | NO 
plot(riceData$Minor_Axis_Length,riceData$Convex_Area, col=factor(riceData$ClassId), main = "Class by Area" ) # Rövidebb oldal - Terület convexitás | NO
plot(riceData$Minor_Axis_Length,riceData$Extent, col=factor(riceData$ClassId), main = "Class by Area" ) # Rövidebb oldal -  terjedelem | NO


plot(riceData$Eccentricity,riceData$Convex_Area, col=factor(riceData$ClassId), main = "Class by Area" ) # Különösség  - Terület convexitás | NO
plot(riceData$Eccentricity,riceData$Extent, col=factor(riceData$ClassId), main = "Class by Area" ) # Különösség  -  terjedelem | NO

plot(riceData$Convex_Area,riceData$Extent, col=factor(riceData$ClassId), main = "Class by Area" ) # Terület convexitás  -  terjedelem | NO

# Follow my feelings
plot(riceData$Perimeter*riceData$Minor_Axis_Length,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Area" ) # Kerület - Rövidebb oldal | REGRESSION +

plot(riceData$Eccentricity*riceData$Convex_Area,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Area" ) # Különösség  - Terület convexitás | NO


plot(riceData$Minor_Axis_Length,riceData$Convex_Area, col=factor(riceData$ClassId), main = "Class by Area" ) # Rövidebb oldal - Terület convexitás | NO
plot(riceData$Minor_Axis_Length*riceData$Convex_Area,riceData$ClassId, col=factor(riceData$ClassId), main = "Class by Area" ) # Rövidebb oldal - Terület convexitás | NO

# Keep Major Axsis, permit| minor axis, major axis 
graphics.off()
par(mfrow = c(1, 2))
plot(riceData$Major_Axis_Length,riceData$Perimeter,col=factor(riceData$ClassId))
abline(a=1000 ,b=-3 , col="blue")

plot(riceData$Major_Axis_Length,riceData$Minor_Axis_Length,col=factor(riceData$ClassId))
abline(v=190, col="blue")


time$stop_VisualAnalisys <- Sys.time()
print("Visual analisys time: " )
time$stop_VisualAnalisys - time$start_VisualAnalisys 


# Z Test scripts -----
time$start_ZTests <- Sys.time()

## HELP: https://www.youtube.com/watch?v=ImbXdYrT59s
#installed.packages("partykit")
# library("grid") # Loaded at the top of the file
# library("libcoin") # Loaded at the top of the file
# library("mvtnorm") # Loaded at the top of the file
# library("partykit") # Loaded at the top of the file
#install.packages("caret",
#                 repos = "http://cran.r-project.org", 
#                 dependencies = c("Depends", "Imports", "Suggests"))
library("caret") # Loaded at the top of the file
library("partykit")
set.seed(1234)
# 2 way separation | data size | replacable the seed | 70% training set and 30% validity set || result 1 for training and 2 for testing
trainOrTest <- sample(2, nrow(riceData), replace = TRUE, prob=c(0.7,0.3))
trainingData.data <- riceData[trainOrTest==1,]
testingData.data <- riceData[trainOrTest==2,]

# Resolvable problem into FORMULA <- Result Column ~ Major_Axis_Length Minor_Axis_Length, Perimeter
myF <- ClassId ~ Area + Perimeter + Major_Axis_Length + Minor_Axis_Length + Eccentricity + Convex_Area +Extent
#myF <- ClassLetter ~ Perimeter + Major_Axis_Length + Minor_Axis_Length 

# Conditioning tree FORMULA,
riceCTree <- ctree(myF, data=trainingData.data)
# Show accurrency
table(predict(riceCTree), trainingData.data$Class)
# Draw decisions 
plot(riceCTree)


time$stop_ZTests <- Sys.time()
print("Z Tests  time: " )
time$stop_TTests - time$start_TTests 

# Post code Analisys scripts -------------------------------------
time$stop_program <- Sys.time();
print("Program TOTAL runtime: " )
time$stop_program - time$start_program 
