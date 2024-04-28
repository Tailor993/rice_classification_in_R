# This file is intended to decide rice type (Cammeo and Osmancik) based on basic aprameters
# @author Attila Szabo [Z4TUHK] <tailor993@gmail.com>
# @version 1.0

# Preconfig for Program Analysis -----------------------------------------------------

# for run time measurement
timeColumns = c("start_program","stop_program","start_fileReadAndPrepProcess","stop_fileReadAndPrepProcess","start_diagramCreation","stop_diagramCreation", "start_linearRegression1", "stop_linearRegression1", "start_linearRegression2", "stop_linearRegression2","start_logosticalRegression1","stop_logosticalRegression1","start_logosticalRegression2","stop_logosticalRegression2") 
time = data.frame(matrix(nrow = 1, ncol = length(timeColumns))) 
colnames(time) = timeColumns
time$start_program <- Sys.time()


# Dependency loading ----
library("GGally") # ggpairs()
library("jtools") # effect_plot()
library("rgl")    # oped3D()

# File Read And PreProcessing ---------------------------------------------------------

time$start_fileReadAndPrepProcess <- Sys.time()
# Reading the Sample file which contains the collected date
#   - Header is TRUE since the first line hase the column names
#   - Sep is , since the file was too big to deal with character changing in Notpad++ and R can handle this with right configuration
#   - Dec is . becouse the source file is related to a country where they use , as a decimal separetor
riceData <- read.csv("Rice_Cammeo_Osmancik.csv",  header=TRUE, sep = ",", dec=".")

# Cratinga unique ID 
riceData$ID <- c(1:nrow(riceData))

# Create a numeric class to be more comperable
# - 0 means CAMMEO rice
# - 1 means OSMANCIK rice
riceData$isCammeo <- ifelse(riceData$Class == "Cammeo", 1, 0)

# Change order of the columns. Usally ID is first and the expected result is bettter as the last ones.
riceData <- riceData[, c(9,1,2,3,4,5,6,7,8,10)]

time$stop_fileReadAndPrepProcess <- Sys.time()
print("File Read and PreProcessing time: " )
time$stop_fileReadAndPrepProcess - time$start_fileReadAndPrepProcess 


# Create diagrams if needed ---------------------------------------------------
createDiagram <- TRUE
if(createDiagram) {
  time$start_diagramCreation <- Sys.time()
  ricePAirsPlot <- ggpairs(data=riceData, columns=c(2:8,10), title="Rice Diagram Matrix")
  ggsave(filename="riceDiagramMatrix.png", 
         plot=ricePAirsPlot, device="png", 
         path=getwd(), 
         height=500, 
         width=500, 
         units="mm", 
         dpi=500)
  time$stop_diagramCreation <- Sys.time()
  print("Diagram creation: " )
  time$stop_diagramCreation - time$start_diagramCreation 
  
}

# Standard Models creation ---------------------------------------------------
riceModelV1 <- isCammeo ~ Area + Perimeter + Major_Axis_Length + Minor_Axis_Length + Eccentricity + Convex_Area + Extent
riceModelV2 <- isCammeo ~ Area * Perimeter * Major_Axis_Length * Minor_Axis_Length * Eccentricity * Convex_Area * Extent

# Calculate Linear Regression Model 1 | Run: 0.02542591 -----------------------
time$start_linearRegression1 <- Sys.time()

riceLinearModellV1 <- lm(riceModelV1, data = riceData)
summary(riceLinearModellV1)
# Cammeo test | Result: 0.8882125
predict(riceLinearModellV1, 
        data.frame(Area = 14656,
                   Perimeter = 494.311,
                   Major_Axis_Length = 206.0201,
                   Minor_Axis_Length = 91.73097,
                   Eccentricity = 0.8954050,
                   Convex_Area = 15072,
                   Extent = 0.6154363)
)
# Osmanik test | Result: 0.06330175
predict(riceLinearModellV1, 
        data.frame(Area = 11486,
                   Perimeter = 420.8529968261719,
                   Major_Axis_Length = 175.25750732421875,
                   Minor_Axis_Length = 83.86408233642578,
                   Eccentricity = 0.8780770897865295,
                   Convex_Area = 11615,
                   Extent = 0.7765008211135864)
)

time$stop_linearRegression1 <- Sys.time()
print("Linear regression Model 1 : " )
time$stop_linearRegression1 - time$start_linearRegression1

# Calculate Linear Regression Model 2 | Run: 0.115773s  ---------------------
time$start_linearRegression2 <- Sys.time()
riceLinearModellV2 <- lm(riceModelV2, data = riceData)
summary(riceLinearModellV2)
# Cammeo test | Result: 1.051158
predict(riceLinearModellV2, 
        data.frame(Area = 14656,
                   Perimeter = 494.311,
                   Major_Axis_Length = 206.0201,
                   Minor_Axis_Length = 91.73097,
                   Eccentricity = 0.8954050,
                   Convex_Area = 15072,
                   Extent = 0.6154363),
        type = "response"
)
# Osmanik test | Result: -0.04021758
predict(riceLinearModellV2, 
        data.frame(Area = 11486,
                   Perimeter = 420.8529968261719,
                   Major_Axis_Length = 175.25750732421875,
                   Minor_Axis_Length = 83.86408233642578,
                   Eccentricity = 0.8780770897865295,
                   Convex_Area = 11615,
                   Extent = 0.7765008211135864),
        type = "response"
)


time$stop_linearRegression2 <- Sys.time()
print("Linear regression Model 2  : " )
time$stop_linearRegression2 - time$start_linearRegression2

# Linear regression Models results --------------------------------------------
# In runtime the Model 1 runs 78,04% faster and result can acccepted, but Model 2 have more precision.


# Logistic regression model 1 | Result: 0.03988791s ---------------------------
time$start_logosticalRegression1 <- Sys.time()
riceLogisticModelV1 <- glm(riceModelV1, 
                           data = riceData,
                           family = "binomial")
summary(riceLogisticModelV1)
# Cammeo test | Result: 0.9920536
predict(riceLogisticModelV1,
        data.frame(Area = 14656,
                   Perimeter = 494.311,
                   Major_Axis_Length = 206.0201,
                   Minor_Axis_Length = 91.73097,
                   Eccentricity = 0.8954050,
                   Convex_Area = 15072,
                   Extent = 0.6154363),
        type = "response") 
# Osmanik test | Result: 0.009709109
predict(riceLogisticModelV1, 
        data.frame(Area = 11486,
                   Perimeter = 420.8529968261719,
                   Major_Axis_Length = 175.25750732421875,
                   Minor_Axis_Length = 83.86408233642578,
                   Eccentricity = 0.8780770897865295,
                   Convex_Area = 11615,
                   Extent = 0.7765008211135864),
        type = "response"
)

time$stop_logosticalRegression1 <- Sys.time()
print("Linear regression Model 1: " )
time$stop_logosticalRegression1 - time$start_logosticalRegression1

# Logistic regression model 2 | Result: 1.152181s -----------------------------
time$start_logosticalRegression2 <- Sys.time()

riceLogisticModelV2 <- glm(riceModelV2, 
                           data = riceData,
                           family = "binomial")
summary(riceLogisticModelV2)
# Cammeo test | Result: 0.9970724
predict(riceLogisticModelV2,
        data.frame(Area = 14656,
                   Perimeter = 494.311,
                   Major_Axis_Length = 206.0201,
                   Minor_Axis_Length = 91.73097,
                   Eccentricity = 0.8954050,
                   Convex_Area = 15072,
                   Extent = 0.6154363),
        type = "response") 
# Osmanik test | Result: 0.01930926
predict(riceLogisticModelV2, 
        data.frame(Area = 11486,
                   Perimeter = 420.8529968261719,
                   Major_Axis_Length = 175.25750732421875,
                   Minor_Axis_Length = 83.86408233642578,
                   Eccentricity = 0.8780770897865295,
                   Convex_Area = 11615,
                   Extent = 0.7765008211135864),
        type = "response"
)


time$stop_logosticalRegression2 <- Sys.time()
print("Linear regression Model 2: " )
time$stop_logosticalRegression2 - time$start_logosticalRegression2

# Post code Analisys scripts -------------------------------------
time$stop_program <- Sys.time();
print("Program TOTAL runtime: " )
time$stop_program - time$start_program 
