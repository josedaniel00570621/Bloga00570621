install.packages("rgenoud")       # Para la optimización usando el algoritmo genético
install.packages("dplyr")         # Manipulación de bases datos
install.packages("readr")         # Lectura de archivos
install.packages("deSolve")       # Para resolver sistemas de ecuaciones diferenciales
install.packages("data.table")    # Para manipulación eficiente de tablas
install.packages("parallel")      # Para paralelización (viene incluida con R, pero asegúrate)
install.packages("foreach")       # Para ciclos paralelos
install.packages("doParallel")    # Backend para paralelizar con foreach
install.packages("readr")         # Lectura rápida de archivos CSV
install.packages("dplyr")         # Manipulación de datos (opcional pero muy útil)
install.packages("mFilter")
install.packages("ggpubr")
library(mFilter)

#in pc
  root <- "C:\\Users\\josed\\OneDrive\\JDFR\\MEK\\Materias\\T6 - Actual\\Macroeconomía\\Proyecto Final\\"
  model.version <- "Ediam_v2020_02_18"
  dir.model <- "C:\\Users\\josed\\OneDrive\\JDFR\\MEK\\Materias\\T6 - Actual\\Macroeconomía\\Proyecto Final\\Ediam_v2020_02_18\\"
  dir.calib <- paste0(dir.model, "CalibrationScripts\\")
  dir.data  <- paste0(dir.model, "Data\\")

  source("C:/Users/josed/OneDrive/JDFR/MEK/Materias/T6 - Actual/Macroeconomía/Proyecto Final/Ediam_v2020_02_18/ediam_Equations_discrete.r")
  source("C:/Users/josed/OneDrive/JDFR/MEK/Materias/T6 - Actual/Macroeconomía/Proyecto Final/Ediam_v2020_02_18/ediam_Main.r")
  source("C:/Users/josed/OneDrive/JDFR/MEK/Materias/T6 - Actual/Macroeconomía/Proyecto Final/Ediam_v2020_02_18/CalibrationScripts/ediam_InitialConditions.r")
  source("C:/Users/josed/OneDrive/JDFR/MEK/Materias/T6 - Actual/Macroeconomía/Proyecto Final/Ediam_v2020_02_18/ediam_Plot.r")

  exists("ediamEmpiricalParameters")
  exists("ediamMain")
  exists("ediamPolicyEval")

#Load Data with scenarios

 #Select Population Scenario
 #pop.scenario.name<-"UN.Median.PI"
  pop.scenario.name<-"UN.Lower95.PI"
 #Load population scenario
  pop.scenario<<-read.csv(paste(dir.data,"PopulationScenarios.csv",sep=""))
  pop.scenario<<-subset(pop.scenario,pop.scenario$Scenario==pop.scenario.name)
 #Select price of oil scenario
  oil.scenario.name<-"Baseline"
 #Load oil scenario
  oil.scenario<<-read.csv(paste(dir.data,"OilPriceScenarios.csv",sep=""))
  oil.scenario<<-subset(oil.scenario,oil.scenario$Scenario==oil.scenario.name)
#Select climate change scenario
  climate.scenario<<-read.csv(paste(dir.data,"ClimateScenarios.csv",sep=""))
  #climate.scenario.name<-"bcc-csm1-1"
  climate.scenario.name<-"GFDL-ESM2G"
  climate.scenario<<-subset(climate.scenario,climate.scenario$Climate.Model==climate.scenario.name)

#Load script for running Ediam
  source(paste(dir.model,"ediam_Main.r",sep=""))
##Load script for plotting Ediam results
  source(paste(dir.model,"ediam_Plot.r",sep=""))
#Load script for determining initial conditions
  source(paste(dir.calib,"ediam_InitialConditions.r",sep=""))
#Load EDIAM Equations
  source(paste(dir.model,"ediam_Equations_discrete.r",sep=""))

# Specify function for evaluating the performance of policy interventi
#definitely a maximization problem

ediamPolicyEval<-function(x,verbose=FALSE){

#Simulation Time Step
  TimeStep<<-5
#Base year for simulation: 2014
#historic data with hp
  Y_re.Nh <<- 490.0097 #Mtoe
  Y_ce.Nh <<- 4255.120 #Mtoe
  Y_re.Sh <<- 1386.1706 #Mtoe
  Y_ce.Sh <<- 7002.889 #Mtoe

#Oil Supply
  Re.Nh<<- 4255.120 #Mtoe
  Re.Sh<<- 7002.889 #Mtoe
  ReToGDP.Nh<<- 0.06227680 # [1]
  ReToGDP.Sh<<- 0.18765958 # [1]

#Oil prices
   oil.times<<- oil.scenario$Year - 2014
   Price.oil.y<<- oil.scenario$Price.Oil

#GDP with hp
    GDP.Nh<<- 47034.07 #(billion 2010 USD using exchange rates)
    GDP.Sh<<- 25688.192 #(billion 2010 USD using exchange rates)

#Population
   pop.times <<- pop.scenario$Year - 2014 #years
   L.N.y <<- pop.scenario$AdvancedRegion #millions
   L.S.y <<- pop.scenario$EmergingRegion #millions

#Calibrated parameters

   calib.params<<-c(
                 epsilon.N = round(2.269395134,4),  #round(2.288057866,4),
                 epsilon.S = round(3.166191929,4),  #round(3.012365201,4),
                 Gamma_re =  round(0.027114996,4), #round(0.029518117,4),
                 Gamma_ce=   round(0.006830185,4),  #round(0.007385199,4),
                 Eta_re.N =  round(0.761353468,4), #round(0.702797514,4),
                 Eta_ce.N =  round(0.942985835,4), #round(0.822026698,4),
                 Eta_re.S =  round(0.051594995,4), #round(0.043944150,4),
                 Eta_ce.S =  round(0.023885790,4), #round(0.022691853,4),
                 val.param.N = round(1.394925760,4), ##round(1.466980717,4),
                 val.param.S = round(1.499957895,4), #round(1.393887206,4),
                 lrng.re.N = 0.0,
                 lrng.ce.N = 0.0,
                 lrng.re.S = 0.0,
                 lrng.ce.S = 0.0,
                 alfa.N = round(0.316324021,4), #round(0.321840915,4),
                 alfa.S = round(0.299931522,4)  #round(0.321840915,4)
                 )
#Climate parameters

   climate.params<<-c(
                   qsi= climate.scenario$qsi,
                   Delta.S = climate.scenario$Delta.S,
                   Delta.Temp.Disaster = climate.scenario$Delta.Temp.Disaster,
                   Beta.Delta.Temp = climate.scenario$Beta.Delta.Temp,
                   CO2.base = climate.scenario$CO2.base,
                   CO2.Disaster = climate.scenario$CO2.Disaster,
                   DeltaTempConstant = climate.scenario$DeltaTempConstant,
                   S_0 = climate.scenario$S.0
                   )

#x<-c(0,0,0,0,0,0,0,0)
#Policy vector
#   policy.params<<-c(
#                  #Advanced Region
#                      ce.tax.N=round(x[1],3),
#                      Tec.subsidy.N = round(x[2],3),
#                      RD.subsidy.N = round(x[3],3),
#                      policy.half.life.N = round(x[4],4),
#                  #Emerging Region
#                      ce.tax.S=round(x[5],3),
#                      Tec.subsidy.S = round(x[6],3),
#                      RD.subsidy.S = round(x[7],3),
#                      policy.half.life.S = round(x[8],4)
#                    )
#
#Policy vector
   policy.params<<-c(
                  #Advanced Region
                      ce.tax.N = round(x[1],3),
                       Schedule.ce.tax.N = round(x[2],4),
                      Tec.subsidy.N = round(x[3],3),
                       Schedule.Tec.subsidy.N = round(x[4],4),
                      RD.subsidy.N = round(x[5],3),
                       Schedule.RD.subsidy.N = round(x[6],4),
                  #Emerging Region
                      ce.tax.S = round(x[7],3),
                       Schedule.ce.tax.S = round(x[8],4),
                      Tec.subsidy.S = round(x[9],3),
                       Schedule.Tec.subsidy.S = round(x[10],4),
                      RD.subsidy.S = round(x[11],3),
                       Schedule.RD.subsidy.S = round(x[12],4)
                    )

# Execute simulation
if (verbose==FALSE) {
                      Objective.Function.Value<-ediamMain(calib.params,verbose=FALSE)
                      return(Objective.Function.Value)
                    } else {
                      SimulData<-ediamMain(calib.params,verbose=TRUE)
                      return(SimulData)
                           }
}

#############################################################################################
# Use algorithm genoud to estimate parameters
#############################################################################################
#Optimization
#Load library for parallel computing
  library(snow)
#Specify numbers of cores available for calibration
  #nCore<-8
  nCore<-16
#Define cluster
  cl <- makeSOCKcluster(rep("localhost", nCore))
  global.elements <- c("ediamPolicyEval", "ediamMain", "ediamEmpiricalParameters", "ediamInitialConditions", "EdiamEquations", "Ediam", "oil.scenario", "pop.scenario", "climate.scenario")
  clusterExport(cl, global.elements,envir=environment())


#Load libary genoud
 library(rgenoud)
#set seed for genetic optimization
 set.seed(55555)
#Execute the optimization
 genoud(ediamPolicyEval,max=TRUE,
       #nvars=8,
       nvars=12,
       starting.values = c( 0.5, #ce.tax.N
                             0.02, #Schedule.ce.tax.N
                            0.10, # Tec.subsidy.N
                             0.02, #Schedule.Tec.subsidy.N
                            1.0, # RD.Subsidy.N
                             0.02, #Schedule.RD.subsidy.N
                            #0.02, #policy.half.life.N
                            0.5, #ce.tax.S
                             0.02, #Schedule.ce.tax.S
                            0.10, # Tec.subsidy.S
                             0.02, #Schedule.Tec.subsidy.S
                            1.0, # RD.Subsidy.S
                             0.02 #Schedule.RD.subsidy.S
                            #0.02 #policy.half.life.S
                          ),
       pop.size=1000,
       Domains=matrix(c(
                        #inferior limits
                              0.0, #ce.tax.N
                               0.0, #Schedule.ce.tax.N
                              0.0, # Tec.subsidy.N
                               0.0, #Schedule.Tec.subsidy.N
                              0.0, # RD.Subsidy.N
                               0.0, #Schedule.RD.subsidy.N
                              #0.0, #policy.half.life.N
                              0.0, #ce.tax.S
                               0.0, #Schedule.ce.tax.S
                              0.0, # Tec.subsidy.S
                               0.0, #Schedule.Tec.subsidy.S
                              0.0, # RD.Subsidy.S
                               0.0, #Schedule.RD.subsidy.S
                              #0.0, #policy.half.life.S
                        #superior limits
                              1.0, #ce.tax.N
                               0.1, #Schedule.ce.tax.N
                              0.9, # Tec.subsidy.N
                               0.1, #Schedule.Tec.subsidy.N
                              4.0, # RD.Subsidy.N
                               0.1, #Schedule.RD.subsidy.N
                              #0.1, #policy.half.life.N
                              1.0, #ce.tax.S
                               0.1, #Schedule.ce.tax.S
                              0.9, # Tec.subsidy.S
                               0.1, #Schedule.Tec.subsidy.S
                              4.0, # RD.Subsidy.S
                               0.1 #Schedule.RD.subsidy.S
                              #0.1 #policy.half.life.S
                              ),
                            ncol=2),
       cluster=cl,
       print.level=1)

stopCluster(cl)

## Solución generada por genoud para optimo
#'wait.generations' limit reached.
#No significant improvement in 10 generations.
#Solution Fitness Value: -9.982071e+00
#Parameters at the Solution (parameter, gradient):
# X[ 1] :        1.673185e-01    G[ 1] : 0.000000e+00
# X[ 2] :        6.982543e-03    G[ 2] : 0.000000e+00
# X[ 3] :        8.739928e-01    G[ 3] : 0.000000e+00
# X[ 4] :        9.734152e-04    G[ 4] : 0.000000e+00
# X[ 5] :        8.260101e-01    G[ 5] : 0.000000e+00
# X[ 6] :        6.865487e-02    G[ 6] : 0.000000e+00
# X[ 7] :        1.994332e-01    G[ 7] : 0.000000e+00
# X[ 8] :        9.900498e-03    G[ 8] : 0.000000e+00
# X[ 9] :        7.537986e-01    G[ 9] : 0.000000e+00
# X[10] :        2.967277e-03    G[10] : 0.000000e+00
# X[11] :        3.255137e-03    G[11] : 0.000000e+00
# X[12] :        2.397878e-05    G[12] : 0.000000e+00
#Solution Found Generation 48
#Number of Generations Run 59
#Fri Jul  4 17:54:33 2025
#Total run time : 0 hours 23 minutes and 38 seconds
#$value
#[1] -9.982071
#$par
# [1] 1.673185e-01 6.982543e-03 8.739928e-01 9.734152e-04 8.260101e-01
# [6] 6.865487e-02 1.994332e-01 9.900498e-03 7.537986e-01 2.967277e-03
#[11] 3.255137e-03 2.397878e-05
#$gradients
# [1] 0 0 0 0 0 0 0 0 0 0 0 0
#$generations
#[1] 59
#$peakgeneration
#[1] 48
#$popsize
#[1] 1000
#$operators
#[1] 122 125 125 125 125 126 125 126   0
#>
#> stopCluster(cl)

x1 <- c(
  #Advanced Region
  0.1759729267,   # ce.tax.N
  0.0083342075,   # schedule.ce.tax.N
  0.8278753097,   # Tec.subsidy.N
  0.0023607783,   # schedule.tec.subsidy.N
  0.2428055360,   # RD.subsidy.N
  0.0748504300,   # schedule.RD.subsidy.N
  #Emerging Region
  0.1915780205,   # ce.tax.S
  0.0007484287,   # schedule.ce.tax.S
  0.6971772584,   # Tec.subsidy.S
  0.0091336108,   # schedule.tec.subsidy.S
  0.0002592633,   # RD.subsidy.S
  0.0652135338    # schedule.RD.subsidy.S
)

SimulData1 <- ediamPolicyEval(x1, verbose = TRUE)

#cost of policy (based on SimulData3)
total.cost.N <- sum(SimulData1$RealTecsubsidy.N * 5) + sum(SimulData1$RealRDsubsidy.N * 5)
total.cost.N

# Share on RD
sum(SimulData1$RealRDsubsidy.N * 5) / total.cost.N

# Share on Technology
sum(SimulData1$RealTecsubsidy.N * 5) / total.cost.N

# Total cost region S
total.cost.S <- sum(SimulData1$RealTecsubsidy.S * 5) + sum(SimulData1$RealRDsubsidy.S * 5)
total.cost.S

# Share on RD
sum(SimulData1$RealRDsubsidy.S * 5) / total.cost.S

# Share on Technology
sum(SimulData1$RealTecsubsidy.S * 5) / total.cost.S

# Share of GDP
total.cost.N / sum(SimulData1$GDP.N * 5)
total.cost.S / sum(SimulData1$GDP.S * 5)

#first we would need to simulate the model
SimulData1 <- ediamPolicyEval(x1, verbose = TRUE)

library(ggplot2)
library(reshape2)
library(ggpubr)

windows()
ediamPlot(SimulData1)  # plot for the zero-policy baseline

## Escenario x2: Análisis en cambio de variables de mayor impacto en compensación de esfuerzo entre OECD y no OECD

x2 <- c(
  #Advanced Region
  0.1759729267,   # ce.tax.N
  0.0083342075,   # schedule.ce.tax.N
  0.3078753097,   # Tec.subsidy.N
  0.0023607783,   # schedule.tec.subsidy.N
  0.0528055360,   # RD.subsidy.N
  0.0148504300,   # schedule.RD.subsidy.N
  #Emerging Region
  0.1915780205,   # ce.tax.S
  0.0007484287,   # schedule.ce.tax.S
  0.6971772584,   # Tec.subsidy.S
  0.0091336108,   # schedule.tec.subsidy.S
  0.52592633,     # RD.subsidy.S
  0.00252135338    # schedule.RD.subsidy.S
)

# Ejecutar la simulación con x2
SimulData2 <- ediamPolicyEval(x2, verbose = TRUE)

# Calcular costos y proporciones
total.cost.N <- sum(SimulData2$RealTecsubsidy.N * 5) + sum(SimulData2$RealRDsubsidy.N * 5)
total.cost.S <- sum(SimulData2$RealTecsubsidy.S * 5) + sum(SimulData2$RealRDsubsidy.S * 5)

cat("\n--- COSTOS ---\n")
cat("Costo total Norte:", total.cost.N, "\n")
cat("  Share RD Norte:", sum(SimulData2$RealRDsubsidy.N * 5) / total.cost.N, "\n")
cat("  Share Tec Norte:", sum(SimulData2$RealTecsubsidy.N * 5) / total.cost.N, "\n")

cat("Costo total Sur:", total.cost.S, "\n")
cat("  Share RD Sur:", sum(SimulData2$RealRDsubsidy.S * 5) / total.cost.S, "\n")
cat("  Share Tec Sur:", sum(SimulData2$RealTecsubsidy.S * 5) / total.cost.S, "\n")

cat("\n--- COSTOS COMO % DEL PIB ---\n")
cat("Norte:", total.cost.N / sum(SimulData2$GDP.N * 5), "\n")
cat("Sur:", total.cost.S / sum(SimulData2$GDP.S * 5), "\n")

# Graficar resultados
windows()
ediamPlot(SimulData2)
