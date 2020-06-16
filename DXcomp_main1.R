source('DXcomp_BEASTpred.R')
source('DXcomp_BEASTsimulation.R')
source('DXcomp_distSample.R')
source('DXcomp_getDist.R')
source('DXcomp_isStochasticDom.R')
source('DXcomp_QualCheck.R')

### Section A: Please do not change this section ###
# load estimation set problems
Data = read.csv("dxcomp_data.csv", header = T)
# Simulate model
nProblems = nrow(Data);
PredictedAll = matrix(-999,ncol=5,nrow=nProblems);
### End of Section A ###

### Section B: Please only change Line 35 in this section ###
for (prob in 1:90) {
#read problem's parameters
  Ha = Data$Ha[prob];
  pHa = Data$pHa[prob];
  La = Data$La[prob];
  Hb = Data$Hb[prob];
  pHb = Data$pHb[prob];
  Lb = Data$Lb[prob];
  Amb = Data$Amb[prob];
  LotShape = Data$LotShape[prob];
  LotNum = Data$LotNum[prob];
  Manipulation = Data$Manipulation[prob];
  Corr = Data$Corr[prob];

# please plug in here your model that takes as input the 10 parameters
# defined above and gives as output a vector size 5 named "Prediction" 
# in which each cell is the predicted B-rate for one block of five trials
# example:  
  Prediction = DXcomp_BEASTpred( Ha, pHa, La, Hb, pHb, Lb, Amb, LotShape, LotNum, Corr );
# end of example
  PredictedAll[prob,] = Prediction;    
}
### End of Section B ###

### Section C: Please do not change this section ###
# check qualifications
qc = DXcomp_QualCheck(PredictedAll[1:30,])
nPass = qc[1]
namesFailed = qc[2:length(qc)]
print(nPass)
print(namesFailed)
# compute MSD
ObservedAll = Data[, c('B1', 'B2', 'B3', 'B4', 'B5')];
probMSDs = 100*sapply((PredictedAll - ObservedAll)^2,mean);
totalMSD = mean(probMSDs)
print(totalMSD)
### End of Section C ###
