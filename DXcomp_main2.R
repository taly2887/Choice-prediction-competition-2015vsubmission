source('DXcomp_BEASTsimulation.R')
source('DXcomp_distSample.R')
source('DXcomp_getDist.R')
source('DXcomp_isStochasticDom.R')
source('DXcomp_QualCheck.R')

### Section A: Please do not change this section ###
# load estimation set problems
Data = read.csv("dxcomp_data_wdist.csv", header = T)
# Simulate model
nProblems = nrow(Data);
PredictedAll = matrix(-999,ncol=5,nrow=nProblems);
### End of Section A ###
    
### Section B: Please only change Lines 44-50 in this section ###
for (prob in 1:90) {
  Amb = Data$Amb[prob];
  Corr = Data$Corr[prob];
  DistA=c()
  DistB=c()
  t1=as.numeric(Data[prob,c('av1','ap1','av2','ap2')])
  
  for (i in 1:2) 
    {
    if (!(is.na(t1[i*2])))
      {DistA = rbind(DistA,c(t1[2*i-1],t1[2*i]));}
    } 

  t2 = as.numeric(Data[prob,c('bv1','bp1','bv2','bp2','bv3','bp3','bv4','bp4','bv5','bp5','bv6','bp6','bv7','bp7','bv8','bp8','bv9','bp9','bv10','bp10')])
  for (i in 1:10)
  {
    if (!(is.na(t2[i*2])))
      {DistB = rbind(DistB,c(t2[2*i-1],t2[2*i]));}
  }
  
  # please plug in here your model that takes as input DistA, the 
  # distribution of Option A, DistB, the distribution of Option B,
  # and the parameters Amb and Corr. The distributions are in 
  # matrix-list form (each row is an outcome and its probability). 
  # The rows are sorted in ascending order of outcomes. 
  # The model's output should be a vector size 5 named "Prediction" 
  # in which each cell is the predicted B-rate for one block of five trials
  # example: 
  Prediction = rep(0,5)
  nSims = 10000;
  for (sim in 1:nSims)
  {
    simPred = DXcomp_BEASTsimulation(DistA, DistB, Amb, Corr);    
    Prediction = Prediction + (1/nSims)*simPred;
  }
  # end of example
  
  PredictedAll[prob,] = Prediction;    
  #print(Prediction)
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