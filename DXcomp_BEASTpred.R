DXcomp_BEASTpred = function( Ha, pHa, La, Hb, pHb, Lb, Amb, LotShape, LotNum, Corr ) {
# Prediction of BEAST model for one problem
#
#  This function gets as input 10 parameters which define a problem
#  and outputs BEAST model's prediction for the problem for five
#blocks of five trials each (the first is without and the others are with
#feedback

#Input:
#10 parametres that define a problem, as described
# in Erev, Ert, & Plonsky, 2015
#Output:
#vector length 5 with prediction per block


Prediction = rep(0,5)

# get both options' distributions
  DistA = DXcomp_getDist(Ha, pHa, La, '-', NA);
  DistB = DXcomp_getDist(Hb, pHb, Lb, LotShape, LotNum);
  
  # run model simulation nSims times
  nSims = 1000;
  for (sim in 1:nSims)
  {
  simPred = DXcomp_BEASTsimulation(DistA, DistB, Amb, Corr);    
  Prediction = Prediction + (1/nSims)*simPred;
  }
  return(Prediction)}
  