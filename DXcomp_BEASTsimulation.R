DXcomp_BEASTsimulation = function (DistA, DistB, Amb, Corr)
{
  SIGMA = 7;
  KAPA = 3;
  BETA = 2.6;
  GAMA = 0.5;
  PSI = 0.07;
  THETA = 1;
  
  nTrials = 25;
  firstFeedback = 6;
  nBlocks = 5;
  
  #draw personal traits
  sigma = SIGMA*runif(1);
  kapa = sample(KAPA,1);
  beta = BETA*runif(1);
  gama = GAMA*runif(1);
  psi = PSI*runif(1);
  theta = THETA*runif(1);
  
  pBias = rep(nTrials - firstFeedback+1,1);
  ObsPay = matrix(0,nrow=nTrials - firstFeedback+1,ncol=2); # observed outcomes in A (col1) and B (col2)
  Decision = -999*matrix(1,nrow=nTrials,ncol=1);
  simPred = -999*matrix(1,nrow=1,ncol=nBlocks);
  # Useful variables
  nA = nrow(DistA); # num outcomes in A
  nB = nrow(DistB); # num outcomes in B

  if (Amb == 1)
  {ambiguous = TRUE}
  else
  {ambiguous = FALSE}

nfeed = 0; # "t"; number of outcomes with feedback so far
pBias[nfeed+1] = beta/(beta+1+nfeed^theta);
MinA = DistA[1,1];
MinB = DistB[1,1];
MaxOutcome = max(DistA[nA,1],DistB[nB,1]);
SignMax = sign(MaxOutcome);
if (MinA == MinB)
  {RatioMin = 1;}
else if (sign(MinA) == sign(MinB))
  {RatioMin = min(abs(MinA),abs(MinB))/max(abs(MinA),abs(MinB));}
else
  {RatioMin = 0;}

Range = MaxOutcome - min(MinA, MinB);
trivial = DXcomp_isStochasticDom( DistA, DistB );
BEVa = DistA[,1]%*%DistA[,2]; 
if (ambiguous)

    outcomeA = DXcomp_distSample(DistA[,1],rep(1/nA,nA),rndNum[2]);
    outcomeB = DXcomp_distSample(DistB[,1],rep(1/nB,nB),rndNum[2]);
  }

# simulation of decisions
for (trial in 1:nTrials)
{
  STa = 0;
  STb = 0;
  # mental simulations
  if (nB>3) #uniform+sign
  {
    outcomeAA = DXcomp_distSample(DistA[,1],rep(1/nA,nA),rndNum[2]);
    outcomeBB = DXcomp_distSample(DistB[,1],rep(1/nB,nB),rndNum[2]);
    
    if (nfeed == 0)
    {
      outcomeAAA = Range * DXcomp_distSample(sign(DistA[,1]),DistA[,2],rndNum[2]);
      outcomeBBB = Range * DXcomp_distSample(sign(DistB[,1]),pEstB,rndNum[2]);
    }
    else
    {
      uniprobs = rep(1/nfeed,nfeed);
      outcomeAAA = Range * DXcomp_distSample(sign(ObsPay[1:nfeed,1]),uniprobs,rndNum[2]);
      outcomeBBB = Range * DXcomp_distSample(sign(ObsPay[1:nfeed,2]),uniprobs,rndNum[2]);
    }
    outcomeA=0.5*outcomeAA+0.5*outcomeAAA
    outcomeB=0.5*outcomeBB+0.5*outcomeBBB}
  
  else for (s in 1:kapa) {
    rndNum = runif(2);
    if (rndNum[1] > pBias[nfeed+1]) # Unbiased technique
    {
      if (nfeed == 0)
    {
      outcomeA = DXcomp_distSample(DistA[,1],DistA[,2],rndNum[2]);
      outcomeB = DXcomp_distSample(DistB[,1],pEstB,rndNum[2]);
    }
    else {
    uniprobs = rep(1/nfeed,nfeed)
    outcomeA = DXcomp_distSample(ObsPay[1:nfeed,1],uniprobs,rndNum[2]);
    outcomeB = DXcomp_distSample(ObsPay[1:nfeed,2],uniprobs,rndNum[2]);
    }           
  }
    
  else if (rndNum[1] > (2/3)*pBias[nfeed+1]) #uniform
{
  outcomeA = DXcomp_distSample(DistA[,1],rep(1/nA,nA),rndNum[2]);
  outcomeB = DXcomp_distSample(DistB[,1],rep(1/nB,nB),rndNum[2]);
}
else if (rndNum[1] > (1/3)*pBias[nfeed+1]) #contingent pessimism
{
if (SignMax > 0 && RatioMin < gama)
{
  outcomeA = MinA;
  outcomeB = MinB;
}
else
  {
    outcomeA = DXcomp_distSample(DistA[,1],rep(1/nA,nA),rndNum[2]);
    outcomeB = DXcomp_distSample(DistB[,1],rep(1/nB,nB),rndNum[2]);
  }
}
else # Sign
{
if (nfeed == 0)
{
  outcomeA = Range * DXcomp_distSample(sign(DistA[,1]),DistA[,2],rndNum[2]);
  outcomeB = Range * DXcomp_distSample(sign(DistB[,1]),pEstB,rndNum[2]);
}
else
{
  uniprobs = rep(1/nfeed,nfeed);
  outcomeA = Range * DXcomp_distSample(sign(ObsPay[1:nfeed,1]),uniprobs,rndNum[2]);
  outcomeB = Range * DXcomp_distSample(sign(ObsPay[1:nfeed,2]),uniprobs,rndNum[2]);
}
}
STa = STa + outcomeA;
STb = STb + outcomeB;
}
STa = STa/kapa;
STb = STb/kapa;

#error term
if (trivial)
  {error = 0;}
else
  error = sigma*rnorm(1); # positive values contribute to attraction to A


# decision
Decision[trial] = (BEVa - BEVb) + (STa - STb) + error < 0;
if ((BEVa - BEVb) + (STa - STb) + error == 0)
  Decision[trial] = sample(2,1) -1;


if (trial >= firstFeedback) # got feedback
{
  nfeed = nfeed +1;
  pBias[nfeed+1] = beta/(beta+1+nfeed^theta);
  rndNumObs = runif(1);
  ObsPay[nfeed,1] = DXcomp_distSample(DistA[,1],DistA[,2],rndNumObs); # draw outcome from A

  if (Corr == 1)
    {ObsPay[nfeed,2] = DXcomp_distSample(DistB[,1],DistB[,2],rndNumObs);}
  else if (Corr == -1)
    {ObsPay[nfeed,2] = DXcomp_distSample(DistB[,1],DistB[,2],1-rndNumObs);}
  else
    {ObsPay[nfeed,2] = DXcomp_distSample(DistB[,1],DistB[,2],runif(1)); # draw outcome from B}
  if (ambiguous)
    {BEVb = (1-1/(nTrials-firstFeedback+1))*BEVb + 1/(nTrials-firstFeedback+1)*ObsPay[nfeed,2];}}}}

#compute B-rates for this simulation
blockSize = nTrials/nBlocks;
for (b in 1:nBlocks)
if  (nBlocks=2)  {simPred[b] = mean(Decision[((b-1)*blockSize+1):(b*blockSize)])+runif(1,0,0.14);}
else             {simPred[b] = mean(Decision[((b-1)*blockSize+1):(b*blockSize)]);}
return(simPred)}