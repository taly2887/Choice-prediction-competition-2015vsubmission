DXcomp_QualCheck = function( PredExp1 ) {
#Checks qualification criteria for CPC2015
#   Input: model predictions for Experiment 1's problems 
#   Output: Number of passed criteria (max 14) and a list of names of
#   failed criteria
nPass = 0;
namesFailed = c();
if (PredExp1[2,1] > 0.5 && PredExp1[1,1] < 0.5)
    {nPass = nPass +1;}
else
    {namesFailed = append(namesFailed, 'Allais')}


if (PredExp1[5,1] > PredExp1[1,1] )
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Reflection');
end

if (PredExp1[5,5] < PredExp1[1,5])
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed, 'Reversed Reflection');


if (PredExp1[10,1] > 0.5)
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Overweighting');


if (PredExp1[10,5] < 0.5)
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Underweighting');


if (PredExp1[12,1] < 0.5)
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Loss Aversion');


if (PredExp1[14,1] > PredExp1[12,1])
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Contingent LA');


if (PredExp1[20,1] < 0.5)
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Risk Aversion in St. Petersburg');


if (PredExp1[21,1] < 0.5)
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Ambiguity Aversion');


if (PredExp1[24,1] < PredExp1[3,1])
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Break Even');


if (PredExp1[25,1] > PredExp1[4,1])
{    nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Get Something');


if (PredExp1[27,1] > PredExp1[26,1])
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Splitting');


if (PredExp1[29,5] > PredExp1[28,5])
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Payoff Variability');


if (PredExp1[30,5] > PredExp1[28,5])
    {nPass = nPass +1;}
else
    namesFailed = append(namesFailed,'Correlation');




return(c(nPass, namesFailed))
}