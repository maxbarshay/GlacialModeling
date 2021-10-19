smryMCMC_MR = function(  codaSamples , 
                         compValBeta0=NULL , ropeBeta0=NULL , 
                         compValBeta1=NULL , ropeBeta1=NULL , 
                         compValSigma=NULL , ropeSigma=NULL , 
                         saveName=NULL ) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  summaryInfo = rbind( summaryInfo , 
                       "beta0" = summarizePost( mcmcMat[,"beta0"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta1" = summarizePost( mcmcMat[,"beta1"] , 
                                                compVal=NULL, 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta2" = summarizePost( mcmcMat[,"beta2"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta3" = summarizePost( mcmcMat[,"beta3"] , 
                                                compVal=NULL, 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "sigma" = summarizePost( mcmcMat[,"sigma"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "nu" = summarizePost( mcmcMat[,"nu"] , 
                                             compVal=NULL , ROPE=NULL ) )

  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}

smryMCMC_Mat = function(  codaSamples , 
                         compValBeta0=NULL , ropeBeta0=NULL , 
                         compValBeta1=NULL , ropeBeta1=NULL , 
                         compValSigma=NULL , ropeSigma=NULL , 
                         saveName=NULL ) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  summaryInfo = rbind( summaryInfo , 
                       "beta0" = summarizePost( mcmcMat[,"beta0"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta1" = summarizePost( mcmcMat[,"beta1"] , 
                                                compVal=NULL, 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta2" = summarizePost( mcmcMat[,"beta2"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta3" = summarizePost( mcmcMat[,"beta3"] , 
                                                compVal=NULL, 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "sigma" = summarizePost( mcmcMat[,"sigma"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "nu" = summarizePost( mcmcMat[,"nu"] , 
                                             compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "error" = summarizePost( mcmcMat[,"error"] , 
                                             compVal=NULL , ROPE=NULL ) )
  
  summaryInfo = rbind( summaryInfo ,
                       "U[1]" = summarizePost( mcmcMat[,"U[1]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[2]" = summarizePost( mcmcMat[,"U[2]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[3]" = summarizePost( mcmcMat[,"U[3]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[4]" = summarizePost( mcmcMat[,"U[4]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[5]" = summarizePost( mcmcMat[,"U[5]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[6]" = summarizePost( mcmcMat[,"U[6]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[7]" = summarizePost( mcmcMat[,"U[7]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[8]" = summarizePost( mcmcMat[,"U[8]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[9]" = summarizePost( mcmcMat[,"U[9]"] ,
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[10]" = summarizePost( mcmcMat[,"U[10]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[11]" = summarizePost( mcmcMat[,"U[11]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[12]" = summarizePost( mcmcMat[,"U[12]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[13]" = summarizePost( mcmcMat[,"U[13]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[14]" = summarizePost( mcmcMat[,"U[14]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[15]" = summarizePost( mcmcMat[,"U[15]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[16]" = summarizePost( mcmcMat[,"U[16]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[17]" = summarizePost( mcmcMat[,"U[17]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[18]" = summarizePost( mcmcMat[,"U[18]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[19]" = summarizePost( mcmcMat[,"U[19]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[20]" = summarizePost( mcmcMat[,"U[20]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[21]" = summarizePost( mcmcMat[,"U[21]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[22]" = summarizePost( mcmcMat[,"U[22]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[23]" = summarizePost( mcmcMat[,"U[23]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "U[24]" = summarizePost( mcmcMat[,"U[24]"] ,
                                                compVal=NULL , ROPE=NULL ) )
  
  summaryInfo = rbind( summaryInfo ,
                       "l" = summarizePost( mcmcMat[,"l"] ,
                                            compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "error" = summarizePost( mcmcMat[,"error"] ,
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo ,
                       "v" = summarizePost( mcmcMat[,"v"] ,
                                                compVal=NULL , ROPE=NULL ) )

  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}

smryMCMC_Exp = function(  codaSamples , 
                      compValBeta0=NULL , ropeBeta0=NULL , 
                      compValBeta1=NULL , ropeBeta1=NULL , 
                      compValSigma=NULL , ropeSigma=NULL , 
                      saveName=NULL ) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  summaryInfo = rbind( summaryInfo , 
                       "beta0" = summarizePost( mcmcMat[,"beta0"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta1" = summarizePost( mcmcMat[,"beta1"] , 
                                                compVal=NULL, 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta2" = summarizePost( mcmcMat[,"beta2"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "beta3" = summarizePost( mcmcMat[,"beta3"] , 
                                                compVal=NULL, 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "sigma" = summarizePost( mcmcMat[,"sigma"] , 
                                                compVal=NULL , 
                                                ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "nu" = summarizePost( mcmcMat[,"nu"] , 
                                             compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[1]" = summarizePost( mcmcMat[,"U[1]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[2]" = summarizePost( mcmcMat[,"U[2]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[3]" = summarizePost( mcmcMat[,"U[3]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[4]" = summarizePost( mcmcMat[,"U[4]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[5]" = summarizePost( mcmcMat[,"U[5]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[6]" = summarizePost( mcmcMat[,"U[6]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[7]" = summarizePost( mcmcMat[,"U[7]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[8]" = summarizePost( mcmcMat[,"U[8]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[9]" = summarizePost( mcmcMat[,"U[9]"] , 
                                               compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[10]" = summarizePost( mcmcMat[,"U[10]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[11]" = summarizePost( mcmcMat[,"U[11]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[12]" = summarizePost( mcmcMat[,"U[12]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[13]" = summarizePost( mcmcMat[,"U[13]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[14]" = summarizePost( mcmcMat[,"U[14]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[15]" = summarizePost( mcmcMat[,"U[15]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[16]" = summarizePost( mcmcMat[,"U[16]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[17]" = summarizePost( mcmcMat[,"U[17]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[18]" = summarizePost( mcmcMat[,"U[18]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[19]" = summarizePost( mcmcMat[,"U[19]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[20]" = summarizePost( mcmcMat[,"U[20]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[21]" = summarizePost( mcmcMat[,"U[21]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[22]" = summarizePost( mcmcMat[,"U[22]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[23]" = summarizePost( mcmcMat[,"U[23]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  summaryInfo = rbind( summaryInfo , 
                       "U[24]" = summarizePost( mcmcMat[,"U[24]"] , 
                                                compVal=NULL , ROPE=NULL ) )
  
  summaryInfo = rbind( summaryInfo , 
                       "l" = summarizePost( mcmcMat[,"l"] , 
                                            compVal=NULL , ROPE=NULL ) )
  
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}