library(Rlibeemd)
library(tseries)
#'Calculate the Mode of a Vector
#'
#'This function calculates the mode of a vector
#'
#'@param vecteur a Vector of values which we want to calculate the mode
#'@return The mode of the Vector
#'@export
stat.mode <- function(vecteur) {
  unique(vecteur)[which.max(tabulate(match(vecteur, unique(vecteur))))]
}


#'Test the Seasonality of an IMF
#'
#'Function that Test if an IMF is Seasonal or not
#'
#'@param y a Vector representing the IMF
#'@param u the Coefficient of Comparison
#'@return TRUE/FALSE TRUE if the IMF is Seasonal, FALSE if not
#'@export
seasonality <- function(y, u){
  a<-extrema(y)
  dm<-0
  Dm<-0
  b=vector()
  b1=vector()
  for (i in 1:(length(a$minima[,1])-1)) {
    dm <-  dm + (a$minima[i])-(a$minima[i+1])
    b<-c(b,((a$minima[i])-(a$minima[i+1])))
  }
  for (i in 1:(length(a$maxima[,1])-1)) {
    Dm <-  Dm+  (a$maxima[i])-(a$maxima[i+1])
    b1<-c(b1,((a$maxima[i])-(a$maxima[i+1])))
  }
  d<-stat.mode(b)
  d1<-stat.mode(b1)
  c<-Dm/d
  c1<-dm/d1
  lk<-(length(a$maxima[,1])-1)
  pk<-(length(a$minima[,1])-1)
  test1<-(   ((c/lk)>=(1-u))    &&    ((c/lk)<=(1+u))   )
  test2<-(  ((c1/pk)>=(1-u))     &&    ((c1/pk)<=(1+u))   )
  if  (test1==TRUE | test2==TRUE)
    return(TRUE)
  else
    return("FALSE")
}



#'The rank of the last seasonal IMF if exist
#'
#'Function that return the rank of the last seasonal IMF of a given time serie.It takes a vector of observations, turn it into a time serie, apply Ensemble Empirical Mode Decomposition and then return the rank of the last seasonal IMF.
#'
#'@param t a vector of Observations
#'@param b coefficient of comparison
#'@return The rank of the last seasonal IMF
#'@export
decom <-function(t,b){
  h<-ts(t)
  s<-eemd(h)
  for(i in 1:(length(s[1,])-1))
  {
    if (seasonality(s[,i],b)==FALSE)
      return(i)

  }
  return("tous les IMFS sont saisonniaires")
}
#'The seasonal component of a time serie
#'
#'Function that returns the seasonal component of a given time serie.It takes a vector of observations, turn it into a time serie, apply Ensemble Empirical Mode Decomposition and then return the seasonal part of this serie.
#'
#'@param t a vector of Observations
#'@param c coefficient of comparison
#'@return The seasonal component of a time serie
#'@export
seasonal_part<- function(t,c){
  o<-t
  ts(t)
  s<-eemd(t)
  b=0
  if(class(decom(o,c))=="integer"){
    for(i in 1:decom(o)){
      b=b+s[,i]
      

    }}
  else return("pas de composante saisonal")
  return(b)
}
#'The trend component of a time serie
#'
#'Function that returns the trend component of a given time serie.It takes a vector of observations, turn it into a time serie, apply Ensemble Empirical Mode Decomposition and then return the trend part of this serie.
#'
#'@param t a vector of Observations
#'@param c coefficient of comparison
#'@return The trend component of a time serie
#'@export
Trend_part<- function(t,c){
  o<-t
  ts(t)
  s<-eemd(t)
  b<-0
  if (class(decom(o,c))=="integer"){
    for(i in (decom(o,c)+1):length(s[1,])){
      b<-b+s[,i]

    }}
  return(b)
}

