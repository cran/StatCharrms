getQuadContrast <-
function(Data,Treatment){
#' @export
#This attains the contrast used to test a quadratic relationship 

K<-nlevels(as.factor(Data[ ,Treatment]))
#Contrasts
switch (K,
return(0),
return(c( 0,0)),
return(c( 1,-2,1)),
return(c(1,-1,-1,1)),
return(c(2,-1,-2,-1,2)),
return(c(5,-1,-4,-4,-1,5)),
return(c(5,0,-3,-4,-3,0,5)),
return(c(7,1,-3,-5,-5,-3,1,7)),
return(c(28,7,-8,-17,-20,-17,-8,7,28)),
return(c(6,2,-1,-3,-4,-4,-3,-1,2,6)),
)
return()
}
