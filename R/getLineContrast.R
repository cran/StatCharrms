getLineContrast <-
function(Data,Treatment){
#' @export
#This attains the contrast used to test a linear relationship 
K<-nlevels(as.factor(Data[ ,Treatment]))

#Contrasts
switch (K,
return(0),
return(c(-1,1)),
return(c(-1,0,1)),
return(c(-3,-1,1,3)),
return(c(-2,-1,0,1, 2)),
return(c(-5, -3, -1,1,3,5)),
return(c(-3,-2, -1,0,1,2,3)),
return(c(-7,-5,-3,-1,1,3,5,7)),
return(c(-4,-3,-2,-1,0,1,2,3,4)),
return(c(-9,-7,-5,-3,-1, 1,3, 5, 7,9)),
)
return()
}
