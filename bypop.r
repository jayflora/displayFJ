# adapted from OF note p.4

bypop = function(qmatrix,coord,pop) {
  # arguments:
  # qmatrix: matrix of coefficients  (nindividuals x K) # K=number of clusters
  # coord: matrix of coordinates  (nindividuals x 2)
  # pop: generic vector of population labels, can be vector of numeric, character, factor
  # returns:
  # list(matrix,matrix) 
  # qpop: average admixture coefficients for each population sample (npopulations x K)
  # coord.pop: average location for each population sample (npopulations x 2)
  # matrix rownames correpond to population labels 
  
  ulabels=unique(pop)
  nlabels=length(ulabels)
  qpop = matrix(NA, ncol = ncol(qmatrix), nrow = nlabels)
  coord.pop = matrix(NA, ncol = 2, nrow = nlabels)
  for (i in 1:nlabels){
    qpop[i,] = apply(qmatrix[pop == ulabels[i],], 2, mean)
    coord.pop[i,] = apply(coord[pop == ulabels[i],], 2, mean)
  }
  rownames(qpop)=ulabels
  rownames(coord.pop)=ulabels
  return( list(qpop=qpop,coord.pop=coord.pop) )
}

## EXAMPLE (after loading everything as indicated in Olivier's note)
pop=rep(paste("plant",1:60),each=10)
info.bypop=bypop(qmatrix,coord,pop)
qpop=info.bypop$qpop
coord.pop=info.bypop$coord.pop

pop=rep(1:60, each = 10)
info.bypop=bypop(qmatrix,coord,pop)
qpop=info.bypop$qpop
coord.pop=info.bypop$coord.pop

pop=as.factor(rep(paste("plant",1:60),each=10))
info.bypop=bypop(qmatrix,coord,pop)
qpop=info.bypop[[1]]
coord.pop=info.bypop[[2]]
