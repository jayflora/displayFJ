# adapted from OF note p.4

bypop = function(qmatrix,coord,pop) {
  # arguments:
  # coord: matrix of coordinates  (nindividuals x 2)
  # qmatrix: matrix of coefficients  (nindividuals x K) # K=number of clusters
  # pop: generic vector of population labels, can be vector of numeric, character, factor
  # returns:
  # list of the 2 matrix qpop and coord.pop with values averaged over individuals of a same population 
  # and with rownames correponding to the population labels 
  
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
tmp=bypop(coord,qmatrix,pop)
qpop=tmp[[1]]
coord.pop=tmp[[2]]

pop=rep(1:60, each = 10)
tmp=bypop(coord,qmatrix,pop)
qpop=tmp[[1]]
coord.pop=tmp[[2]]

pop=as.factor(rep(paste("plant",1:60),each=10))
tmp=bypop(coord,qmatrix,pop)
qpop=tmp[[1]]
coord.pop=tmp[[2]]
