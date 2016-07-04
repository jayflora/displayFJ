

bypop = function(coord,qmatrix,pop) {
  # returns 2 matrix with rownames correponding to the populaation labels 
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
  return( list(qpop,coord.pop) )
}

## EXAMPLE

pop=rep(1:60, each = 10)
tmp=bypop(coord,qmatrix,pop)
qpop=tmp[[1]]
coord.pop=tmp[[2]]

pop=rep(paste("plant",1:60),each=10)
tmp=bypop(coord,qmatrix,pop)
qpop=tmp[[1]]
coord.pop=tmp[[2]]

pop=as.factor(rep(paste("plant",1:60),each=10))
tmp=bypop(coord,qmatrix,pop)
qpop=tmp[[1]]
coord.pop=tmp[[2]]
