## develops matrix that will link a solution to get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ##i represents the inverse
  i<-NULL
  set<-function(y){
    ##<<- assign the 'X' value to 'y' 
    x<<-y
    i<<-NULL
  }
  ##search and call for function in this formula
  get<-function()x
  ##<<- assign the 'i' value to an the term 'inversematrix' 
  setInv<-function(inversematrix)i<<-inversematrix
  getInv<-function()i
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
  
}


##CacheSolve function returns the inverse matrix of the matrix used in makeMatrixCache

cacheSolve <- function(x, ...) {
  ##Returns the inverse of matrix 'x'
  ##i is the value from the makeMatrixCache, to tie two funcitons together.
  i<-x$getInv()
  if(!is.null(i)){
    message("getting cache data")
    return(i)
  }
  matx<-x$get()
  i<-solve(matx,...)
  x$setInv(i)
  i
}
