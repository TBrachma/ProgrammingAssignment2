## These two functions cache the inversion of a square invertible matrix

## makeCacheMatrix creates a Matrix-List of the chosen matrix to be cache by the
## following function.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set<-function(y){
    x<<- y
    mat<<- NULL
  }
  get<-function() x
  setinvmatrix<-function(solve) mat<<-solve
  getinvmatrix<-function() mat
  
  list(set=set,get=get,setinvmatrix=setinvmatrix,getinvmatrix=getinvmatrix)
  
}


## cacheSolve computes the inversion of a Matrix-List created by makeCacheMatrix
## if the computationen has already done before, nothing is computed and an output
## given to message the user

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat<-x$getinvmatrix()
  
  if(!is.null(mat)){
    message("Inverse Matrix Value from Cache")
    return(mat)
  ## already computed
  }
  matdata<-x$get()
  mat<-solve(matdata,...)
  x$setinvmatrix(mat)
  mat
}
