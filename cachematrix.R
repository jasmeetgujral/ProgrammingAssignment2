
#makeCacheMatrix creats special matrix to cache inverse
#it returns list of functions to store and get matrix and its inverse

makeCacheMatrix <- function(xmtx = matrix()) {
  #xmtx is the matrix for whcih inverse is to be calculates
  
  inv <- NULL
  #inverse set to NUll at the initiation.
  
  set <- function(ymtx) {
    #function to store new matrix
    xmtx <<- ymtx
    inv <<- NULL
    #as new matrix is stored its inverse is again set to Null
  }
  
  get <- function() xmtx
  #get function to call stored matrix 
  
  setinv <- function(minverse) inv <<- minverse
  #storing inverse for matrix stored
  
  getinv <- function() inv
  #to call inverse already stored
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  #list of fucntion set as output for makeCacheMatrix
}


#cachesolve function computes inverse or pick value from cache if already computed

cacheSolve <- function(x, xmtx, ...) {
  #two arguments: 1. Output list vector from makeCacheMatrix
  #2. Matric for which inverse needs to be calculated
  
  inv <- x$getinv()
  #using x$getinv() to get stored inverse
  
  if(!is.null(inv) & (all.equal(x$get,xmtx))) {
    #if stored inverse is not null and 
    #stored matrix is same as input matrix
    #inverse is returned as stored value(from cache)
        message("getting cached data")
    return(inv)
  }
  
  #if matrix inverse is not stored in cache
  
  x$set(xmtx)
  #set stored matrix to new matrix
  
  inv <- solve(xmtx, ...)
  #calculate inverse of new matrix
  
  x$setinv(inv)
  #store new inverse as cache
  
  inv
  #return inverse of a matrix
}
