## The makeCacheMatrix function creates a special matrix that can cache its inverse and the 
## cacheSolve function computes the inverse of the matrix returned by the function above. If the
## inverse has already been calculated then the cacheSolve function will retrieve the inverse
## from the cache

## The makeCacheMatrix function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix returned by the function above. 
## If the inverse has already been calculated then the cacheSolve function will retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Retrieving Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
