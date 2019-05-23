## I wrote this R function which caches the Inverse of a matrix in memory to avoid recalculation when the inverse 
##is requested for the same matrix again or calculate a new inverse if the matrix changes and caches the result in memory


## Function creates a special “matrix” and provides function to set values,get values,set inverse, get inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInv = getInverse)
}


## Below  Function will look into cache to see if inverse exist for matrix and ##retrieve the value.
##If its the same matrix but will calculate if no inverse exist or matrix is not same.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv      
}

