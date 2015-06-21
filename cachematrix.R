
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

  invM <- NULL
  
  
  set <- function (y){
    x <<- y
    invM <<- Null
  }
  
  get <- function() x
  
  setinvM <- function (inverseM) invM <<- inverseM
  
  getinvM <- function () invM
  
  list(set = set, get = get, setinvM = setinvM, getinvM = getinvM )  
}


## Write a short comment describing this function
# cacheSolve: Calculates the inverse of input matrix. It checks if the inverse is already calculated,
# it returns the result from the cache.
cacheSolve <- function(x, ...) {
invM <- x$getinvM()
  
  if (!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  
  data <- x$get()
  invM <- solve(data, ...)
  
  x$setinvM(invM)
## Return a matrix that is the inverse of 'x'
invM

}
