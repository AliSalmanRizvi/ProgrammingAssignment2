## The function, makeCacheMatrix creates a special vector of functions, 
## which is really a list containing a function to

## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of the inverse of matrix
## d. get the value of the inverse of matrix

## Note: To test, define a square matrix using makeCacheMatrix() and then
## evaluate the inverse via cacheSolve which leverages the caching logic
## by way of invoking functions defined via makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {

  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  } 
  get <- function() x
  setinverse <- function(solve) inverse_x <<- solve
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function calculates the mean of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the mean has already been calculated. 

## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value 
## of the mean in the cache via the setmean function.

cacheSolve <- function(x,...) {
    
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("getting cached data")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data)
  x$setinverse(inverse_x)
  inverse_x
}
