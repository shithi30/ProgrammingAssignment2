## A pair of functions that cache the inverse of a matrix:

# The first function, makeVector creates a special "vector", which is really a list containing a function to:
# - set the value of the vector
# - get the value of the vector
# - set the value of the mean
# - get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The following function calculates the mean of the special "vector" created with the above function. 
# If already calculated, it gets the mean from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

# cases for checking: 
# mat <- (matrix(c(2, 1, 1, 3, 2, 1, 2, 1, 2), 3, 3, byrow=TRUE))     // take an invertible 3 by 3 matrix
# demo <- makeCacheMatrix(mat)                                        // create object like in OOP 
# demo$get()                                                          // get data within the object
# demo$getInverse()                                                   // get inv, whatever its current value is
# cacheSolve(demo)                                                    // calculate inverse
# cacheSolve(demo)                                                    // get chached inverse
# demo$set(matrix(c(3, 4, 1, 2), 2, 2, byrow=TRUE))                   // reset object to a new set of values
# demo$get()
# demo$getInverse()
# cacheSolve(demo) 
# cacheSolve(demo) 
