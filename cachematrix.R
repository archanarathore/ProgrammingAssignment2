## The function takes inversible (assumed) matrix x as input
## and returns a list of functions (set, get, setinverse and getinverse)

## Function set - sets the value of matrix (x)
## Function get - returns the value of matrix x (if not set, should return NULL)
## Function setInverse - Inverses the matrix (assuming it is not singular) to variable m
## Function getInverse - returns the value in variable m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  setInverse <- function(y)
  {
    m <<- solve(y)
  }
  get <- function() x
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function checks if a matrix is already inversed and stored in m
## if Inverse was already calculated, it returns the cached Inverse otherwise calls setInverse to calculate the Inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m))
  {
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
