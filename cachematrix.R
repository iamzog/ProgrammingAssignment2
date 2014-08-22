## This function creates the matrix that is cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculates the inverse or returns previously calculated inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

x <- matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3) #creates matrix
x #shows matrix
solve(x) #shows inverse of matrix
z <- makeCacheMatrix(x) #caches the matrix
cacheSolve(z) #solves inverse which matches solve(x) 
cacheSolve(z) #shows that it doesn't recalculate