## Put comments here that give an overall description of what your
##My functions produce and invert a complex matrix and store this object into the environment, separately from the parent environment. This prevents repetition of the computation
##should the inverted matrix be involved in further computations, as we can retreive the inverted matrix from the cache environment, saving time. 
## functions do

## Write a short comment describing this function
##The makeCacheMatrix() function produces an object (the matrix in set(), retrieved in get(), first set to NULL at the start of a new function) and performs a manipulation 
##(i.e. solves to find the inverse setmatrix, then using the <<- operator to store this into the cache environment that is retrieved by getmatrix). 
##set() will store the value of m that has been specified from the new environment in subsequent computations. These components are then put into a list form that can be 
##analysed in the cachesolve function using the $ operator.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
##The cacheSolve function takes the value stored in getmatrix, asks whether this value is present in a cache. If m
##only exists in the parent environment, then a separate inverse calculation will be performed and this will be returned to the
##cache for use in setmatrix. If m exists in a cache (set function has been employed) then it will return the cache'd value.

cacheSolve <- function(x, ...) {
         m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return((m))
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

MATRIX <- makeCacheMatrix(matrix(c(6, 8, 2, 4), nrow = 2, ncol = 2 ))
cacheSolve(MATRIX)
cacheSolve(MATRIX)
