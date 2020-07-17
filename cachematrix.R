###The following two functions, makeCacheMatrix and cacheSolve, allow us to cache the inverse of a matrix and retrieve that inverse, respectively. 

## The first function (makeCacheMatrix) creates a new matrix. The function contains a list of functions that a)set the value of the matrix to whatever is passed to the function, b) retrieve the value of that matrix, c) set the inverse of the matrix and c) retrieve the inverse of the matrix that's passed to the function. This function ultimately caches the value of the inverse (or the original matrix). To use this function properly, the output of the function must be stored an a new object, which will then be passed to the cacheSolve function (described below).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##After sourcing makeCacheMatrix, store it's output (a list of new functions) in a new object that will be passed to the cacheSolve function described below.
## E.g., newcache <- makeCacheMatrix(inputmatrixhere)

## The second function returns the inverse of the matrix after retrieving it from the cache function created above. Therefore, the object stored by running makeCacheMatrix (described above) is the appropriate input for this function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##After sourcing this function, pass the new object created after running makeCacheMatrix to return the inverse matrix. 
## E.g., cacheSolve(newcache)
