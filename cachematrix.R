## Put comments here that give an overall description of what your
## functions do

## This fuction caches the inverse of a matrix, as well as creates 4 fuctions

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getInverse <- function() i
  setInverse <- function(setInverse) i <<- setInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This fuction calculates the inverse of the matrix returned by the fuction above. If the inverse is cached,
## simply return the cached value.

cacheSolve <- function(x, ...) 
{
  i <- x$getInverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
