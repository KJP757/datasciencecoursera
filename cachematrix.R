## Program assignment 2, Lexical Scoping
## The objective is to store an inverted matrix such that it can be accessed
## from a cache rather than taking time to repeat the inversion process

## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(p) m <<- p
  getinv <- function() m
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## Compute the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
