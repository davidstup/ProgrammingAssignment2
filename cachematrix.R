## davidstup version
## 01/2/2015

## makeCacheMatrix creates a list, in order to create the matrix

makeCacheMatrix <- function(x = matrix()) 
## x will be the matrix input to this function
  {
  m <- NULL
  set <- function(y) 
## assign values using <<- to more than the current environment
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x

## solve creates an inverse of the matrix input
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m

## create a list of the matrix creation
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve funciton returns the created matrix, or the cached version
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
## if m is already assigned (not null) from the makeCacheMatrix function, use the value (cache) instead of recreating
  if(!is.null(m)) 
  {
    message("get cached matrix")
    return(m)
  }
## use the get function input from the makeCacheMatrix function
  data <- x$get()
  m <- solve(data, ...)
## solve / create the inverse the matrix input
  x$setsolve(m)
  m
}
