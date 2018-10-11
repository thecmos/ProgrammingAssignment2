## First, as per the provided example, an attempt to create a matrix function that can cache 
## (from the French cacher, I assume) its inverse.
## Then, also following the example, an attempt to compute the inverse of the matrix, only if not done before.
## Else, just returning its reviously calculated value.
## Note: I had figured out the first line of the first function, initialize a matrix.
## Good luck in your endeavors.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Creating the list that caches the inverse. I would have never figured out this part.
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## If not calculated before, then calculate now the inverse of the matrix. Print it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'. I have followed the example almost to the letter.
  inv <- x$getsolve()
  ## I have to admit that I have not been able to print the previously calculated result. this loop below.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else { 
    ## This part works well, the inverse of the matrix is printed.
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv) 
    inv
  }
}
