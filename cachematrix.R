# makes a Special list object to acts as a specialized Matrix with Cache capability
# The list includes a getter and setter for both the Matrix and the inverse Matrix Cache, and a 
# bool function indicating wether the inverse is cached
# If the Matrix changes, the cache will be cleaned (Set to Null)
# Input parameter -- x : Matrix
# Output Value: list (Specialized Matrix)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(computation) {
    matrix <<- computation
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  isCached <- function() !is.null(inverse)
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       isCached = isCached
       )
}


# Accepts a inversible special Matrix object
# If the Matrix inverse is cached, this value won't be computed and returned 
# Otherwise the inverse will be computed using the solve function of R
# Input parameter -- x : list(specialized Matrix), ... (passed to solve)
# Output parameter -- inverse : Matrix
# Note, the Matrix has to be inversible, there are no built in Exceptions or checks
cacheSolve <- function(x, ...) {  
  if (x$isCached()) {
    message("The computation is cached, and won't be re-computed.") 
    inverse <<- x$getInverse()
  }  
  else {        
    inverse <<- solve(x$get(), ...)
    x$setInverse(inverse)
  }  
  inverse #I prefer one return (exit) point
}
