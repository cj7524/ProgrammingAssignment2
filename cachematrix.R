## These two functions when used together will check to see if there is a cached version 
## of a matrix that we are trying to create an inverse of.  If there isn't, it will
## run the inverse.  If there is it will call the cached version of the inverse.
## These two functions will only work with invertible matrices.  Use det(x) to 
## determine if a matrix is invertible (0 is no)

##This reads in a matrix and creates a cached version of an inversed matrix by
##putting it into a list

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inversed_M) inv <<- inversed_M
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


##Determine if cache matrix created or if need to create an inverse
## if there is no cache matrix then it will create one via solve

cacheSolve <- function(x, ...) {
      
      
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setmInverse(inv)
      return(inv) 
      ## Return a matrix that is the inverse of 'x'
}
