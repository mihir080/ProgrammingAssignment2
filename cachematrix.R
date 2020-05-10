## This function will cache the inverse of matrices and show them as required
## This will save computing time

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }

      get <- function() x
      setSolve <- function(solve) m <<- solve
      getSolve <- function() m
      list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## this function allows for us to make a cache matrix and set variables for the same. 

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
## here we check the cache for any stored inverse of the matrix and return the same
## if such data does not exist we will solve the matrix and recall the cache function 