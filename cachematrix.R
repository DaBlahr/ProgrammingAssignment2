## A method to wrap a matrix so that its inverse may be conveniently cached.
## First use makeCacheMatrix to turn a base matrix into its wrapped form,
## then use use cacheSolve to get its inverse. If the inverse has already
## been calculated, time will be saved by grabbing the cache.

## makeCacheMatrix: wraps a basic matrix in a special structure
##			  that can cache its inverse and has getter
##			  and setter functions

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setcache <- function(newcache) inverse <<- newcache
	getcache <- function() inverse
	list(set = set, get = get,
		setcache = setcache,
		getcache = getcache)
}


## cacheSolve: used on an x of the type returned by makeCacheMatrix,
##		   this returns the inverse of the basic matrix it wraps

cacheSolve <- function(x, ...) {
	inverse <- x$getcache()
	if(!is.null(inverse)) {
            message("getting cached data")
      	return(inverse)
      }

      mat <- x$get()
      inverse  <- solve(mat)
      x$setcache(inverse)
      inverse 
}