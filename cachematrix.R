## When having to create inverse of a matrix , 
## which repeately can happen on the same variable.
## This file provides functions that has the capability to cache the result of the s
## so when the cacheSolve is called the second time it returns the cached result

## to use create an instance with the makeCacheMatrix with the matrix you want to use
## then call cacheSolve for the results, one or more times.

## Create a complex object, 
## that is capable of storing the cached version
makeCacheMatrix <- function(x = matrix()) {
	## initialize the solve_cache variable
      solve_cache <- NULL
	  ## setting new data invalidates the cached variable
        set <- function(y) {
                x <<- y
                solve_cache <<- NULL
        }
		##return the matrix data
        get <- function() x
		##set the cache
        setSolve <- function(cache) solve_cache <<- cache
		##get the cache variable
        getSolve <- function() solve_cache
		
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Return a matrix that is the inverse of 'x'
## x is an instance of the makeCacheMatrix
cacheSolve <- function(x, ...) {
        ##get the solve_cache variable
		  solve_cache <- x$getSolve()
		## Check if there is a cached version
        if(!is.null(solve_cache)) {
                message("getting cached data")
				##return the cached version
                return(solve_cache)
        }
		##get the data
        data <- x$get()
		## calculate the inverse of the matrix
        solve_cache <- solve(data, ...)
		## put the result in the cache
        x$setSolve(solve_cache)
		##return the inverse matrix
        solve_cache
}
