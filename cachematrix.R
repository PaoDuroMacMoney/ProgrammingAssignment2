## This function append a list of funtions to a matrix object making possible for it to be cached.
## It appends a list of functions as follows:
## set : sets and can be used to change matrix which inverse will be cached
## get : get last Setted Matrix which inverse will be cached
## setinverse : sets inverse in cache
## getinverse : get inverse from cache, NULL if none setted

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
			x <<- y
			i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Wraps cache control and inverse call

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
