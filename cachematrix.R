##These two functions will create a special matrix that has the ability to cache its own inverse,
##and actually calculate the inverse of the matrix, respectively.

##This takes in a generic matrix and constructs a "special" matrix
##that can cache its own inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This takes in one of the "special" matrices that are created by
##makeCacheMatrix and then either retrieves the inverse from the cache
##if it exists there already and calculate it otherwise.
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setInverse(i)
	i
}
