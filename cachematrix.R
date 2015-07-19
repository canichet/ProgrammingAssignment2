## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	dim(x) = c(length(x)/2,length(x)/2)
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setCache <- function(inverseMat) m <<- inverseMat
	getCache <- function() m
	list(set = set, get = get,
		setCache = setCache,
		getCache = getCache)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getCache()
	if(!is.null(m)) {
		message('getting from cache')
		return(m)

	}
	data <- x$get()
	m <- solve(data, ...)
	x$setCache(m)
	m
}


}
