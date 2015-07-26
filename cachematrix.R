## Put comments here that give an overall description of what your

## functions do



## Write a short comment describing this function



makeCacheMatrix <- function(x = numeric()) {
  copy_x <- x
  prev <- 0
  dim(x) = c(length(x)/2,length(x)/2)
  m <- NULL
  set <- function(y) {
    dim(y) = c(length(y)/2,length(y)/2)
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(inverseMat) {
    print ('inside set cache')
    m <<- inverseMat
  }
  getCache <- function() {
    if (all(prev == copy_x)) {
      print ('inside if of getcache')
      return (m)
    }
    else
    {
      print ('the matrix is different')
      prev <<- copy_x
      return (NULL)
    }
  }
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
