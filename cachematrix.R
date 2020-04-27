makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix has matrix (x). 
  inver <- NULL
  ## We have created value inver that we have assigned null to.
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  ## we define set, get, setinverse, getinverse. 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ##list is what is being returned 
}


cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  ##if a value exist for inverit keeps it. But if its null it will do the calculation below. 
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
  ##Returns: the inverse matrix of the initial matrix 
}




makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
