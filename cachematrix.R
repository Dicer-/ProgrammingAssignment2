makeCacheMatrix <- function(x = matrix()) {
## Makes a list that can store an invertible matrix
  i <-NULL
  set <- function(y = matrix()) {
          x<<-y
          i <<-NULL
  }
  get <-function() x
  setinv <- function(inv) i<<-inv
  getinv <- function() i
  
  list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Checks to see if the inverse has been solved first
  i <- x$getinv()
  
  if (!is.null(i)) {
      message("Retrieving Cached Data")
      return (i)
  }
  
  data <-x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
        
