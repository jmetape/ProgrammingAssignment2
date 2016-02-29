##submission for week 3 assignment

##  sets  matrix, get the matrix, set  inverse, get  inverse, calls cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invert) inv <<- invert
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## checks if inverse exists. yes, gets from cache, no, set it and put in cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  #checks to see if inverse has been calculated
  if(!is.null(inv)){
    print("Pulling info...")
    return(inv)
  }
  
  data <- x$get()
  inv = solve(data, ...)
  return(inv)
}