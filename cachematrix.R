## below are functions that store and cache the inverse of a matrix, and return it

## set and store functions(get,setinverse,getinverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                       ## set a variable m
  set <- function(y)                              ## store vaule 
  {
    x <<- y                                       ## set y to x under set enviornment
    m <<- NULL                                    ## set m vaule under set enviornment
  }
  get <- function() x                             ## take vaule from the function above
  setinverse <- function(inverse) m <<- inverse   ## give the value of inverse to m
  getinverse <- function() m                ##get the value of m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## take the value of makeCacheMatrix function value and calculate the inverse matrix

cacheSolve <- function(x,A) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                             ##get the value of x and assign it to m
  if(!is.null(m))
  {message("getting chached data")
  return(m)}                                  ##return inverse matrix if it has been calculated before
  data <- x$get()
  data <- solve(A)                            ## calculate inverser matrix and return
  m <- data
  x$setinverse(m)                ##return the value of m to makeCacheMatrix function enviornment
  m
}
