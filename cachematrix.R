## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the function has a set of operation related to a matrix
## set - set the matrix in cache
## get - return the matrix
## setInverse - set/calculate the inverse of teh matrix
## getInverse - get the inverse of teh matrix. 


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    # set the matrix
    set <- function(y) {
       x <<- y
       m <<- NULL
    }

    # get the matrix
    get <- function() x
    
    # set the inverse of the matrix using solve function
    setInverse <- function(solve) m <<- solve

    # get the inverse of the matrix
    getInverse <- function() m

    list( set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse
        )
    
}


## Write a short comment describing this function
## If the inverse of the matrix already exist, just return the result
## If the inverse doesn't exist in cache, 
## calculate the inverse, store it in cache and return the inverse

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

   m <- x$getInverse()
   if (!is.null(m)) {
      message("getting cached data")
      return (m)
   }
   
   data <- x$get()
   m <- solve(data)
   x$setInverse(m)
   m
}
