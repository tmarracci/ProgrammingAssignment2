## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function will create a matrix and prepare for calculating
## and caching it's inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## setup a new matrix and reset the cache
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get this matrix
    get <- function() x
    
    ## return the cached inverse (it might be null)
    getinverse <- function() m
    
    ## set the cached inverse 
    setinverse <- function(inverse) m <<- inverse
    
    ## return a list that contains these methods
    list(set = set, get = get, 
         getinverse = getinverse,
         setinverse = setinverse)
}


## Write a short comment describing this function
## get the inverse of the makeCacheMatrix x
## and update/use the cache as needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    
    ## calculate the matrix inverse and set the cache
    m <- solve(x$get())
    x$setinverse(m)
    
    m
}
