## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a matrix, that contains the its inverse by:
## 1. incoporates a function that sets the value of the matrix by looking outside its environment
## 2. incorporates a function that gets the value of the matrix
## 3. incorporates a function that sets the value of the inverse of the matrix
## 4. incorporates a function that gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
        m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of x by:
## 1. checking to see if the inverse has already been calculated
## 2. if the inverse has been already calculated it gets the inverse from the cache and skips the computation
## 3. if the inverse is not in the cache it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                 m <- x$getinverse()
                        if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
