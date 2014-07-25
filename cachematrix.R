## There are 2 functions here:
## 1. MakeMatrixObj: Takes in a matrix and initializes the "Object" with functions and the matrix passed in
## 2. CacheSolve: returns the matrix inverse either from the cache or else gets the new inverse and
##      sets the cache

## Take a matrix and create a new "Object" that is initialized with routines and the data.
##
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse to null the first time it's created
    inv <- NULL
    
    #Set new Matrix and clear the cache.
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    #Get the Matrix
    get <- function() x
    
    #Set the inverse
    setinverse <- function(solve) inv <<- solve
    
    #Get the inverse
    getinverse <- function() inv
    
    #List of functions available
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Taks the matrix Object and returns the inverse either from the cache or computes and saves.
#
cacheSolve <- function(x, ...){
    
    ## Get the current inverse value
    inv <- x$getinverse()
    
    ## If the value is not null, it's in the Cache
    if (!is.null(inv)){
        
        ## Nothing to do. But put up a message to indicate cache was used
        message("getting cached data")
                
    } else {
        ## Not in the Cache.  Put up a message
        message("not cached")
        ## Load a var with the matrix data
        getMatrix <- x$get()
        ## Calc the inverse and store in the cache
        inv <- solve(getMatrix,...)
        x$setinverse(inv)

    }
    ## Return the inverse value
    inv
}

## NOTE: if you uncomment this section, it will show the code works:
##
## n=1000
## my_matrix <- matrix(rnorm(n*n), n, n)
## my_special_matrix <- makeCacheMatrix(my_matrix)
## my_special_matrix$set(my_matrix)
## print(system.time(cacheSolve(my_special_matrix)))
## print(system.time(cacheSolve(my_special_matrix)))


