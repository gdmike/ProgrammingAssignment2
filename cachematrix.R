## There are 2 functions here:
## 

## Write a short comment describing this function




## Write a short comment describing this function




makecacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...){
    m <- x$getinverse()
    if (!is.null(m)){
        print("getting cached data")
        return()
        #return(m)
    }
    print("not cached")
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    #m
}



