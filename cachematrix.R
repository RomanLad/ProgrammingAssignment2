## Function sets the value of the matrix and stores inversed matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## m is NULL 
        set <- function(y) { ## Set function created, which sets the value of the matrix 
                x <<- y ## getting the value from the cash and sets the value 
                m <<- NULL ##if matrix exists sets value to NULL
        }## list of functions to put matrix in cache
        get <- function() x ## get the value of the matrix
        setinverse <- function(invesrse) m <<- invesrse ## stores inversed value of the matrix
        getinverse <- function() m ## store inversed matrix
        list(set = set, get = get, ## list of functions to be returned to the above 
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function returns and computes inversed matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()## get the matrix if it is stored in cache
        if(!is.null(m)) {## if inverse matrix stored proceed to cache
                message("getting cached data")## prints message
                return(m)#return the inversed matrix
        }
        data <- x$get()## if not in cache, solves the matrix
        m <- solve(data, ...)## matrix inversed
        x$setinverse(m)## inversed matrix to cache 
        m ## casched matrix is printed
}


