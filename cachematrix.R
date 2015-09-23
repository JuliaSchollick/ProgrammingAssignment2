makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}#defines a set of functions get,set,setinv and getinv, which will be used to 'get' or 
#'set' the vector stored in the main function; or store the value of the input in a 
#variable m into the main function makeCacheMatrix (setinv) and return it (getinv).

cacheSolve <- function(a, ...) {
        m <- a$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- a$get()
        m <- solve(data, ...)
        a$setinv(m)
        m
}#a is the value of makeCacheMatrix(x), returns a matrix that is the inverse of 'a'. 
#If the inverse has already been stored by setinv, it returns this value, without 
#calculating anything further.
