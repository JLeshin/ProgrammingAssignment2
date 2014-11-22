## The first function creates an input that goes into the second function. By 
##first entering a matrix into the first function and then entering the output 
##of the first function into the second function, the second function will 
##output the inverse of the original matrix. If this inverse has not yet been
##computed, the second function will do the computation. If we have already
##computed the inverse of the matrix, the second function will simply print out 
##this inverse by using the "getinverse" function from makeCacheMatrix. 

## This function produces three functions, "get"; "setinverse"; and "getinverse"
##that will be used in the second function. Note: I have omitted the "set" 
##function within makeCacheMatrix because it is not necessary. 

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        
        get <- function() {x}
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list( get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function first checks if the inverse of x has already been computed.
##If so, it prints out the inverse; if not, it computes the inverse and then
##prints it. 

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}

