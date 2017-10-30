## This assigment aims at practicing the use of the scoping rules to preserve state 
## inside a R object.
##
## We create two functions: makeCacheMatrix() and cacheSolve()


## The first function, makeCacheMatrix(), processes an input matrix and stores it + a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                       # Function (can be called with 'object$set(<matrix>)' where object <- makeCacheMatrix())
                cached_x <<- y                     # it generates the matrix and stores it in cache.
                cached_m <<- NULL 
        }
        get <- function() cached_x                 # Function (can be called with 'object$get()' where object <- makeCacheMatrix()) to get 
						   # matrix passed through object$set()

        setCachedMatrix <- function(m) cached_m <<- m    # function to cache the calue of cached_m
        getCachedMatrix <- function() cached_m           # function to retrieve the value of cached_m
        list(set = set, get = get,                       # list containing the 4 functions (all can be called with object$ - e.g. object$get())
             setCachedMatrix = setCachedMatrix, 
             getCachedMatrix = getCachedMatrix)
}





## The next function, cacheSolve(), processes the matrix generated in makeCacheMatrix and returns it as an inversed matrix. It also stores 
## the generated inverse Matrix in the cache environement. The inverse matrix is not generated again if a new call happens: the function first 
## checks in the cache if the matrix is there. So, theoretically, we can call this function n times - it only generate the inverse matrix 
## once (the first time) 



cacheSolve <- function(x, ...) {

        m <- x$getCachedMatrix() 				# Return the inverse matrix of 'x'

        if(!is.null(m)) {                                       # If m is not null (cacheSolve() has been already called)
                message("getting cached data")                  # returns its value + message, otherwise, next code... 
                return(m)
        }
        data <- x$get()                                         # ...Retrieves the matrix
        m <- solve(data)                                        # Process inverse matrix
        x$setCachedMatrix(m)                                    # calls the function to store m to the cache environement
        m
}
