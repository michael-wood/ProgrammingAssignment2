#
# makeCacheMatrix:  outputs a list of functions to get the inverse of a matrix
# cacheSolve:       returns the cached or newly solved inverse

# makeCacheMatrix:
#	INPUTS |  a square matrix which is assumed to be inversable
#	OUTPUTS |  a list (cm) containing the functions:
#			setmat - changes the input matrix
#			getmat - returns the input matrix
#			setinv - uses the solve function to find the invers
#			getinv - returns the matrix inverse
#
#			functions from the list are referenced: cm$function

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	setmat <- function(y) {
                x <<- y
                minv <<- NULL
        }
	getmat <- function() x
	setinv <- function(solve) minv <<- solve
	getinv <- function() minv
	list(set = setmat, get = getmat,
             setinv = setinv,
             getinv = getinv)

}


# cacheSolve:
#	INPUTS |  the list vector created by the makeCacheMatrix function
#	OUTPUTS |  the cached or newly solved inverse of the input matrix

cacheSolve <- function(x, ...) {
	minv <- x$getinv()
	if(!is.null(minv)) {
                message("Retrieving data from cache")
                return(minv)
        }
	mraw <- x$get()
	minv <- solve(mraw)
	x$setinv(minv)
	minv
        ## Return a matrix that is the inverse of 'x'
}

