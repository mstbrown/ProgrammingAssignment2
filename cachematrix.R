## This file is completed in fufillment of R Programming 
# Assignment 2. The file has two functions. Together the two
# functions display an inverted matirx, but the main 
# benifit of this file is that the inverted matrix to 
# display can be pulled from cached memory if it had already 
# been computed.

## The first function (makeCacheMatrix) serves the main
# purpose of setting the value of a matrix. This allows
# the second function to pull that set value out of cached
# memory. This saves computation time versus recomputing 
# the "solve()" function (which inverts a matrix) again. The
# first three lines of code create a matrix of NULL values.

makeCacheMatrix <- function(x = matrix()) {
    nullmatrix <- list()
    length(nullmatrix) <- length(matrix)
    dim(nullmatrix) <- c(length,length)
        inverse<- nullmatrix
            set <- function(y) {
            x <<- y
            inverse <<- nullmatrix
    }
    get <- function() x
    setsolve <- function(solve) inverse <<- solve
    getsolve <- function() inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



## The second function (cacheSolve) displays the inverted 
# matrix. This matrix will be inverted on the spot or 
# displayed from the cached memory. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse<-x$getsolve()
    if(!is.null(inverse)){
        message("cached data from memory will be displayed")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setsolve(inverse)
    inverse
    }
        #(test notes) solution<-solve(x)
        #solution
}

##A sample test matrix (uncomment the following two lines):
#amatrix<-matrix(c(1,2,3,4),nrow=2,ncol=2,)
#cacheSolve(amatrix)
