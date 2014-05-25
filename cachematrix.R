## This function creates a special "matrix" object that
## can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    # initial inverse matrix (dim => 1-by-1) 
    # containing NULL
    inv <- NULL
    
    # set the matrix	
    set <- function(y) {
        # assign 'x' as the matrix 'y' 
        x <<- y
        # reset the matrix to intial state
        inv <<- NULL
    }
    
    # get the matrix 'x'
    get <- function(){
        # display the current inverse matrix
        x
    }
    
    # set the inverse matrix
    setinv <- function(inv_matrix){
        # manually set the inverse matrix 'm'
        inv <<- inv_matrix
    }
    
    # get the inverse matrix 'm'
    getinv <- function(){
        inv
    }
    
    # set the name of each defined function
    # so that it can be called using the object
    # of this function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## This function computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix'.  If the inverse has already been
## calculated(and the matrix has not changed), then the cache solve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # check if the object 'x already holds
    # cached inverse matrix
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # if there was no cached data
    # obtain the matrix from the object
    data <- x$get()
    
    # compute the inverse matrix 
    inv <- solve(data, ...)
    
    # store the computed inverse matrix
    # in the object
    x$setinv(inv)
    
    # display the computed inverse matrix
    message("completed operation and cached inversed matrix")
    inv
}

