# create a function called makeCatcheMatrix
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL     # set inv as NULL so it will hold the value of inverse
    set <- function(y) {  # create a function called set to assign new
        x <<- y    
        inv <<- NULL  # if there is a new matrix, inv reset to NULL
    }
    get <- function() x   # get function returns value of the matrix argument
    
    setinverse <- function(m_inv) inv <<- m_inv  # assigns value of inv in parent environment
    getinverse <- function() inv  # get the value of inv where called
    list(set = set, get = get,
    setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x,...) {
    # return matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message('Getting Cache Data')  # set message for inverse
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$getinverse(inv)
    inv  # print inverse
}
