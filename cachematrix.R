## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        tm <- NULL 
        set <- function(m){
                x <<- m
                m <<- NULL
        }
        get <- function() x
        settm <- function(transposed) tm <<- transposed
        gettm <- function() tm
        list(set = set, 
             get = get,
             settm = settm,
             gettm = gettm)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        tm <- x$gettm()
        print(tm)
        if(!is.null(tm)){
                message("getting cached data")
                return(tm)
        }
        data <- x$get()
        tm <- t(data)
        x$settm(tm)
        return(tm)
}
