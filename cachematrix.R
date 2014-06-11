## This script will perform the matrix transposing         ##
## with caching which can save transposed matrix into      ##
## cache. When performming repeated process, cached result ##
## Can be readed directly instead of recalculating again   ##
## to reducing computational overhead.                     ##    



makeCacheMatrix <- function(x = matrix()) {
        ## This function makeCachMatrix create 
        ## a specal matrix which can save result into cache
        
        
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



cacheSolve <- function(x, ...) {
        ## This function cacheSolve can perform transposing 
        ## process and save it into caches
        ## If the result has been in the caches, then return 
        ## the result directlly. 
        
        
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
 