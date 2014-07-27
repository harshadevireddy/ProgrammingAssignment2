## The two function written calculate the inverse of a matrix that is sent in. 
##They are useful to verify whether theinverse is in the cache or if it was calculated previously

## This function prepares the cachematrix which is the input for the next function.
## The output of this Function contains a list with four elements

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
	x<<-y
	m<<-NULL
}
	get<-function() x
	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
	

}


## This function calculates the inverse if it is not calculated peviously
## Otherwise it returns the cached value

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
    m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
