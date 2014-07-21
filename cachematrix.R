## This group of functions takes a matrix and returns its inverse.  If that inverse has already
## been calculated then it does not recompute such inverse, but, rather, returns the previously
## calculated inverse


## Sets up the functions used in cacheSolve for determining if already calculated
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Return a matrix that is the inverse of x; returns cached data if has already
## been previously called with same input

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
