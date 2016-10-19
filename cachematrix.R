## gives the Inverse of a matrix by computing 
## or caching it if solved befor

## Can store matrix and inverse of matrix

makeCacheMatrix <- function(x = matrix(), inverse=NULL) {
        m<-NULL
        set<-function(y=matrix()){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## checks if Inverse of Matrix has been caculated befor, if not computes and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <-x$get ()
        m<-solve(data,...)
        x$setinverse(m)
        m
}

