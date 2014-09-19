##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
        x<<-y
        m<<-NULL
        }
        ##sets the "matrix" object
        get<-function() x
        setMatrix<-function(solve) m<<- solve
        ##caching the "matrix" object
        getMatrix<-function() m
        list(set=set, get=get,
             setMatrix=setMatrix,
             getMatrix=getMatrix)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
#should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
		#checking for "cached data (matrix)"
        m<-x$getMatrix()
        if(!is.null(m)){
                message("acquiring cached data, please hold...")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setMatrix(m)
        m
}