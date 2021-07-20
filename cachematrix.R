makeCacheMatrix <- function(x = matrix()){   #forms a matrix that can cache inverse of its values
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL   #modify variable in parent env
        }
        get <- function(){x}      #will return the matrix
        setInverse <- function(inverse){inv <<- inverse}   
        getInverse <- function() {inv}    #gets inverse of the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x,...){      #return the inverse values of variables in matrix x
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)   #assigns inverse of the matrix to inv
        x$setInverse(inv)       #returns the inverse of the matrix
        inv
}
