## function for inversing matrices


## It take a matrix as an argument and checks if it's already
## been calculated. if so, it gets the inverse from the cache  
## and skip the calculation. Otherwise, it calculates the 
## inverse.


makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
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

#########################################
#usage: mymatrix < - makeCacheMatrix(x)
# cacheSolve(mymatrix)


