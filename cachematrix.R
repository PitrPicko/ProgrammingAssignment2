## Put comments here that give an overall description of what your functions do

##makeCacheMatrix (mCM) creates special type of variable (list) that is capable of capturing own value as well as its inverse
##cacheSolver is function that checks whether inverse is already computed.
        ## IF YES --> prints computed value
        ## IF NO  --> calculates inverse, and assign it in (mCM)-inverse list

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # when creating new (mCM) inverse is not computed yet
        set <- function(y) {  #set function allows to change value of (mCM)-source
                x <<- y
                inv <<- NULL  #reset inverse as not computed, as original matrix changed
        }
        get <- function() x   #returns value of (mCM)-source
        setinv <- function(i) inv <<- i  #assigns externally computed inverse to (mCM)-inverse
        getinv <- function() inv  #returns NULL if inverse is not comuted yet or inverse if it is computed
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        inv<-x$getinv()  #gets inverse of x if it is stored in (mCM), NULL otherwise
        if(!is.null(inv)){  #if inverse is computed
        
        #for testing I recomend to comment following line
                message("getting cached")  
        #################################################        
                return(inv)  #return computed inverse and breaks
        }
        data<-x$get  
        inv<-solve(data,...)  #compute inverse
        
        x$setinv(inv)  #store inverse into (mCM)
        inv             # return inverse
}
