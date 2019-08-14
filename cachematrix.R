## Below are two functions that can create a special object to store a matrix and cache ##its inverse
## The first function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
			inverse<-NULL
			set<-function(y){
				x<<-y
				inv<<-NULL
			}
			get<-function()x
			set_inverse<-function(inverse)inv<<-inverse
			get_inverse<-function()inv
			list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## The second function computes the inverse of the special matrix created above. If the 
## the inverse has been computed and the matrix has not changed, the function below should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invervse<-x$get_inverse()
        if(!is.null(inverse)){
        		message("getting cached data")
        		return (inverse)
        }
        data<-x$get()
        inverse<-solve(data)
        x$set_inverse(inverse)
        inverse
}

