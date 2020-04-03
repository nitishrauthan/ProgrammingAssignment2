# This function creates a special "matrix" object that can cache its inverse




CacheMatrix<-function(x=matrix()){                     #defining the argument of the function with matrix
  i<-NULL                                                  #this initializes the inverse matrix(i) to null
  set<-function(a){                                        # define the set function to assign new 
    x <<- y                                                # value of matrix in parent environment                
    i<<-NULL                                               # if there is a new matrix, reset i to NULL
  }
  get<-function() x                                        #define the value of the get function and returns a matrix x
  setinverse<-function(inverse) i<<-inverse                #setinverse function defines i in the parent environment
  getinverse<-function() i                                 #getinverse function gets the value of i where it is called
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve will retrieve the inverse from the cache


cacheinverse<-function(q,...){                          #defining the argument of the cacheinverse function with matrix
  a<-q$getinverse()                                     #getting the inverse of q matrix
  if(!is.null(a))                                       #if the inverse is there then the inverse is not calculated 
  {
    message("getting cached data")                      #then the message is printed
    return(a)                                           #inverse matrix is returned
  }
  #if the matrix is not inverted
  d<-q$get()                                            #get the  matrix
  a<-solve(d,...)                                  #calculating the inverse
  q$setinverse(a)                                       #set the inverse matrix
  return(a)                                             #returning the inverse matrix
}

#testing the code
t<-matrix(1:4,2,2)
a<-CacheMatrix(t)
a$get()
a$getinverse()

cacheinverse(a)

