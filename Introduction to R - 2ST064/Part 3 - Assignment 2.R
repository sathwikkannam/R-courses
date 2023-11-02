#####
# Introduction to R - Assignment 2
#####

# Remember you can test your answers at https://statistics-umu.shinyapps.io/introduction-to-r/
# (testing is not the same as a submission, which should be in Canvas)

#In this assignment we will be using R and its functions to calculate distances between 
#points. You can think of it as taking the distance between points in a map (like google maps giving you the distance to the closest restaurants!). 
#Although the earth is a sphere, treating it as a plane (a map) and 
#then taking euclidean distances is a good approximation at short scales.

#Most of the calculations in this assignment will be based on Pythagora's theorem. 
#If you want a quick refresher on how distances between points work and Pythagora's theorem, 
#you can read a bit more here: https://www.khanacademy.org/math/geometry/hs-geo-analytic-geometry/hs-geo-distance-and-midpoints/a/distance-formula

### Start of the Assignment

#Q1
# Create a function that returns the square root of the sum of squares of two numbers. 
# Example: it takes a=4, and b=3 and should return sqrt(4*4 + 3*3) = sqrt(16 + 9) = sqrt(25) = 5.

Root_sum_squares <- function(a, b) {
  return(sqrt(a^2 + b^2))
}

#Q2
#a)
# Create a function that takes two pairs of numbers p1 = (x1, y1) and p2 = (x2, y2) (each representing a point on the plane), then returns the distance between them.
# In addition, you should use the previous function within this one.
# Example: the distance between (0,0) and (3,4) is equal to sqrt( (3-0)^2 + (4-0)^2 ) = Root_sum_squares(3-0, 4-0) =5

#b)
# Additionally, c(3,0) and c(0,4) for p1 and p2 should be default input values for the function.

#c)
# Ensure the function returns an error if any of p1 or p2 has a length other than 2.
# !important: Do not change the error message in the example below (keep the stop and the message after, alter only inside the if()).
Distance <- function(p1=c(3, 0), p2=c(0, 4)) {
  #alter the if() below for c)
  #Note that currently this function always creates an error, including if you upload it to the server for testing other answers
  if (length(p1) != 2 | length(p2) != 2) {
    stop('The length of either p1 or p2 is not two')
  }
  return(Root_sum_squares(p1[1] - p2[1], p1[2] - p2[2]))
}

#Q3
# Lets define two data frames with two columns in this way:
m1 <- data.frame(x = c(0, 0, 12), y = c(0, 3, 0))
m2 <- data.frame(x = c(0, 0, 4), y = c(0, 5, 0))
# Each data frame is a set of two dimensional vectors which contains its position in x (horizontal) and in y (vertical), 
# for each of such points in the first data frame we want to find its distance to all the points in second data frame.
# Example: say `m1` contains `v1`, `v2`, and `v3`, so `v1 = c(0,0)`,`v2 = c(0,3)`, and `v1 = c(12,0)` and 
# `m2` contains `v4`,`v5`, and `v6`. We want the output of the function to contain the distance of all possible pairs for vectors in the first data frame and the second one, like:
#   `Distance(v1,v4), Distance(v1,v5), Distance(v1,v6)`
#   `Distance(v2,v4), Distance(v2,v5), Distance(v2,v6)`
#   `Distance(v3,v4), Distance(v3,v5), Distance(v3,v6)`
# Write a function that takes two data frames each with two columns and calculates distances of all pairs of two-dimensional vectors from the first data frame and the second one. The output should be a matrix.

Distance_matrix <- function(m, n) {
  output <- matrix(0, nrow = nrow(m), ncol = nrow(n))
  # Alter the loops and the objects being assigned to the Distance function:
  for (i in 1:nrow(m)) {
    for (j in 1:nrow(n)) {
      output[i,j] <- Distance(as.numeric(m[i,]), as.numeric(n[j,]))
    }
  }

  return(output)
}

#Q4
# Use the function Distance_matrix to figure out if three input points lay on one line or not.
# Hint: Three points p1,p2, and p3 lay on one line if and only if the distance between two of them is equal to the sum of distances of those two point with the third point:
# Example: let p1=c(0,0), p2=c(0,1), and p3=c(0,2). Then,  Distance(p1,p2) = 1, Distance(p1,p3) = 2, and Distance(p3,p2) = 1. So we can conclude that they lay on one line as,
# Distance(p1,p3) = Distance(p1,p2) + Distance(p3,p2), 2=1+1.
# In another formulation, it can be expressed as these three points do not lay on one line if the summation of each two length is greater than the third distance.
# The function should return TRUE if p1, p2 and p3 are in a line and FALSE if not.
is_one_line <- function(p1, p2, p3) {
  #The following if check is just for the cases when two points are in the same place.
  #You should just keep it as it is
  if (all(p1 == p2) | all(p2 == p3) | all(p3 == p1)) {
    return(TRUE)

  }

  m <- is_one_line(p1, p2, p3) #you need to put something in place of ?. This currently creates an error
  d <- Distance_matrix(m, m)
  d1 <- d[1, 2]
  d2 <- d[1, 3] #fill it here too
  d3 <- d[2, 3]

  if (d1 + d2 > d3 & d3 + d2 > d1 & d1 + d3 > d2) {
    return(FALSE)
  }
  return(TRUE)
}

#Q5 
# Utilizing the Distance and Apply functions, write write a function, 
# is_one_line_2, that is the same as the one described in Q4:
is_one_line_2 <- function(p1, p2, p3) {
  #The following if check is just for the cases when two points are in the same place.
  #You should just keep it as it is
  if (all(p1 == p2) | all(p2 == p3) | all(p3 == p1)) {
    return(TRUE)
  }
  l1 <- list(p1, p1, p3)
  l2 <- list(p2, p3, p2)
  d <- mapply(Distance(l1, l2)) #fill in this mapply function using the Distance function and other objects

  if (max(d) == sum(setdiff(d, max(d)))) {
    return(TRUE)
  }
  return(FALSE)
}

