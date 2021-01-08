# Data generation. ------------------------------------------------------------
seed = 1848
set.seed(seed)
nobs = 10
xx1 = runif(nobs,-1,1)
xx2 = runif(nobs,-1,1)
ws = rep(1,nobs)
eps = rnorm(nobs)
yy = (1 + 2*xx1 - 0.5*xx2 + eps) >= 0

# Algorithm attempt. ----------------------------------------------------------

# We construct an arrangment by using a double-edged list. In addition to
# the ordinary geometric information, we sequentially update weights for the
# faces.

# If a line doens't intersect a given face, we'll still have to decide wether
# it updates it with weights or not. Hence we'll need some way of calculating 
# its "aboveness" or "belowness". This can be done by letting one vertex be 
# designated as "special" for each face. It's fast to calculate its value and
# check if it's above or below our line. 

# In addition, we will have to distinguish between real faces and fake faces.
# Fake faces are not needed in our end product, but might be required during
# its construction. This will make the graph look unlike an mash-up of lines.

# Adds a limit to the search box.
limit = 20

add_region = function(regions,H){
  
}