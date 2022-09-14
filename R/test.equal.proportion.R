## 08/28/2020

##' Test of equal proportions for grass seed data
##'
##' @param x a n-vector of numbers of weed seeds in each small bag
##' @param w a n-vector of weights of each small bag
test.equal.proportion = function(x, w) {
  s = !is.na(x) & !is.na(w) & w>0;
  x = x[s];
  w = w[s];
  
  e = sum(x) * w / sum(w);
  df = length(x) - 1;
  t = sum((x-e)^2/e);
  p = pchisq(t, df = df, lower.tail = FALSE);
  list(statistic=t, df = df, p.value = p);
}

test.test.equal.proportion = function() {
  
  ## Number of seeds
  N = 1e5;
  
  ## Number of weeds
  n = 1e2;
  
  ## Number of small bags
  nb = 10;
  x = numeric(nb);
  y = numeric(nb);
 
  bag = numeric(N);
  bag[1:n] = 1;
  f = sample(1:10, N, replace=TRUE);
  s = split(bag, f);
  for(i in 1:10) {
     x[i]= sum(s[[i]]);
     y[i] = length(s[[i]]);
  }
  
  prop.test(x, y)
  prop.test(x, y*2)
  
  test.equal.proportion(x, y/50)
  test.equal.proportion(x, y/100)
  
}


