CV <-
function(x)
{
  n = length(x)
  v = ((n-1)/n)*var(x)
  cv = sqrt(v)/abs(mean(x))
  return(cv)
}
