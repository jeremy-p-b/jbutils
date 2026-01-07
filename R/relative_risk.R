#' Calculate risk ratio and score confidence intervals (based on https://users.stat.ufl.edu/~aa/cda/R/two-sample/R2/ and epitools package)
#'
#' @param x1 Scalar number exposed with outcome
#' @param n1 Scalar number exposed
#' @param x2 Scalar number unexposed with outcome
#' @param n2 Scalar number unexposed
#' @param conflev Confidence level
#'
#' @return A vector with the lb, estimate, and ub
#'
#' @examples
#' binom_exact_ci(15,80, accuracy=0.01)
riskscoreci <- function(x1,n1,x2,n2,conflev)
{
  z =  abs(stats::qnorm((1-conflev)/2))
  if ((x2==0) &&(x1==0)){
    ul = Inf
    ll = 0
  }
  else{
    a1 =  n2*(n2*(n2+n1)*x1+n1*(n2+x1)*(z^2))
    a2 = -n2*(n2*n1*(x2+x1)+2*(n2+n1)*x2*x1+n1*(n2+x2+2*x1)*(z^2))
    a3 = 2*n2*n1*x2*(x2+x1)+(n2+n1)*(x2^2)*x1+n2*n1*(x2+x1)*(z^2)
    a4 = -n1*(x2^2)*(x2+x1)
    b1 = a2/a1
    b2 = a3/a1
    b3 = a4/a1
    c1 = b2-(b1^2)/3
    c2 = b3-b1*b2/3+2*(b1^3)/27
    ceta = acos(sqrt(27)*c2/(2*c1*sqrt(-c1)))
    t1 = -2*sqrt(-c1/3)*cos(pi/3-ceta/3)
    t2 = -2*sqrt(-c1/3)*cos(pi/3+ceta/3)
    t3 = 2*sqrt(-c1/3)*cos(ceta/3)
    p01 = t1-b1/3
    p02 = t2-b1/3
    p03 = t3-b1/3
    p0sum = p01+p02+p03
    p0up = min(p01,p02,p03)
    p0low = p0sum-p0up-max(p01,p02,p03)

    if( (x2==0) && (x1!=0) ){
      ll = (1-(n1-x1)*(1-p0low)/(x2+n1-(n2+n1)*p0low))/p0low
      ul = Inf
    }
    else if( (x2!=n2) && (x1==0)){
      ul = (1-(n1-x1)*(1-p0up)/(x2+n1-(n2+n1)*p0up))/p0up
      ll = 0
    }
    else if( (x2==n2) && (x1==n1)){
      ul = (n2+z^2)/n2
      ll =  n1/(n1+z^2)
    }
    else if( (x1==n1) || (x2==n2) ){
      if((x2==n2) && (x1==0)) { ll = 0 }
      if((x2==n2) && (x1!=0)) {
        phat1  = x2/n2
        phat2  =  x1/n1
        phihat = phat2/phat1
        phil = 0.95*phihat
        chi2 = 0
        while (chi2 <= z){
          a = (n2+n1)*phil
          b = -((x2+n1)*phil+x1+n2)
          c = x2+x1
          p1hat = (-b-sqrt(b^2-4*a*c))/(2*a)
          p2hat = p1hat*phil
          q2hat = 1-p2hat
          var = (n2*n1*p2hat)/(n1*(phil-p2hat)+n2*q2hat)
          chi2 = ((x1-n1*p2hat)/q2hat)/sqrt(var)
          ll = phil
          phil = ll/1.0001}}
      i = x2
      j = x1
      ni = n2
      nj = n1
      if( x1==n1 ){
        i = x1
        j = x2
        ni = n1
        nj = n2
      }
      phat1  = i/ni
      phat2  =  j/nj
      phihat = phat2/phat1
      phiu = 1.1*phihat
      if((x2==n2) && (x1==0)) {
        if(n2<100) {phiu = .01}
        else {phiu=0.001}
      }
      chi1 = 0
      while (chi1 >= -z){
        a = (ni+nj)*phiu
        b = -((i+nj)*phiu+j+ni)
        c = i+j
        p1hat = (-b-sqrt(b^2-4*a*c))/(2*a)
        p2hat = p1hat*phiu
        q2hat = 1-p2hat
        var = (ni*nj*p2hat)/(nj*(phiu-p2hat)+ni*q2hat)
        chi1  = ((j-nj*p2hat)/q2hat)/sqrt(var)
        phiu1 = phiu
        phiu = 1.0001*phiu1
      }

      if(x1==n1) {
        ul = (1-(n1-x1)*(1-p0up)/(x2+n1-(n2+n1)*p0up))/p0up
        ll = 1/phiu1
      }
      else{ ul = phiu1}
    }

    else{
      ul = (1-(n1-x1)*(1-p0up)/(x2+n1-(n2+n1)*p0up))/p0up
      ll = (1-(n1-x1)*(1-p0low)/(x2+n1-(n2+n1)*p0low))/p0low
    }
  }
  est <- (x1/n1)/(x2/n2)
  c(ll,est, ul)
}

#' Calculate upper bound of risk ratio score CI
#'
#' @param x1 Scalar number exposed with outcome
#' @param n1 Scalar number exposed
#' @param x2 Scalar number unexposed with outcome
#' @param n2 Scalar number unexposed
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' rr_score_upper(15,80,30,160)
rr_score_upper <- function(x1, n1, x2, n2) {
  return(mapply(function(x1, n1, x2, n2) riskscoreci(x1,n1,x2,n2, conflev=0.95)[3], x1, n1, x2, n2))
}

#' Calculate lower bound of risk ratio score CI
#'
#' @param x1 Scalar number exposed with outcome
#' @param n1 Scalar number exposed
#' @param x2 Scalar number unexposed with outcome
#' @param n2 Scalar number unexposed
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' rr_score_lower(15,80,30,160)
rr_score_lower <- function(x1, n1, x2, n2) {
  return(mapply(function(x1, n1, x2, n2) riskscoreci(x1,n1,x2,n2, conflev=0.95)[1], x1, n1, x2, n2))
}

#' Calculate string confidence interval for risk ratio score CI
#'
#' @param x1 Scalar number exposed with outcome
#' @param n1 Scalar number exposed
#' @param x2 Scalar number unexposed with outcome
#' @param n2 Scalar number unexposed
#' @param accuracy Accuracy of rounding
#' @param ci_only Boolean of whether to report CI only without mean
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' rr_score_ci(3,50, 6, 100)
rr_score_ci <- function(x1,n1, x2, n2, accuracy=0.01, ci_only=FALSE) {
  estimated_rr <- (x1/n1)/(x2/n2)
  estimated_lb <- rr_score_lower(x1,n1,x2,n2)
  estimated_ub <- rr_score_upper(x1,n1,x2,n2)
  if (ci_only) {
    ci_string <- paste0(scales::number(estimated_lb, accuracy=accuracy), "-",
                        scales::number(estimated_ub, accuracy=accuracy))
  } else {
    ci_string <- paste0(scales::number(estimated_rr, accuracy=accuracy)," (",
                        scales::number(estimated_lb, accuracy=accuracy), "-",
                        scales::number(estimated_ub, accuracy=accuracy),")")
  }
  return(ci_string)
}

#' Calculate variance of log risk ratio (following Jewell 2003)
#' @param x1 number of cases in group 1
#' @param n1 number in group 1
#' @param x2 number of cases in group 2
#' @param n2 number in group 2
#'
#' @return A numerical output of estimated variance
#' @export
#'
#' @examples
#' calculate_fe_pooled_rr(38,398,37,514,29,314,18,215)
var_log_rr <- function(x1, n1, x2, n2) {
  varest <-((n1-x1)/(x1*n1)) + ((n2-x2)/(x2*n2))
  return(varest)
}
