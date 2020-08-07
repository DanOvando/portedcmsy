#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix schaefer(NumericVector startbt,
                       double ri,
                       NumericVector ct,
                       NumericVector intbio,
                       NumericVector endbio,
                       int i,
                       int nyr,
                       int ni,
                       double ki,
                       double sigR,
                       double duncert,
                       int int_yr_i,
                       int end_yr) {

  int nsbt = startbt.length();


  NumericVector  dep(nyr + 1);

  NumericMatrix inmemorytable(nsbt * ni, 4 + nyr + 1);

  NumericVector inmemorytablerow(4 + nyr + 1);

  // NumericMatrix inmemorytable(nsbt, 4);


int  counter = 0;

bool crashed = 0;

  for (int nj = 0; nj < nsbt; nj++){ // loop over initial depletions

    NumericVector  bt(nyr + 1);

    double j = startbt(nj); // set initial biomass, including 0.1 process error to stay within bounds

    bt(0) = j * ki * exp(Rf_rnorm(0, 0.1 * sigR)); // ## set biomass in first year

    for (int re = 0; re < ni; re++)   {   // repeat time loops ni times

      int tt = 0;

      crashed = 0;

      for (int t = 0; t < nyr; t++)  {       // for all years in the time series

        double xt = Rf_rnorm(0, sigR); //# set new process error for every year

        double zlog_sd = sqrt(log(1 + pow(duncert,2)));

        double zt = exp(Rf_rnorm(0, zlog_sd)); //# model the catch error as a log normal distribution.


        // calculate biomass as function of previous year's biomass plus surplus production minus catch


        if ((bt(t) / ki) >= 0.25){

          bt(t + 1) =  bt(t) + ri * bt(t) * (1 - bt(t) / ki) * exp(xt) - ct(t) * zt;

        } else {

          bt(t + 1) =  bt(t) + (4 * bt(t) / ki) * ri * bt(t) *
            (1 - bt(t) / ki) * exp(xt) - ct(t) * zt;


        } //  assuming reduced r at B/k < 0.25

          // if biomass < 0.01 k, discard r-k-startbt combination

          tt = t + 1;
        if (bt(t + 1) < (0.01 * ki)) {
          crashed = 1;
          break;
        } //# stop looping through years, go to next upper level
//# intermediate year check
          if ((t + 1) == (int_yr_i - 1) &&
              (bt(t + 1) > (intbio(1) * ki) || bt(t + 1) < (intbio(0) * ki))) {
            crashed = 1;
            break;
          }


      } // close t loop


    // # if loop was broken or last biomass falls outside of expected ranges
    // # do not store results, go directly to next startbt


    if ((bt(end_yr) <= (endbio(1) * ki) &&
        bt(end_yr) >= (endbio(0) * ki)) && crashed == 0) {

      dep = bt / ki;

      NumericVector inmemorytablerow(4 + nyr + 1);

      IntegerVector inds = seq_len(4 + nyr  + 1) - 1;

      inmemorytablerow(0) = i;

      inmemorytablerow(1) = j;

      inmemorytablerow(2) = ri;

      inmemorytablerow(3) = ki;

      for (int l = 0; l < (nyr + 1); l++){

        inmemorytablerow(4 + l) = dep(l);

      }

      inmemorytable(counter,_) = inmemorytablerow;

       counter += 1;

    }  // close if made it to the end and if end is within bounds


  } // close re loop

  } // } # end of nj-loop of initial biomasses

  inmemorytable = inmemorytable(Range(0, counter), _);

    return(inmemorytable);

} // close function




