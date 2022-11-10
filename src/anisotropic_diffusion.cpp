#include <Rcpp.h>
#include "AnisotropicDiffusion.h"
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix cpp_anisotropic_diffusion(Rcpp::IntegerMatrix image, unsigned int iterations, double lambda, double k)
{
  AnisotropicDiffusion AD(Rcpp::clone(image), iterations, lambda, k);
  AD.applyDiffusion();
  return AD.getImage();
}
