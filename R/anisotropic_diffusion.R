#' Anistropic diffusion
#'
#' In image processing and computer vision, anisotropic diffusion, is a technique aiming at reducing
#' image noise without removing significant parts of the image content, typically edges, lines or
#' other details that are important for the interpretation of the image.
#'
#' @param x a grayscale matrix with values ranging in [0,1] or [0, 255]. Rasters from package raster
#' and terra are also supported
#' @param iteration,lambda numeric
#' @param k integer
#'
#' @examples
#' f = system.file("extdata", "albert.png", package="anidiff")
#' m1 = t(apply(png::readPNG(f)[,,1], 2, rev))
#' m2 = anisotropic_diffusion(m1)
#' image(m1, col = gray((0:255)/255))
#' image(m2, col = gray((0:255)/255))
#' @export
#' @useDynLib anidiff, .registration=TRUE
#' @importFrom Rcpp evalCpp
anisotropic_diffusion = function(x, iteration = 50, lambda = 0.2, k = 10)
{
  UseMethod("anisotropic_diffusion", x)
}

#' @export
anisotropic_diffusion.RasterLayer = function(x, iteration = 50, lambda = 0.2, k = 10)
{
  M = raster::as.matrix(x)
  M2 = anisotropic_diffusion(M, iteration, lambda, k)
  y = x
  y[] = M2
  y
}

#' @export
anisotropic_diffusion.SpatRaster = function(x, iteration = 50, lambda = 0.2, k = 10)
{
  M <- terra::as.matrix(x, wide = TRUE)
  M2 <- anisotropic_diffusion(M, iteration, lambda, k)
  y <- terra::deepcopy(x)
  y[] <- M2
  y
}

#' @export
anisotropic_diffusion.matrix = function(x, iteration = 50, lambda = 0.2, k = 10)
{
  r = range(x)
  m = 0

  if (r[1] >= 0 && r[2] <= 255) m = 1
  if (r[1] >= 0 && r[2] <= 1) m = 255
  if (m == 0) stop("The image must range in [0,1] or [0,255]")

  y = cpp_anisotropic_diffusion(x*m, iteration, lambda, k)
  return(y/m)
}

