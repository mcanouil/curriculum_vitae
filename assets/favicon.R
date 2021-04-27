library(magick)
path_in <- "utils/favicon.png"
image <- image_read(path_in)
sizes <- c(
  "favicon-16x16.png" = "16", 
  "favicon-32x32.png" = "32", 
  "apple-touch-icon.png" = "180", 
  "apple-touch-icon-120x120.png" = "120", 
  "apple-touch-icon-76x76.png" = "76", 
  "apple-touch-icon-60x60.png" = "60"
)
mapply(
  FUN = function(x, y) image_write(image_resize(image, paste0(x, "x")), file.path("utils", y)), 
  x = sizes,
  y = names(sizes)
)
