# Project 3 Unstructured Data Analytics
# Task 1
# Name: Goh Jia En
# Matric No: P121534

################################ PART A ###############################
library(imager)
folder_path <- "C:/Users/User/Downloads/images"
photos <- list.files(folder_path)

# Edge detection
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  layout(t(1:2))
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  image_xedges <- deriche(image, 2, order = 2, axis = "x") # Edge detector along x-axis
  
  plot(image_xedges, main = paste0("Edge x-axis ", photo_clean))
  
  image_yedges <- deriche(image, 2, order = 2, axis = "y") # Edge detector along y-axis
  
  plot(image_yedges, main = paste0("Edge y-axis ", photo_clean))
}


# Splitting and concatenating image
### By x
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  par(mfrow = c(2, 2))
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  image_xsplit <- imsplit(image, "x", 4)
  
  for(i in seq_along(image_xsplit)){
    plot(image_xsplit[[i]], main = paste0("x-axis Split ", photo_clean))
  }
}


### By y
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  par(mfrow = c(2, 2))
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  image_ysplit <- imsplit(image, "y", 3)
  
  for(i in seq_along(image_ysplit)){
    plot(image_ysplit[[i]], main = paste0("y-axis Split ", photo_clean))
  }
}


### negative nb
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  par(mfrow = c(2, 2))
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  neg_image_xsplit <- imsplit(image, "x", -250)
  
  for(i in seq_along(neg_image_xsplit)){
    plot(neg_image_xsplit[[i]], main = paste0("negative x-axis Split ", photo_clean))
  }
}


## Concatenating
### direction = "x"
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  par(mfrow = c(1, 2))
  
  image_xsplit <- imsplit(image, "x", 4)
  
  image_xconcate <- imappend(image_xsplit, "x")
  
  plot(image_xconcate, main = paste0("x-Concatenated ", photo_clean))
  
  image_ysplit <- imsplit(image, "y", 3)
  
  image_yconcate <- imappend(image_ysplit, "y")
  
  plot(image_yconcate, main = paste0("y-Concatenated ", photo_clean))
}


# Image transformation such as resizing, rotation, scaling, and cropping
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  par(mfrow = c(2, 2))
  
  image_resize <- resize(image, -10, -10)
  
  plot(image_resize, main = paste0("Resized ", photo_clean))
  
  image_rotate <- imrotate(image, 90)
  
  plot(image_rotate, main = paste0("Rotated ", photo_clean))
  
  image_scale <- imshift(image, 40, 20)
  
  plot(image_scale, main = paste0("Scaled ", photo_clean))
  
  image_crop <- autocrop(image_scale, c(0, 0, 0))
  
  plot(image_crop, main = paste0("Cropped Scaled ", photo_clean))
}


# Filtering image
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  par(mfrow = c(1, 1))
  
  flt <- as.cimg(matrix(1, 4, 4))
  
  gray_image <- grayscale(image)
  
  image_filter <- correlate(gray_image, flt)
  
  plot(image_filter, main = paste0("Filtered with box filter ", photo_clean))
}


# Rectangular, circular, and fuzzy selection
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  par(mfrow = c(2, 2))
  
  image_rectangle <- (Xc(image) %inr% c(30, 150)) & (Yc(image) %inr% c(10, 100))
  
  plot(image, main = paste0("Rectangular Selection ", photo_clean))
  
  highlight(image_rectangle)
  
  image_circle <- (Xc(image) - 200)^2 + (Yc(image) - 350)^2 < 150^2
  
  plot(image, main = paste0("Cicular Selection ", photo_clean))
  
  highlight(image_circle)
  
  image_fuzzy <- px.flood(image, 100, 100, sigma = 0.14)
  
  plot(image, main = paste0("Fuzzy Selection ", photo_clean))
  
  highlight(image_fuzzy)
}


# Blurry and sharpen
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  image_noisy <- (image + 0.5 * rnorm(prod(dim(image))))
  
  layout(t(1:2))
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  plot(image_noisy, main = paste0("Blurred ", photo_clean))
  
  denoised_image <- isoblur(image_noisy, 5)
  
  plot(denoised_image, main = paste0("Sharpened ", photo_clean))
}


# Segmentation
library(imagerExtra)
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  par(mfrow = c(2, 2))
  
  gray_image <- grayscale(image)
  
  plot(gray_image, main = paste0("Original ", photo_clean))
  
  image_segment1 <- ThresholdTriclass(gray_image, stopval = 0.001)
  
  plot(image_segment1, main = paste0("stopval = 0.001 ", photo_clean))
  
  image_segment2 <- ThresholdTriclass(gray_image, repeatnum = 1)
  
  plot(image_segment2, main = paste0("repeatnum = 1 ", photo_clean))
  
  image_CVS <- SegmentCV(gray_image, lambda2 = 2)
  
  plot(image_CVS, main = paste0("CVS3 ", photo_clean))
}


# Histogram equalization
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  par(mfrow = c(2, 1))
  
  gray_image <- grayscale(image)
  
  hist(gray_image, main = paste0("Luminance values in ", photo_clean))
  
  image_red <- R(image)
  
  hist(image_red, main = paste0("Red channel values in ", photo_clean))
}


################################ PART B ###############################
# Import data
folder_path <- "C:/Users/User/Downloads/Noise Image"
photos <- list.files(folder_path)


# Image denoising
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  layout(t(1:2))
  
  plot(image, main = paste0("Original ", photo_clean))
  
  denoised_image <- isoblur(image, 3)
  
  plot(denoised_image, main = paste0("Denoised ", photo_clean))
}


# Morphological operations
for (photo in photos){
  photo_path <- file.path(folder_path, photo)
  
  image <- load.image(photo_path)
  
  photo_clean <- gsub("\\.jpeg$", "", photo, ignore.case = TRUE)
  
  par(mfrow = c(2, 2))

  morpho1_noise <- threshold(image,"20%")
  
  plot(morpho1_noise, main = paste0("Threshold = 0.2 ", photo_clean))
  
  morpho2_noise <- threshold(image,"15%")
  
  plot(morpho2_noise, main = paste0("Threshold = 0.15 ", photo_clean))
  
  morpho3_noise <- threshold(image,"10%")
  
  plot(morpho3_noise, main = paste0("Threshold = 0.1 ", photo_clean))
}