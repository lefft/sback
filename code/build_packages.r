### build_packages.r ##########################################################
# 
# this script iterates over images in a directory and a set of sizes,
# producing a spaceback 'package' consisting of each original image
# in each of the specified sizes.
# 
# each output image has a border and the spaceback logo in bottom left
# 
# see README.md for list of outstanding issues
# === === === === === === === === === === === === === === === === === === 

# load magick:: for image processing
library("magick"); library("magrittr"); library("plyr")

# load functions
source("functions.r")

# image sizes we need, w/o geometric formatting
sizes <- c("300x250","728x90","160x600","320x50","300x600","970x250")

# path to specific package folder, containing the images
package_name <- "animals2"
loc <- paste0("../../packages/", package_name, "/")

# filenames of raw images, w/o path
files <- dir(loc)[endsWith(dir(loc), ".jpg") | endsWith(dir(loc), ".jpeg")]

### build 1 -- in old_approach.r (works but slow + inflexible) ################
# === === === === === === === === === === === === === === === === === === 

# (see 'old_approach.r')

### build 2 -- explicitly loop over sizes and files ###########################
# === === === === === === === === === === === === === === === === === === 

# ordinary loop -- more transparent but slower than vectorized version below
for (x in seq_along(files)){
  for (y in seq_along(sizes)){
    spacebackify(
      path="../../packages/animals2/", img=files[x], size=sizes[y],
      logo=image_read("../../packages/spaceback/spaceback.png"),
      back=image_read("../../packages/2000px-Solid_black.svg.png")
    )
  }
  print(paste0("done with file ", x, " of ", length(files)))
}




### build 3 -- nested sapply() calls for vectorized application ###############
# === === === === === === === === === === === === === === === === === === 

# vectorize the loops for better performance
sapply(files, function(image){
  sapply(sizes, function(outsize){
    spacebackify(
      path="../../packages/test/", img=image, size=outsize,
      logo=image_read("../../packages/spaceback/spaceback.png"),
      back=image_read("../../packages/2000px-Solid_black.svg.png"),
      border=5, color="#E67457", resolution=100
    )
  })
})



### SCRATCH AREA ##############################################################
# === === === === === === === === === === === === === === === === === === 

# spacebackify(path="../../packages/test/",
#              img="bird-animal-nature-strauss-60692.jpg",
#              logo=image_read("../../packages/spaceback/spaceback.png"),
#              back=image_read("../../packages/2000px-Solid_black.svg.png"),
#              size="300x250")

# for cropping
# image_crop(boosh<-image_scale(image_read(paste0(path,img)),"500"), 
#            "100x150+10+30")


# read in the logo image and black background (must be in global environment)
# logo <- image_read("../../packages/spaceback/spaceback.png")
# back <- image_read("../../packages/2000px-Solid_black.svg.png")




# initialize a progress bar
# progbar <- create_progress_bar("text")
# progbar$init(length(files))
# step the progress bar
# progbar$step()