# for reference, see: 
#   https://cran.r-project.org/web/packages/magick/vignettes/intro.html

# next steps:
# 
# build in logic for:
#   > if image dimensions are close to target size, stretch or add negative space
#   > if image dimensions are far from target size, then either:
#       --> crop until close to target size, then stretch or add space; or
#       --> just skip it (depending on image -- requires manual inspection)
#   


# load magick:: for image processing
library("magick"); library("magrittr")

# image sizes we need, in geometric format
sizes <- c("300x250!","728x90!","160x600!","320x50!","300x600!","970x250!")

# dir to vintage_bball package, containing images
loc <- "../packages/vintage_bball/"
files <- dir(loc)[endsWith(dir(loc), ".jpg")]

for (y in seq_along(sizes)){
  size <- sizes[y]
  if (!file.exists(paste0(loc, gsub("!","",size)))){
    dir.create(paste0(loc, gsub("!","",size)))
  }
  for (x in seq_along(files)){
    img <-
      paste0(loc, files[x]) %>%
      image_read(paste0(loc,files[x])) %>%
      image_scale(size) %>%
      image_border("#E67457","5x5")
    
    # filename and location for output file
    saveloc <- paste0(
      loc, gsub("!","",size), "/rendered_", gsub("!","",size), "_", files[x]
    )
    # write output file
    image_write(path=saveloc, image=img, quality=100, format="jpeg", flatten=TRUE)
    
    # print success message if saved; else print failure message
    if (file.exists(saveloc)){
      print(paste0("saved img ", x, " size ", y, " -- ", saveloc))
    } else {
      print(paste0("didn't save file </3 ", saveloc))
    }
    
  }
  print(paste0("done processing size ", y, " of ", length(sizes)))
}



sapply(strsplit(gsub("!","",sizes), "x"), function(x)x[[1]])

# === === === === === === === === === === === === === === === === === === 
# === === === === === === === === === === === === === === === === === === 


boosh <- image_read(paste0(loc,files[1]))

image_info(boosh)[["width"]]
image_info(boosh)[["height"]]

black <- image_read("../packages/2000px-Solid_black.svg.png")

# for each size y:
#   create dir if doesnt exist
#   
#   for each file x: 
#     read x
#     
#     ar_x = aspect ratio of x
#     ar_y = aspect ratio of y (target)
#     
#     if ar_x and ar_y are < 10% different:
#       x = stretch x %>% 
#       
#     else 
#       x = blow up/shrink as close as possible x %>% 
#           put black swatch behind x %>% 
#           ensure x w border is right size
#     
#     put border on x %>% 
#     put logo on x 
#     
#     save x
#     
#     print success/failure message
#           
#     


