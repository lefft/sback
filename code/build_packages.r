# for reference, see: 
#   https://cran.r-project.org/web/packages/magick/vignettes/intro.html

# next steps:
# 
# build in logic for:
#   > if image dimensions are close to target size, 
#     then stretch or add negative space
#   > if image dimensions are far from target size, 
#     then either:
#       --> crop until close to target size, then stretch or add space; or
#       --> just skip it (depending on image -- requires manual inspection)
#   



# load magick:: for image processing
library("magick"); library("magrittr")

# image sizes we need, in geometric format
sizes <- c("300x250!","728x90!","160x600!","320x50!","300x600!","970x250!")

# path to specific package folder, containing the images
# loc <- "../../packages/vintage_bball/"
loc <- "../../packages/animals/"

# list filenames of raw images, w/o path
files <- dir(loc)[endsWith(dir(loc), ".jpg") | endsWith(dir(loc), ".jpeg")]

# black swatch to use as background 
# QUESTION: is this adding to filesize/causing distortion of logo?
black <- image_read("../../packages/2000px-Solid_black.svg.png")

# the logo (in color), to slap on processed images
logo <- image_read("../../packages/spaceback/spaceback.png") 

# black + white version of the logo
logo_bw <- image_read("../../packages/spaceback/spaceback_bw.png") 

# initialize a df object for storing image i/o info (for later inspection)
# update after each loop iteration
info <- data.frame(
  file=rep(files, times=length(sizes)),
  in_size=rep(NA, times=length(file)),
  out_size=rep(sizes, each=length(files))
)


# for each size y:
for (y in seq_along(sizes)){
  
  # set 'size' to the y-th element of 'sizes'
  size <- sizes[y]
  
  # create dir for output if it doesnt exist
  if (!file.exists(paste0(loc, gsub("!","",size)))){
    dir.create(paste0(loc, gsub("!","",size)))
  }
  
  # set the background swatch to appropriate size + add border
  back <- 
    black %>% 
    image_scale(size) %>% 
    image_convert("jpg") %>% 
    image_border("#E67457","5x5") %>% 
    image_scale(size) # rescale after border addition
  
  # for each file x: 
  for (x in seq_along(files)){
    
    # read in raw/original image x
    img <- image_read(paste0(loc,files[x])) 
    
    # dimensions of raw image as a vector
    dim_in <- c(
      width=image_info(img)[["width"]],
      height=image_info(img)[["height"]]
    )
    # aspect ratio of original image
    ar_in <- as.numeric(dim_in["width"] / dim_in["height"])
    
    # dimensions of output as a vector 
    dim_out <- 
      gsub("!","",sizes[y]) %>%
      strsplit("x") %>%
      (function(z) c(z[1],z[2])[[1]]) %>% 
      as.integer() %>% 
      (function(z) c(width=z[1], height=z[2]))
    # aspect ratio of target image to create
    ar_out <- as.numeric(dim_out["width"] / dim_out["height"])
    
    # scale logo size -- set width to 
    #   - 1/2 output width for 160x600, 300x250, 300x600
    #   - 1/3 output width for 320x50, 728x90, 970x250
    if (size %in% c("160x600!","300x250!","300x600!")){
      logo <- image_scale(logo, as.character(.5*dim_out["width"]))
    } else {
      logo <- image_scale(logo, as.character(.33*dim_out["width"]))
    }
    
    # now begin assembling 
    if (ar_in == ar_out){
      img <- img %>% 
        image_scale(size) 
        # image_border("#E67457","5x5")
    } else 
      if (ar_in < ar_out){ # pic narrower than out
        img <- img %>% 
          image_scale(paste0("x", dim_out["height"]-10)) %>% 
          # image_border("#E67457", "2x2") %>% 
          (function(z){
            image_composite(
              back, z, 
              offset=paste0("+", as.character(
                .5*dim_out["width"] - .5*image_info(z)[["width"]]
                ), "+5")
              )
          })
        
        # introduce logo to output image 
        img <- 
          image_composite(
            img,logo, 
            offset=paste0(
              "+10", "+", dim_out["height"]-image_info(logo)[["height"]]-7
            )
          )
        
      } else 
      if (ar_in > ar_out){ # pic wider than out
      img <-
        img %>% 
        image_scale(as.character(dim_out["width"]-10)) %>% 
        # image_border("#E67457", "2x2") %>% 
        # image_scale(as.character(dim_out["width"]-8)) %>% 
        (function(z){
          image_composite(
            back, z, 
            offset=paste0("+5", "+", as.character(
              .5*dim_out["height"] - .5*image_info(z)[["height"]]
            ))
          )
        })
      
      # introduce logo to output image 
      img <- 
        image_composite(
          img,logo, 
          offset=paste0(
            "+10", "+", dim_out["height"]-image_info(logo)[["height"]]-10
          )
        )
    }
    # filename and location for output file
    saveloc <- paste0(
      loc, gsub("!","",size), "/rendered_", gsub("!","",size), "_", files[x]
    )
    # write output file
    image_write(path=saveloc, image=img, quality=100, 
                format="jpg", flatten=TRUE)
    
    # print success message if saved; else print failure message
    if (file.exists(saveloc)){
      print(paste0("saved img ", x, " size ", size, " -- ", files[x]))
    } else {
      print(paste0("didn't save file </3 ", files[x], " in size ", size))
    }
    
  }
}






# === === === === === === === === === === === === === === === === === === 
# === === === === === === === === === === === === === === === === === === 
