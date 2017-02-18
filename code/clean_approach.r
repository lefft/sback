

# functions:
#   render_W*xH*(pic/path, exact, logo, border, resolution)
#   
#   find_sizes 
#   
#   build_package(sizes)
# 
# basic idea:
#   
#   make a list of the images/image paths
#   
#   create the folders for the output
#   initialize the log file object
#   
#   for each image/image path x:
#       find the appropriate sizes for x
#       
#       render x in each of the appropriate sizes
#       
#       save each rendering in its appropriate location
#       
#       write a line of the log file
#   
#   

# load magick:: for image processing
library("magick"); library("magrittr")

# image sizes we need, in geometric format
sizes <- c("300x250!","728x90!","160x600!","320x50!","300x600!","970x250!")


# path to specific package folder, containing the images
package_name <- "test"
path <- paste0("../../packages/", package_name, "/")
img <- "bird-animal-nature-strauss-60692.jpg"
logo <- image_read("../../packages/spaceback/spaceback.png")
black <- image_read("../../packages/2000px-Solid_black.svg.png")
border <- 5
color <- "#E67457"
resolution <- 100
flatten <- TRUE

aspect_ratio <- function(x){
  round(as.numeric(image_info(x)["width"] / image_info(x)["height"]), 4)
}



# want to add args for:
#   exact (can img be scaled?)
render_300x250 <- function(path, img, logo, border, color, resolution, flatten){
  pic <- image_read(paste0(path,img))
  logo <- logo
  back <- black
  
  # scale back to 300x250 less border, and add border
  back_scaled <-
    image_scale(back, "290x240!") %>% 
    image_border(color=color, paste0(border,"x",border))
  
  
  # scale logo to 75 height (for example)
  logo_scaled <-
    image_scale(logo, "100")
  
  ar_pic <- aspect_ratio(pic)
  ar_out <- aspect_ratio(back_scaled)
  # if pic aspect ratio is larger/eq than 300/250 = 1.2, scale to height
  if (ar_pic >= ar_out){
    # scale img to 300x250 less twice the border width
    pic_scaled <-
      image_scale(pic, paste0(300 - (2*border)))
    
    # put img on back, st vertical edges adjacent to inner border
    out <-
      # left edge +5 for border; top edge +125 half height -79 half pic height
      image_composite(back_scaled, pic_scaled, offset="+5+46") %>% 
      # 250 to bottom; -19 for logo height; -5 for border; -5 to match
      image_composite(logo_scaled, offset="+10+221") 
      
  } else {
    # otherwise (ar_pic less than ar_out), scale to width
    pic_scaled <- 
      image_scale(pic, paste0("x", 250 - (2*border)))
    
    # MODIFY THIS SO IT SCALES TO HEIGHT -- ALSO GENERALIZE THE PARTS HERE
    # AND ABOVE WHERE WE MAKE ASSUMPTIONS ABOUT THE HEIGHT OF THE INPUT PIC
    # (CAN REFER TO IT VIA image_info()["height"] ETC LIKE THATTTE)
    out <-
      # left edge +5 for border; top edge +125 half height -79 half pic height
      image_composite(back_scaled, pic_scaled, offset="+5+46") %>% 
      # 250 to bottom; -19 for logo height; -5 for border; -5 to match
      image_composite(logo_scaled, offset="+10+221") 
  }
  
  
  
  
  # save rendered image in appropriate directory
  # ADD LOGIC TO FIYLENAYMMME
  image_write(path=paste0(path,"boosh.eps"), image=out, quality=resolution, 
              format="eps", flatten=flatten)
  
  
  # end of function body
}






### SCRATCH AREA ##############################################################
# === === === === === === === === === === === === === === === === === === 


## THIS IS AN OLD VERSION FROM JUST COPYING PARTS OF LOOPs
render_250x300 <- function(img, exact, logo, border, resolution){
  img <- 
    image_scale(img, as.character(dim_out["width"]-10)) %>% 
    (function(z){image_composite(
      back, z, offset=paste0("+5", "+", as.character(
        .5*dim_out["height"] - .5*image_info(z)[["height"]]
      ))
    )})
  # end of function body
}



# for cropping
# image_crop(boosh<-image_scale(image_read(paste0(path,img)),"500"), 
#            "100x150+10+30")
