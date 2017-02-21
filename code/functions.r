# want to add args for:
#   >> exact: should image be cropped or scaled?
#   >> threshold: maximum difference in aspect ratio to tolerate before crop
#   >> ...
spacebackify <- function(path, img, logo, back, border=5, 
                         color="#E67457", resolution=100, size){
  
  # create dir for output if it doesnt exist
  if (!file.exists(paste0(path, size))){
    dir.create(paste0(path, size))
  }
  
  pic <- image_read(paste0(path,img))
  back <- back
  dims <- as.integer(strsplit(size, "x")[[1]])
  
  # scale back to 300x250 less border, and add border
  back_dim <- paste0(dims[1]-(2*border),"x",dims[2]-(2*border),"!")
  back_scaled <-
    image_scale(back, back_dim) %>% 
    image_border(color=color, paste0(border,"x",border))
  
  
  # scale logo to 100 height (for example)
  # logo_scaled <-
  #   image_scale(logo, "100")
  
  # scale logo size -- set width to 
  #   - 1/2 output width for 160x600, 300x250, 300x600
  #   - 1/3 output width for 320x50, 728x90, 970x250
  if (size %in% c("160x600","300x250","300x600")){
    logo_scaled <- image_scale(logo, as.character(.5*dims[1]))
  } else {
    logo_scaled <- image_scale(logo, as.character(.33*dims[1]))
  }
  
  
  ar_pic <- aspect_ratio(pic)
  ar_out <- aspect_ratio(back_scaled)
  
  # if pic aspect ratio is larger/eq than 300/250 = 1.2, scale to height
  if (ar_pic >= ar_out){
    
    # scale img to 300x250 less twice the border width
    pic_scaled <-
      image_scale(pic, paste0(width(back_scaled) - (2*border)))
    
    # put img on back, st vertical edges adjacent to inner border
    out <-
      # left edge +5 for border; top edge +125 half height -79 half pic height
      image_composite(back_scaled, pic_scaled, offset=paste0(
        "+",border,"+", (.5 * height(back_scaled)) - (.5 * height(pic_scaled))
      )) %>% 
      # 250 to bottom; -19 for logo height; -5 for border; -5 to match
      image_composite(logo_scaled, offset=paste0(
        "+10","+", height(back_scaled) - border*2 - height(logo_scaled)
      ))
    
  } else {
    # otherwise (ar_pic less than ar_out), scale to width
    pic_scaled <- 
      image_scale(pic, paste0("x", height(back_scaled) - (2*border)))
    
    # put img on back, st vertical edges adjacent to inner border
    out <-
      # left edge +5 for border; top edge +125 half height -79 half pic height
      image_composite(back_scaled, pic_scaled, offset=paste0(
        "+", (.5 * width(back_scaled)) - (.5 * width(pic_scaled)), "+", border
      )) %>% 
      # 250 to bottom; -19 for logo height; -5 for border; -5 to match
      image_composite(logo_scaled, offset=paste0(
        "+10","+", height(back_scaled) - border*2 - height(logo_scaled)
      ))
  }
  
  # filename and location for output file
  saveloc <- paste0(path, size, "/rendered_", size, "_", img)
  
  # save rendered image in appropriate directory
  image_write(path=saveloc, image=out, quality=resolution, 
              format="jpg", flatten=TRUE)
  
  # end of function body
}




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


# functions:
#   render_W*xH*(pic/path, exact, logo, border, resolution)
#   find_sizes 
#   build_package(sizes)


aspect_ratio <- function(x){
  round(as.numeric(image_info(x)["width"] / image_info(x)["height"]), 4)
}

width <- function(x){
  round(as.numeric(image_info(x)["width"]))
}

height <- function(x){
  round(as.numeric(image_info(x)["height"]))
}