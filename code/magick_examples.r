

# read image with magick (can view in quartz/rstudio this way)
pic1 <- image_read(pic1_location)
# class is magick-image + type is externalptr (a blob)
class(pic1); typeof(pic1)

# go thru web example from documentation
frink <- image_read("https://jeroenooms.github.io/images/frink.png")
print(frink)

# Add 20px left/right and 10px top/bottom
image_border(frink, "#E67457", "5x5")

# Trim margins
image_trim(frink)

# Passport pica
image_crop(frink, "100x150+50")

# Resize
image_scale(frink, "300") # width: 300px

image_scale(frink, "x300") # height: 300px

# Rotate or mirror
image_rotate(frink, 45)
image_flip(frink)
image_flop(frink)

# Set a background color
image_background(frink, "pink", flatten = TRUE)
image_fill(frink, "orange", point = "+100+200", fuzz = 30000)
# Add randomness
image_blur(frink, 10, 5)

image_noise(frink, noisetype="gaussian") # or uniform

# This is so ugly it should be illegal
image_frame(frink, "25x25+10+10")

# Silly filters
image_charcoal(frink)

image_oilpaint(frink)

image_edge(frink)

image_negate(frink)

# Add some text
# NOTE: this breaks the session! DO NOT USE
# # image_annotate(frink, "I like R!", 
# size = 70, gravity = "southwest", color = "green")
# image_annotate(frink, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",
#                degrees = 60, location = "+50+100")
# Only works if ImageMagick has fontconfig
# try(image_annotate(frink, "The quick brown fox", font = 'times-new-roman', size = 30), silent = T)

# another example -- this one with layering
bigdata <- image_read('https://jeroenooms.github.io/images/bigdata.jpg')
frink <- image_read("https://jeroenooms.github.io/images/frink.png")
logo <- image_read("https://www.r-project.org/logo/Rlogo.png")
img <- c(bigdata, logo, frink)
img <- image_scale(img, "300x300")
image_info(img)
# can access by vec position
img[2]

# A mosaic prints images on top of one another, expanding the output canvas such that that everything fits:
image_mosaic(img)
# Flattening combines the layers into a single image which has the size of the first image:
image_flatten(img)

# Flattening and mosaic allow for specifying alternative composite operators:
image_flatten(img, 'Add')
image_flatten(img, 'Modulate')
image_flatten(img, 'Minus')

left_to_right <- image_append(image_scale(img, "x200"))
image_background(left_to_right, "white", flatten = TRUE)

top_to_bottom <- image_append(image_scale(img, "100"), stack = TRUE)
image_background(top_to_bottom, "white", flatten = TRUE)

bigdatafrink <- image_scale(image_rotate(image_background(frink, "none"), 300), "x200")
image_composite(image_scale(bigdata, "x400"), bigdatafrink, offset = "+180+100")


# can also do animations! and interfaces with grid + ggplot + base graphics!

frink

image_scale(frink, "200x200!")

image_scale(frink, "300") # width: 300px
image_scale(frink, "x300") # height: 300px

f <- image_scale(frink,"300")
image_scale(f, "x300")



# Several of the transformation functions take an geometry parameter which requires a special syntax of the form  AxB+C+D where each element is optional. Some examples:
#   
#   image_crop(image, "100x150+50"): crop out width:100px and height:150px starting +50px from the left
# image_scale(image, "200"): resize proportionally to width: 200px
# image_scale(image, "x200"): resize proportionally to height: 200px
# image_fill(image, "blue", "+100+200"): flood fill with blue starting at the point at x:100, y:200
# image_border(frink, "red", "20x10"): adds a border of 20px left+right and 10px top+bottom


# package constraints:
# 
# 	- one theme
# 	- all commercial use 
# 	- attribution where necessary
# 	- try unrestricted use archives first
# 	- 200kb max each
# 	- description: filenames, dims, content, package name, company name
#   - 6 diff sizes 
#   - 10 diff images 
#   I have been putting an orange border around some and I 
#   used the darker orange which is #E67457 or 230 116 87 in RGB FWIW


# all images in 300x250
# perfect world = each pic in each size

# mite have to compress a bit



### acquire MET catalog of images #############################################
# === === === === === === === === === === === === === === === === === === 
link <- paste0(
  "https://media.githubusercontent.com/",
  "media/metmuseum/openaccess/master/MetObjects.csv"
)
# dataset is ~250mb
met <- read.csv(link, header=TRUE, sep=",")
# write it to disk (unable to dl from browser)
write.csv(met, "../data/met_image_metadata-feb2017.csv", row.names=FALSE)
# === === === === === === === === === === === === === === === === === === 

### image processing with jpeg:: ##############################################
# === === === === === === === === === === === === === === === === === === 
library("jpeg")
pic1_jpeg <- readJPEG(pic1_location) 
dim(pic1_jpeg)
# pic read w jpeg:: is of class matrix + type double/float
class(pic1_jpeg); typeof(pic1_jpeg)
# === === === === === === === === === === === === === === === === === === 

# package contents: 
# 
# 	- thumbnail
# 	- name
# 	- description
# 	- each image w sizes:
# 		> 300 x 250
# 		> 728 x 90
# 		> 160 x 600
# 		> 320 x 50
# 		> 300 x 600
# 		> 970 x 250 
# 
# from google adwords image guidelines:
# "The top five sizes (300 x 250, 728 x 90, 160 x 600, 320 x 50, 300 x 600) 
# can be automatically resized to fit 95% of the available placements on the Google Display Network."
# (https://support.google.com/adwords/answer/6223297)






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
    image_write(path=saveloc, image=img, 
                quality=100, format="jpeg", flatten=TRUE)
    
    # print success message if saved; else print failure message
    if (file.exists(saveloc)){
      print(paste0("saved img ", x, " size ", y, " -- ", saveloc))
    } else {
      print(paste0("didn't save file </3 ", saveloc))
    }
    
  }
  print(paste0("done processing size ", y, " of ", length(sizes)))
}

# === === === === === === === === === === === === === === === === === === 
# === === === === === === === === === === === === === === === === === === 

as.character(
  .5*dim_out["width"] - .5*image_info(logo)[["width"]]
)