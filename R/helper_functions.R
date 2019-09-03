
#' Make title indicators for bootstrap carousel from template
#' @param data Configuration data with title attributes for each element
#' @return Vector of HTML strings of title indicators
make_title_indicators <- function(data){
  # Make carousel indicators
  indicators = c()
  for(i in 1:length(data)){
    class = ''
    if(i == 1){
      class = 'active'
    }
    title = data[[i]]$title
    indicators = c(indicators, htmlTemplate("html/car_ind.html",
                                            index = i-1,
                                            title = title,
                                            class = class,
                                            document_ = FALSE))
  }
  return(indicators)
}

#' Make information boxes for bootstrap carousel from template
#' @param data Configuration data with title, link, 
#'    image, caption, creator attributes for each element
#' @return Vector of HTML strings of content boxes
make_content_boxes <- function(data){
  # Make content boxes
  boxes = c()
  for(i in 1:length(data)){
    class = 'item'
    if(i == 1){
      class = "item active"
    }
    
    link = data[[i]]$link
    image = data[[i]]$image
    caption = data[[i]]$caption
    creator = data[[i]]$creator
    boxes = c(boxes, htmlTemplate("html/car_box.html",
                                  link = link,
                                  image = image,
                                  caption = caption,
                                  creator = creator,
                                  class = class,
                                  document_ = FALSE))
  }
  return(boxes)
}


#' Make bootstrap carousel
#' @param indicators Vector of HTML snippets for title indicators
#' @param boxes Vector of HTML snippets for content boxes
#' @return HTML document of a bootstrap carousel
make_carousel <- function(indicators, boxes){
  # Populate carousel template with indicators and boxes
  renderDocument(htmlTemplate("html/car_temp.html",
                              indicators = indicators,
                              boxes = boxes,
                              document_ = TRUE))
}


library(htmltools)
# Make thumbnail
thumbnail <- function(title, cap, img, href, caption = TRUE) {
  div(class = "col-sm-4",
      a(class = "thumbnail btn-outline-info", href = href,
        img(src = img),
        div(class = if (caption) "caption",
            if (caption) HTML(paste(tags$b(title), "<br>", cap))),
        style = "height:240px;width:250px;")
  )
}

app_icon <- function(href, title, img, width=NULL, height=NULL){
  if(!is.null(width) & !is.null(height)){
    dim_str = paste(width, "x", height, sep="")
    resize_helper(img, dim_str)
  }
  
  as.character(a(href = href,
    style = "text-decoration:none;color:transparent",
    title = title,
    img(src = img)
  ))
}


# 
# library(magick)
# library(purrr)
# resize_imgs <- function(path, width, height, keep_ratio = TRUE){
#   files = list.files(path, full.names = TRUE)
#   if(keep_ratio){
#     dim_str = paste(width, "x", height, sep="")
#   }
#   else{
#     dim_str = paste(width, "x", height, "!", sep="")
#   }
#   purrr::map(files, ~resize_helper(.x, dim_str))
#   return()
# }
# 
# resize_helper <- function(img_path, dim_str){
#   imFile = image_read(img_path)
#   resized = image_scale(imFile, dim_str)
#   image_write(resized, img_path)
#   return()
# }