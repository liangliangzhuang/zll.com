#============================================================================
#===================================  Function ==============================
#============================================================================
# process_image
process_image <- function(image_file_name, k_list) {
  # To process the original image data, 
  # use the `kmeans` method to divide the R, G, and B data into 
  # k categories to obtain different category colors. 
  
  ## Input:
  ## – image_file_name - a PNG or JPEG image.
  ## – out - the number of centres in the clustering
  ## 
  ## Output:
  ## – cluster_info: a [list] that contain
  ## * the original output of the kclust calls,
  ## * the tidied clusters, their associated RGB values and their nearest DMC thread colour
  ## information in `result`.
  ##	
  ## - Example:
  ## image_file_name = "./avatar.jpg"
  ## k_list = c(2:5)
  ## cluster_info = process_image(image_file_name, k_list)

  # load image and show
  im <- imager::load.image(image_file_name)
  plot(im)
  
  # k-means cluster
  data <- as.data.frame(im, wide = "c") %>% rename(R = c.1, G = c.2, B = c.3)
  
  dat = select(data, c(-x,-y))
  kclusts <-
    tibble(k_list) %>%
    mutate(
      kclust = map(k_list, ~kmeans(x = dat , centers = .x, nstart=4)),
      glanced = map(kclust, glance),
      tidies = map(kclust, tidy),
    )
  
  clusterings <- 
    kclusts %>%
    unnest(cols = c(glanced))
  
  cluster_info <- list(data, clusterings)
  
  return(cluster_info)
}
# scree_plot
scree_plot <- function(cluster_info) {
  # Input:
  # - the element of function [process_image]
  #
  # Output:
  # - a graph
  ##
  ## Example:
	## scree_plot(cluster_info[[2]])

  plot.withinss <- ggplot(cluster_info, aes(k_list, tot.withinss)) +
    geom_line() +
    geom_point()
  
  
  nclust = length(cluster_info$k_list)
  ratio = rep(NA, nclust-1)
  for (kk in 2:nclust) {
    ratio[kk-1] = cluster_info$tot.withinss[kk]/cluster_info$tot.withinss[kk-1]
  }
  plot_data <- data.frame(k = cluster_info$k_list[2:nclust],ratio)
  plot.ratio <- ggplot(plot_data, aes(x=k, y = ratio)) + geom_line()
  
  plot_grid(plot.withinss, plot.ratio, labels = c("withinss", "ratio"))
}
# colour_strips
colour_strips <- function(one_cluster_info) {
  # The goal of dmc is to allow you to find the closest DMC embroidery floss 
  # colour(s) for a given colour, as well as access colour (hex, RGB) 
  # information about DMC colours.
  ## Input:
  ## - the element of function [process_image]
  ##
  ## Output:
  ## - a numeric: the hex of each clusters[denoted by k].  
  ##
  ## Example:
	## k = 4
  ## one_cluster_info = colour_strips(cluster_info[[2]][k-1,])

  # cluster information x y R G B .cluster
  one_cluster <- 
    augment(one_cluster_info$kclust[[1]], cluster_info[[1]]) %>% 
    rename(cluster = .cluster)
  
  # use dmc to get the cluster color
  centres <- one_cluster_info$tidies[[1]] %>% mutate(col = rgb(R,G,B))
  centres_color <- 
    map(centres$col, ~dmc(.x, visualize = FALSE, method = "euclidean")) %>%
    tibble() %>%
    unnest(cols = c(.))
  
  show_col(centres_color$hex)
  
  one_cluster <-
    centres_color[array(one_cluster$cluster),] %>%
    select(dmc,name,hex) %>%
    cbind(one_cluster, .)
  
  return(one_cluster)
}
# make_pattern
make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL){
  ## Input:
  ## – cluster_info - The output of process_image
  ## – k - The chosen cluster size
  ## – x_size - The (approximate) total number of possible stitches in the horizontal direction
  ## – black_white - (logical) Print the pattern in black and white (TRUE) or colour (FALSE,
  ## default)
  ## – background_colour - The colour of the background, which should not be stitched in the
  ## pattern. (Default is to not have a colour)
  ##
  ## Output:
  ## - A graph
  ##
  ## Example:
	## make_pattern(one_cluster_info, k = k, x_size = 50, black_white = FALSE, background_colour = NULL)
  ## make_pattern(one_cluster_info, k = k, x_size = 50, black_white = TRUE, background_colour = NULL)
  ## make_pattern(one_cluster_info, k = k, x_size = 50, black_white = FALSE, background_colour = "#000000")

  
  
  source('change_resolution.R')
  data_resize = change_resolution(cluster_info, x_size=x_size)
  
  if (black_white){
    # plot black white stitch image here
    ggplot(data_resize, aes(x = x, y = y, shape=cluster)) + 
      geom_point() + 
      scale_y_reverse() +
      theme_void() +
      theme(legend.position="bottom")
  } else {
    if (!is.null(background_colour)){
      # set the background color and plot the stitch image here
      center_color = data_resize$hex
      center_color[center_color == "#BA4A4A"] = background_colour
      ggplot(data_resize, aes(x = x, y = y, color=I(center_color), shape=cluster)) + 
        geom_point() + 
        scale_y_reverse()+
        theme_void() +
        theme(legend.position="bottom")
    }
    else{
      # plot normally
      ggplot(data_resize, aes(x = x, y = y, color=I(hex), shape=cluster)) + 
        geom_point() + 
        scale_y_reverse()+
        theme_void() +
        theme(legend.position="bottom")
    }
  }
  
}


