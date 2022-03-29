require(ggplot2)
require(ggmap)
require(tmaptools)
require(magick)
require(av)
require(plotly)

format_plot <- function() {
  ggplot2::theme(text = element_text(size = 20),
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                 panel.background = element_rect(fill = "white",
                                                 colour = "white"),
                 panel.grid.major = element_line(size = 0.5,
                                                 linetype = "solid",
                                                 color = "light gray")) +
    ggplot2::theme(legend.title = element_blank())
}

# # Create bounding box for map coordinates
position_margin <- 0.005 # Degrees
bbox <- c(min(v$position_long_deg) - position_margin,
          min(v$position_lat_deg) - position_margin,
          max(v$position_long_deg) + position_margin,
          max(v$position_lat_deg) + position_margin)

# # Cadence plot
ggplot2::ggplot(data = v, aes(time_since_start_minutes, cadence)) +
  ggplot2::geom_line(color = "blue") +
  ggplot2::geom_point(color = "blue") +
  ggplot2::labs(y = 'Cadence, rpm',
                x = 'Time, min') + 
  format_plot() 

# # Heart rate plot
ggplot2::ggplot(data = v, aes(time_since_start_minutes, heart_rate)) +
  geom_line(color = "red") + 
  geom_point(color = "red") +
  ggplot2::labs(y = 'Heart Rate, bpm',
                x = 'Time, min') +
  format_plot()

# # Speed plot
ggplot2::ggplot(data = v, aes(time_since_start_minutes, speed)) +
  geom_line(aes(color = speed)) + 
  # geom_point(color = "blue") +
  ggplot2::labs(y = 'Speed, m/s',
                x = 'Time, min') +
  format_plot()

# # Altitude plot
ggplot2::ggplot(data = v, aes(time_since_start_minutes, altitude)) +
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  ggplot2::labs(y = 'Altitude, m',
                x = 'Time, min') +
  format_plot()

# # 3D Position plot
# plot_ly(x = v$position_long_deg, 
#         y = v$position_lat_deg, 
#         z = v$altitude,
#         type = "scatter3d")


# # Map and position plot
# Create map object with altitude color scale
map <- tmaptools::get_stamenmap(bbox = bbox, zoom = 15)
ggmap(map) + 
  geom_path(data = v,
            aes(x = position_long_deg, 
                y = position_lat_deg, 
                color = altitude),
            size = 1.5) + 
  scale_color_gradient(low = "blue", high = "red", na.value = "grey50") + 
  ggplot2::labs(y = 'Latitude, deg',
                x = 'Longitude, deg') +
  format_plot()

# # Animated map plots
num_frames <- 100
row_array_for_frame <- round(seq(from = 1, to = dim(v)[1], length.out = num_frames), 0)
frame_path_array <- paste0("c:/temp/imgs/", 1:num_frames, ".jpg")
map <- tmaptools::get_stamenmap(bbox = bbox, zoom = 15)

for(i in 1:length(row_array_for_frame)){
  ggmap(map) + 
    geom_path(data = v[1:row_array_for_frame[i], ],
              aes(position_long_deg, position_lat_deg),
              color = "red",
              size = 0.5) + 
    geom_point(data = v[row_array_for_frame[i], ],
               aes(position_long_deg, position_lat_deg),
               color = "red",
               size = 1.5) + 
    ggplot2::labs(y = 'Latitude, deg',
                  x = 'Longitude, deg')
  ggsave(frame_path_array[i])
}
m <- image_read(frame_path_array)
m <- image_animate(m)
image_write(m, "c:/temp/imgs/movie.gif")

# # County map plot 
ggplot2::ggplot(data = county_map,
                aes(x = long, y = lat)) +
  ggplot2::geom_polygon() +
  ggplot2::geom_path(data = v,
                     aes(x = position_long_deg, y = position_lat_deg),
                     color = 'white', size = 0.1) +
  ggthemes::theme_map() + # rids plot of extra labels, etc.
  ggplot2::coord_quickmap() +
  ggplot2::ggtitle("") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5))

