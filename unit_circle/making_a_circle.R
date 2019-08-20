library(tidyverse)
library(gganimate)

#Create a data frame containing a high-resolution unit circle
circle_df <- tibble(theta = seq(0, 2 * pi, by = pi/1e3)) %>%
  mutate(circle_x = cos(theta),
    circle_y = sin(theta))

#Make sure that the cirlce plots properly
circle_df %>%
  ggplot(aes(circle_x, circle_y)) +
  geom_path() +
  theme_bw() +
  coord_equal()

#Create a data frame of vectors that point to the edge of the unit circle
# Note: Most testing was done with length.out = 9, to speed things up
vectors_df <- tibble(theta = seq(0, 2 * pi, length.out = 473)) %>%
  mutate(vector_x = cos(theta),
    vector_y = sin(theta),
    center_x = 0,
    center_y = 0) %>%
  #Remove the last row, so there is not overlap with the last frame of the gif
  .[-nrow(.),]

#Plot everything together --- first in a static way, using just ggplot2
p <- ggplot() +
  #Horizontal line segment; could have used hline
  geom_segment(data = tibble(x = -1.25, y = 0, xend = 1.25, yend = 0),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black") +
  #Vertical line segment; could have used vline
  geom_segment(data = tibble(x = 0, y = -1.25, xend = 0, yend = 1.25),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black") +
  #Add the unit circle
  geom_path(data = circle_df,
    aes(circle_x, circle_y),
    size = 0.5) +
  #Add all the lines pointing from the center to the edge of the circle
  geom_segment(data = vectors_df,
    aes(x = center_x,
      y = center_y,
      xend = vector_x,
      yend = vector_y),
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(1, "line")),
    size = 1) +
  #"Highlight" (in yellow) part of x-axis the corresponds to vector
  geom_segment(data = vectors_df,
    aes(x = center_x,
      y = center_y,
      xend = vector_x,
      yend = center_y),
    color = "yellow",
    size = 2,
    alpha = 0.3) +
  #Same highlight for the y-axis
  geom_segment(data = vectors_df,
    aes(x = center_x,
      y = center_y,
      xend = center_x,
      yend = vector_y),
    color = "yellow",
    size = 2,
    alpha = 0.3) +
  #Complete the square using a dashed line; x-axis
  geom_segment(data = vectors_df,
    aes(x = center_x,
      y = vector_y,
      xend = vector_x,
      yend = vector_y),
    color = "grey",
    size = 0.5,
    linetype = "dashed") +
  #Complete the square for the y-axis
  geom_segment(data = vectors_df,
    aes(x = vector_x,
      y = center_y,
      xend = vector_x,
      yend = vector_y),
    color = "grey",
    size = 0.5,
    linetype = "dashed") +
  #Add a blue dot to the end of the radius vector
  geom_point(data = vectors_df,
    aes(x = vector_x,
      y = vector_y),
    size = 5,
    color = "slateblue",
    alpha = 0.7) +
  #Add a red dot to highlight the end of the highlight along the y-axis
  geom_point(data = vectors_df,
    aes(x = center_x,
      y = vector_y),
    size = 5,
    color = "tomato",
    alpha = 0.7) +
  #Same thing for the x-axis
  geom_point(data = vectors_df,
    aes(x = vector_x,
      y = center_y),
    size = 5,
    color = "tomato",
    alpha = 0.7) +
  theme_void() +
  coord_equal()

#Now add the bits required to make gganimate work properly
circle_animation <- p +
  transition_states(vectors_df$theta,
    transition_length = 0.01 / nrow(vectors_df),
    state_length = 0,
    wrap = TRUE) +
  ease_aes('linear')

#Animate all the things!
# I'm using magick_renderer because it works with svg, which looks much better
# than png
p_anim <- animate(circle_animation, nframes = nrow(vectors_df), fps = 50,
  device = "svglite", renderer = magick_renderer())

#Save
anim_save("Unit_circle_animation.gif", p_anim)
