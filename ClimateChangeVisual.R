#### Common Era ####
# read in txt file data
tempdata <- read.table("C://Desktop/temps.txt", sep = "\t", header = F)

tempdata <- tempdata %>%
  # select first three columns
  select(V1, V2, V3) %>%
  # rename the variables
  rename(year = V1, rawinstdata = V2, reconst_median = V3) %>%
  # consolidate the proxy data and the measured data for the years they overlap by averaging them
  mutate(avg = rowMeans(select(., rawinstdata, reconst_median), na.rm = T)) %>%
  # select out just year and avg
  select(year, avg)

# add the data for 2018 and 2019 (from more recent studies)
tempdata <- rbind(tempdata, c(2018,0.71))
tempdata <- rbind(tempdata, c(2019,0.83))

# create a 20-year rolling average
tempdata <- tempdata %>%
  mutate(rolling_avg = frollmean(avg,20))

# create a vector of years with 5-year increments from year 24 to year 2019
dates_for_animation <- seq(24, 2019, by=5)
# take the length of that vector for the loop to run through
amin_length <- c(1:length(dates_for_animation))

# for loop to produce frames of the gif
for (i in amin_length) {
  
  # the ggplot
  ggplot() +
    # basic line element with data determined as the year less than or equal to
    # where we are in the dates_for_animarion vector
    # color scale is defined by the termperature value
    geom_line(data = tempdata[tempdata$year <= dates_for_animation[i],],
              aes(x = year, y = rolling_avg, color = rolling_avg),
              size = 1.5) +
    # adding a single point to the end of the line with a white border
    geom_point(
      data = tempdata[tempdata$year == dates_for_animation[i],],
      aes(x = year, y = rolling_avg, fill = rolling_avg),
      color = 'white',
      stroke = 2,
      shape = 21,
      size = 4
    ) +
    # scales for both the line and the point, with limits at the max of 
    # the data so taht they do not change frame to frame
    scale_color_viridis(option = 'C', limits = c(
      min(tempdata$rolling_avg, na.rm = T),
      max(tempdata$rolling_avg, na.rm = T)
    )) +
    scale_fill_viridis(option = 'C', limits = c(
      min(tempdata$rolling_avg, na.rm = T),
      max(tempdata$rolling_avg, na.rm = T)
    )) +
    # black and white theme
    theme_bw() +
    # other theme changes
    theme(
      legend.position = 'none',
      plot.title = element_text(
        size = 16,
        face = 'bold',
        hjust = 0.5
      ),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 6)
    ) +
    # labels
    labs(
      title = 'Global Temperature Trend from 0 to 2019 AD',
      subtitle = paste('Temperature Deviation (C) from 1961-1990 Average\nYear: ', dates_for_animation[i]),
      x = 'Year (AD)',
      y = 'Temperature Deviation (C)',
      caption = 'Visualization by Ben Gregory\n(benpgregory@gmail.com)'
    )
  
  # command to save the frames into the 'Animation' folder on my desktop
  ggsave(
    filename = paste('frame_', dates_for_animation[i], '.png'),
    device = 'png',
    height = 6,
    width = 6,
    units = 'in',
    dpi = 200,
    path = 'C:/Desktop/Animation'
  )
  
}