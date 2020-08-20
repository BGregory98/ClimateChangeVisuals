#### Common Era ####
tempdata <- read.table("C://Desktop/temps.txt", sep = "\t", header = F)

tempdata <- tempdata %>%
  select(V1, V2, V3) %>%
  rename(year = V1, rawinstdata = V2, reconst_median = V3) %>%
  mutate(avg = rowMeans(select(., rawinstdata, reconst_median), na.rm = T)) %>%
  select(year, avg)

tempdata <- rbind(tempdata, c(2018,0.71))
tempdata <- rbind(tempdata, c(2019,0.83))

tempdata <- tempdata %>%
  mutate(rolling_avg = frollmean(avg,20))

tempdata %>%
  filter(year < 2100,
         year > 1999) %>%
  group_by()%>%
  summarize(avg = mean(avg))


dates_for_animation <- seq(24, 2019, by=5)
amin_length <- c(1:length(dates_for_animation))

for (i in amin_length) {
  ggplot() +
    geom_line(data = tempdata[tempdata$year <= dates_for_animation[i],],
              aes(x = year, y = rolling_avg, color = rolling_avg),
              size = 1.5) +
    geom_point(
      data = tempdata[tempdata$year == dates_for_animation[i],],
      aes(x = year, y = rolling_avg, fill = rolling_avg),
      color = 'white',
      stroke = 2,
      shape = 21,
      size = 4
    ) +
    #geom_label_repel(data = labels, aes(x = year, label = label), y = -0.4141050) +
    scale_color_viridis(option = 'C', limits = c(
      min(tempdata$rolling_avg, na.rm = T),
      max(tempdata$rolling_avg, na.rm = T)
    )) +
    scale_fill_viridis(option = 'C', limits = c(
      min(tempdata$rolling_avg, na.rm = T),
      max(tempdata$rolling_avg, na.rm = T)
    )) +
    theme_bw() +
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
    labs(
      title = 'Global Temperature Trend from 0 to 2019 AD',
      subtitle = paste('Temperature Deviation (C) from 1961-1990 Average\nYear: ', dates_for_animation[i]),
      x = 'Year (AD)',
      y = 'Temperature Deviation (C)',
      caption = 'Visualization by Ben Gregory\n(benpgregory@gmail.com)'
    )
  
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