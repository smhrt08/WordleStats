ggplot(CleanWordleStats, aes(x = weighted_daily_avg)) + 
  geom_histogram(binwidth = 0.2, fill = '#D52B33', color = '#212121') + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = 'Wordle Stats', subtitle = 'Weighted Daily Average', x = '', y = '') + 
  theme(
    panel.background = element_rect(fill = '#F7F7F7'),
    panel.grid = element_blank(),
    axis.line = element_line(color = '#212121'),
    plot.title.position = 'plot',
    plot.title = element_text(size = 18, family = 'mono'),
    plot.subtitle = element_text(size = 10, family = 'mono'),
    axis.text = element_text(family = 'mono')
  )
