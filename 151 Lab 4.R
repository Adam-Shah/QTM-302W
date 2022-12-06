plot_ly(data = tips, x = ~totbill, y = ~tip,
        text = ~time, # Hover text
        type = 'scatter', color = ~day,
        marker = list(size = 10)) %>%
  layout(title = 'My nice title', # Layout
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

dat <- USArrests[1:6,] 
dat
new_dat<- arrange(dat, UrbanPop)
new_dat
