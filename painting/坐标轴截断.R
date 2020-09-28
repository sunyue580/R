##方法一：plotrix包
##方法二：ggplot2包

library(ggplot2)
set.seed(123)
d <- data.frame(
		  x = 1:20,
		    y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
		  )
ggplot(d, aes(x, y)) + geom_col()

library(dplyr)
breaks = c(7, 17)
d$type <- NA
d$type[d$y < breaks[1]] = "small"
d$type[d$y > breaks[2]] = "big"

d <- filter(d, type == 'big') %>%
	  mutate(type = "small", y = breaks[1]) %>%
	    bind_rows(d)

mymin = function(y) ifelse(y <= breaks[1], 0, breaks[2])              
p <- ggplot(d, aes(x, y)) +
	      geom_rect(aes(xmin = x - .4, xmax = x + .4, ymin = mymin(y), ymax = y)) +
	        facet_grid(type ~ ., scales = "free") +
		  theme(strip.text=element_blank())
p
