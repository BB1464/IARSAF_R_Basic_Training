################################
################################
## The Data
################################
################################

library(tidyverse)
library(agridat)

data(australia.soybean)
dat <- australia.soybean %>% 
  as_tibble()

dat

min(dat$protein, na.rm=TRUE)
max(dat$protein, na.rm=TRUE)
range(dat$protein, na.rm=TRUE)
range(dat$oil, na.rm=TRUE)

levels(dat$env)
# similar to below
unique(dat$env)
levels(dat$loc)
table(dat$loc, dat$year)

## Factor
dat$year <- as.factor(dat$year)
dat

################################
################################
## Scatter plot
################################
################################

## Scatter Plot -- Option 1
ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point()

# We can sometimes see the aes() inside the ggplot() function --- Option 2
ggplot(dat , aes(x = oil, y = yield)) +
  geom_point()

# However, we prefer Option  1 (for readability)
  # the data
  # the variables
  # the type of plot

#####################################
ggplot(dat) +                       #
  aes(x = oil, y = yield) +         #
  geom_point()                      #
#####################################

# We can do better: personalize the plot to make it more informative.
ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point() +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  ) 


# We can save the "basic" plot in an object and add layer(s) by cascades for more readability
p <- ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p

## geom_point() understands also the following aesthetics
p <- ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point(shape=21, color="red", fill="green", size=3)
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p

?geom_point


# We may be interested to plot by environment
p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env) +
  geom_point()

p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p

dat

# We can do better by changing the title of the legend: "Environment" instead of "env"
p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + scale_color_discrete(name="Environment")
p


# We can change the default color
# The RColorBrewer package makes it easy to quickly load sensible color palettes

library(RColorBrewer)

# We can change the palette via the function scale_color_brewer()
# The function below displays the available palettes
display.brewer.all()

p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + scale_color_brewer(name="Environment", palette="Dark2")
p

# Possible to add a smooth line fitted to the data
p <- ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + geom_smooth()
p

# In case of simple linear regression, possible to display the regression line on
# the plot. Can be done by adding method = lm in geom_smooth()
p <- ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + geom_smooth(method = lm)
p


# We can add additional information to the plot, the protein
p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env, size=protein) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + scale_color_discrete(name = "Environment")
p <- p + scale_size_continuous("Protein")
p


# There are several functions in ggplot2 to change the theme of the plot
# theme_gray(): default
p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env, size=protein) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + scale_color_discrete(name = "Environment")
p <- p + scale_size_continuous("Protein")
p <- p + theme_gray()
p

# theme_bw(): black and white
p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env, size=protein) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + scale_color_discrete(name = "Environment")
p <- p + scale_size_continuous("Protein")
p <- p + theme_bw()
p

# theme_minimal(): minimal 
p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env, size=protein) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + scale_color_discrete(name = "Environment")
p <- p + scale_size_continuous("Protein")
p <- p + theme_minimal()
p

# theme_classic(): classic
p <- ggplot(dat) +
  aes(x = oil, y = yield, color=env, size=protein) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + scale_color_discrete(name = "Environment")
p <- p + scale_size_continuous("Protein")
p <- p + theme_classic()
p

# Facet grid option
p <- ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + facet_grid(. ~ env)
p <- p + theme_bw()
p


################################
################################
## Line plot
################################
#################################

# Line plots, particularly useful in time series, can be created by using geom_line()
p <- ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_line()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + theme_classic()
p


# We can add a line to a scatter plot by simply adding a layer to the initial scatter plot
p <- ggplot(dat) +
  aes(x = oil, y = yield) +
  geom_point() +
  geom_line()
p <- p +
  labs(title = "Soybeans, yield and oil in Australia",
       subtitle = "Period 1970-1971",
       caption = "Data: agridat::Australia.soybean",
       x = "Oil (%)",
       y = "Yield (metric tons/hectare)"
  )
p <- p + theme_classic()
p

################################
################################
## Histogram & density plot
################################
#################################

## Histogram 
p <- ggplot(dat) +
  aes(x = yield) +
  geom_histogram()
p <- p + theme_classic()
p

# Density plot
p <- ggplot(dat) +
  aes(x=yield) +
  geom_density()
p <- p + theme_classic()
p

# Density plot & Histogram overplayed
p <- ggplot(dat) +
  aes(x=yield, y=..density..)+
  geom_histogram() +
  geom_density()
p <- p + theme_classic()
p

# Can superimpose several densities by environment
p <- ggplot(dat) +
  aes(x=yield, color=env, fill=env) +
  geom_density(alpha = 0.25)
p <- p + scale_fill_discrete(name="Environment")
p <- p + guides(color=FALSE)
p <- p + theme_classic()
p

################################
################################
## Boxplot
################################
#################################

p <- ggplot(dat) +
  aes(x = "", y = yield) +
  geom_boxplot()
p

# Boxplot by env
p <- ggplot(dat) +
  aes(x = env, y = yield) +
  geom_boxplot()
p

# Much better?
p <- ggplot(dat) +
  aes(x = env, y = yield, fill = env) +
  geom_boxplot()
p <- p + labs(x = "", y = "Yield (metric tons/hectare)")
#p <- p + guides(fill=FALSE)
p <- p + theme_classic()
p


################################
################################
## Bar plot
################################
#################################

p <- ggplot(dat) +
  aes(x = lodging) +
  geom_bar()
p <- p + labs(x = "Lodging", y = "Count")
p <- p + theme_classic()
p

# fill by year - stacked bar plot
p <- ggplot(dat) +
  aes(x = lodging, fill=year) +
  geom_bar()
p <- p + labs(x = "Lodging", y = "Count")
p + scale_fill_discrete(name="Lodging")
p

# fill by year - multiple bar plot
p <- ggplot(dat) +
  aes(x = lodging, fill=year) +
  geom_bar(position="dodge")
p <- p + labs(x = "Lodging", y = "Count")
p + scale_fill_discrete(name="Lodging")
p

?geom_bar

# Save plot
# The function ggsave() will save the most recent plot to disk

ggsave("my-plot.png")

