# Week 5 Data Visualization Lab

# Install the package ggplot2
# install.packages("ggplot2")

#Anytime I want to use this package
# I need to load it
library(ggplot2)

View(cars)

# Make a quick base R plot- this is not a ggplot
plot(cars)

# Our first ggplot
# we need data + aes + geoms
p <- ggplot(data= cars) + 
        aes(x=dist, y=speed) +
        geom_point()

#One last thing. let's add a line to the data
p + geom_smooth(method= "lm", se= FALSE)

# Add lab fxn and bw theme
p + labs(x= "Speed (MPH)" , y= "Stopping Distance (ft)" , 
         title= "Speed and Stopping Distance" , 
         subtitle = "Analyzing correlation" , 
         caption = "Dataset: Cars") +
  geom_smooth(method= "lm", se = FALSE) + 
  theme_bw()

# RNAseq experiment dataset

# Read the data into R
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

# How many genes in dataset?
  nrow(genes)

# Column genes and how many?
  colnames(genes)
  ncol(genes)
  
# how many "up" regulated genes?
  table(genes$State)
  
round( table(genes$State)/nrow(genes)*100, 2) 

# Make a ggplot
ggp <- ggplot(genes) + aes(x= Condition1, y= Condition2) +
                geom_point() 

#Make a ggplot w/ color
ggp2 <- ggplot(genes) + aes(x= Condition1, y= Condition2, col= State) +
  geom_point() +
  labs(x = "Control- No Drug", y = "Drug Treatment",
        title = "Gene Expression Changes Upon Drug Treatment",
        caption = "Data:Genes")

#Change color
ggp2 + scale_color_manual(values= c("blue", "gray", "red"))


#Optional gapminder
library(gapminder)

url <- "https://raw.githubusercontent.com/jennybc/gapminder/master/inst/extdata/gapminder.tsv"

gapminder <- read.delim(url)

# install.packages("dplyr")
library(dplyr)

gapminder_2007 <- gapminder %>% filter (year==2007)

# scatter plot
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point()

# Help us see better scatter plot
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point(alpha=0.5)

# Add more variables
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp,
      color= continent, size = pop) +
  geom_point(alpha=0.5)

# numeric variable population
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp,
      color = pop) +
  geom_point(alpha= 0.8)

# Adjusting point size
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp,
      size = pop) +
  geom_point(alpha= 0.5)

# Scale Size area
ggplot(gapminder_2007) +
  geom_point(aes(x=gdpPercap, y=lifeExp,
      size = pop), alpha= 0.5) +
  scale_size_area(max_size = 10)

#ggplot 1957
gapminder_1957 <- gapminder %>% filter(year==1957)

ggplot(gapminder_1957) +
  aes(x = gdpPercap, y = lifeExp,
      color = continent, size = pop) +
  geom_point(alpha=0.7) +
  scale_size_area(max_size = 10)

# both 1957 & 2007
gapminder_57_07 <- gapminder %>% filter(year==1957 | year==2007)

ggplot(gapminder_57_07) + 
  geom_point(aes(x = gdpPercap, y = lifeExp, 
                 color=continent,size = pop), 
             alpha=0.7) + 
  scale_size_area(max_size = 10) +
  facet_wrap(~year)

# Optional- Bar Chart
gapminder_top5 <- gapminder %>% 
  filter(year==2007) %>% 
  arrange(desc(pop)) %>% 
  top_n(5, pop)

gapminder_top5

# Create bar charts
ggplot(gapminder_top5)+
  geom_col(aes(x=country,y=pop))

# Fill with color
ggplot(gapminder_top5) + 
  geom_col(aes(x = country, y = pop, fill= continent))

# Use numeric variable like Life Expectancy instead
ggplot(gapminder_top5) + 
  geom_col(aes(x = country, y = pop, fill= lifeExp))

# Population size by Country
ggplot(gapminder_top5) + 
  aes(x = country, y = pop, fill=gdpPercap) +
  geom_col()

# Reorder bars
ggplot(gapminder_top5) + 
  aes(x = reorder(country, -pop), y = pop, fill=gdpPercap) +
  geom_col()

# Fill by countries
ggplot(gapminder_top5) + 
  aes(x = reorder(country, -pop), y = pop, fill=country) +
  geom_col(col="gray30") +
  guides(fill=FALSE)

# Flipping bar charts
# Let's look at the inbuild dataset USArrests
head(USArrests)

USArrests$State <- rownames(USArrests)
ggplot(USArrests) +
  aes(x=reorder(State,Murder), y= Murder) +
  geom_col() +
  coord_flip()

# Seems crowded, let's fix by combining
ggplot(USArrests) +
  aes(x=reorder(State,Murder), y= Murder) +
  geom_point() +
  geom_segment(aes(x=State,
                   xend=State,
                   y=0,
                   yend=Murder), color="blue")+
  coord_flip()