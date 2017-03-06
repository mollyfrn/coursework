#Name 
#UNC-CH Department of Biology
#Hurlbert Lab SS2017
#ggplot2: A tutorial with guided examples and exercises
#Sources: http://r-statistics.co/ggplot2-Tutorial-With-R.html 
#         & http://ms.mcmaster.ca/~bolker/misc/ggplot2-book.pdf


#setwd("C:/Users/mollyfrn/Desktop")

####1: Setup####
library(ggplot2)
library(ggfortify)
library(reshape2)
library(zoo) #for time series example data
library(gridExtra) 

# if only the dataset is known:
ggplot(diamonds)  

# if only X-axis is known. The Y-axis can be specified in respective geoms.
ggplot(diamonds, aes(x=carat))  

# if both X and Y axes are fixed for all layers.
ggplot(diamonds, aes(x=carat, y=price)) 

# Each category of the 'cut' variable will now have a distinct  color, once a geom is added.
ggplot(diamonds, aes(x=carat, color=cut))  

ggplot(diamonds, aes(x=carat), color="steelblue")


####2: Adding geoms, layers####
#Part I: some basic geoms 

df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a","b","c")
)
p <- ggplot(df, aes(x, y, label = label)) +
  labs(x = NULL, y = NULL) + # Hide axis label
  theme(plot.title = element_text(size = 12)) # Shrink plot title
p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat = "identity") + ggtitle("bar")
p + geom_tile() + ggtitle("raster")
p + geom_line() + ggtitle("line") #connects but out of data sequence
p + geom_area() + ggtitle("area")
p + geom_path() + ggtitle("path") #connects in order of data sequence
p + geom_polygon() + ggtitle("polygon")

####Exercise 2.1####
#What's the difference between geom_path() and geom_line()?



#Part II: Doing things with geoms!

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth() 
# Adding scatterplot geom (geom_point, layer1) and smoothing geom (geom_smooth, layer2).


ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) + 
  geom_smooth(aes(x=carat, y=price, color=cut)) 
# Same as above but specifying the aesthetics inside the geoms.


ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) +
  geom_smooth(aes(x=carat, y=price)) 
# Remove color from geom_smooth


ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point(aes(color=cut)) + geom_smooth()  
# same as above but simpler


####Exercise 2.2####
#Can you make the shape of the points vary with the color feature?






# Answer to Exercise 2.1 below this line
############
ggplot(diamonds, aes(x=carat, y=price, color=cut, shape=color)) + geom_point()


####3: Labels#### 
####Exercise 3.1: Add appropriate labels to the graph below####
gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() +
  labs(title="", x="", y="")  # add axis lables and plot title.
print(gg)


####4: Theme####
gg1 <- gg + theme(plot.title=element_text(size=30, face="bold"), 
                  axis.text.x=element_text(size=15), 
                  axis.text.y=element_text(size=15),
                  axis.title.x=element_text(size=25),
                  axis.title.y=element_text(size=25)) + 
  scale_color_discrete(name="Cut of diamonds")  # add title and axis text, change legend title.
print(gg1)  # print the plot 
#made axes text HUGE -> easier for vision-challenged to see in presentations


#themes can be really instrumental in creating beautiful graphs;  
#there are a number of color scales you can create from scratch or download 
#including color palettes that are disabled-and-greyscale friendly, like viridis: 
library(viridis)
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() +
  labs(title="Diamonds: Carat vs. Price", x="Carat", y="Price") + scale_color_viridis(discrete=TRUE)


#...or based on your favorite film director 
library(wesanderson)
ggplot(diamonds)+geom_boxplot(aes(fill = factor(cut), x=color,y=price))+
  scale_fill_manual(values = wes_palette("Darjeeling"))+ylim(0, 7500) + 
  labs(title="Diamonds: Color vs. Price", x="Color", y="Price")


#there is such a thing as prudence and simplicity (recall data-ink conversation from last week)
#...or is there? 
ggplot(mpg, aes(cty, hwy)) + add_cat() +geom_point()



####5: Facets####
gg1 + facet_wrap( ~ cut, ncol=3)  # columns defined by 'cut'

gg1 + facet_wrap(color ~ cut)  # row: color, column: cut

gg1 + facet_wrap(color ~ cut, scales="free")  # row: color, column: cut

gg1 + facet_grid(color ~ cut)   # In a grid


####6: Time Series & Multiple Plots on Same Graph####
autoplot(AirPassengers) + labs(title="AirPassengers")  
# where AirPassengers is a 'ts' or time series object

#Multiple plots
# Approach 1:
data(economics, package="ggplot2")  # init data
economics <- data.frame(economics)  # convert to dataframe
ggplot(economics) + geom_line(aes(x=date, y=pce, color="pcs")) +
  geom_line(aes(x=date, y=unemploy, col="unemploy")) + scale_color_discrete(name="Legend") +
  labs(title="Economics") # plot multiple time series using 'geom_line's


# Approach 2, using reshape2:
df <- melt(economics[, c("date", "pce", "unemploy")], id="date")
ggplot(df) + geom_line(aes(x=date, y=value, color=variable)) + 
  labs(title="Economics")# plot multiple time series by melting

#can also use facet_wrap
df <- melt(economics[, c("date", "pce", "unemploy", "psavert")], id="date")
ggplot(df) + geom_line(aes(x=date, y=value, color=variable))  +
  facet_wrap( ~ variable, scales="free")


####7: Bar charts & Histograms####
plot1 <- ggplot(mtcars, aes(x=cyl)) + geom_bar() + labs(title="Frequency bar chart")  
# Y axis derived from counts of X item
print(plot1)

df <- data.frame(var=c("a", "b", "c"), nums=c(1:3))
plot2 <- ggplot(df, aes(x=var, y=nums)) + geom_bar(stat = "identity")  
# Y axis is explicit. 'stat=identity'
print(plot2)

library(gridExtra) 
#nice alternative to facet_grid when you want to manually specify a certain order
grid.arrange(plot1, plot2, ncol=2)

#easy vert-to-horiz fix
df <- data.frame(var=c("a", "b", "c"), nums=c(1:3))
ggplot(df, aes(x=var, y=nums)) + geom_bar(stat = "identity") +
  coord_flip() + labs(title="Coordinates are flipped")


####8: Changing coordinates####
#1
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth() + 
  coord_cartesian(ylim=c(0, 10000)) + labs(title="Coord_cartesian zoomed in!")
#2
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth() + 
  ylim(c(0, 10000)) + labs(title="Datapoints deleted: Note the change in smoothing lines!")


####9: Themes, legends, and other bells and whistles####
#Themes
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() + geom_smooth() +theme_bw() + labs(title="bw Theme")
#try subbing in the following themes for theme_bw() above: 
theme_gray()
theme_bw()
theme_linedraw()
theme_light()
theme_minimal()
theme_classic()
theme_void()


#Manipulating the legend
p1 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() + geom_smooth() + theme(legend.position="none") + 
  labs(title="legend.position='none'")  # remove legend

p2 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() + geom_smooth() + theme(legend.position="top") + 
  labs(title="legend.position='top'")  # legend at top

p3 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() + geom_smooth() + labs(title="legend.position='coords inside plot'") +
  theme(legend.justification=c(1,0), legend.position=c(1,0))  # legend inside the plot

p4 <- grid.arrange(p1, p2, p3, ncol=3)  # arrange for comparison 
p4


#some really ugly grid lines! Exactly what we want!
ggplot(mtcars, aes(x=cyl)) + geom_bar(fill='darkgoldenrod2') +
  theme(panel.background = element_rect(fill = 'steelblue'),
        panel.grid.major = element_line(colour = "firebrick", size=3),
        panel.grid.minor = element_line(colour = "blue", size=1))


#annotations 
library(grid)
my_grob = grobTree(textGrob(
  "This text is at x=0.1 and y=0.9, relative!\n Anchor point is at 0,0", 
                            x=0.1,  y=0.9, hjust=0,
                            gp=gpar(col="firebrick", fontsize=25, fontface="bold")))
ggplot(mtcars, aes(x=cyl)) + 
  geom_bar() + annotation_custom(my_grob) + labs(title="Annotation Example")


####10: Saving Plots####
#individually:
plot1 <- ggplot(mtcars, aes(x=cyl)) + geom_bar()
ggsave("myggplot.png")  # saves the last plot.
ggsave("myggplot.png", plot=plot1)  # save a stored ggplot

#in a plotting loop:
#before the loop, specify file name, type, and whether individually or in a single file
# ggplot will automatically store each new plot created within that empty file when it hits the loop end


####Exercise 10.1####
#Save p4 from section 9 as a png file to your desktop 


