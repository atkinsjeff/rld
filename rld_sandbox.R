# rld (root length density)

#package import
library(plyr)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(tidyverse)
library(ggplot2)
library(viridis)
library(minpack.lm)
library(arm)
# import data
# <- read.csv("./data/rld.csv")

df <- read.csv("./data/D02W_POST_rld.csv")

# the values are actuall proportion of grid cells from graph that are "filled" they are 4 x 4 so divided by 16 for ratio
# df$rld <- df$rld / 16
# ()
# max.vai <- 8
# transect.length <- 10


df$rld[df$rld == 0] <- NA

################

#split into list of data frames
df.list <- split(df, list(df$x, df$y))


### trying with tidy

df %>%
  mutate(x = as.factor(x)) %>%
  mutate(y == as.factor(y)) %>%
  group_by(x, y) %>%
  
  
df.list <- lapply(df.list, function(x){
  for (i in seq_along(x)) {
      # x$biomass_a[j] = x$biomass_b_kg_new[j - 1]
      # x$biomass_b_kg_new[j] = (x$biomass_a[j] * x$RGR_obs_est[j]) + x$biomass_a[j]
      # model for beta
      z <- (x[i]$z[i] * -1)
      rld <- x[i]$rld
      
      #nls model
      m <- nls(y ~ -(beta^rld), start = list(beta = 1))
      
      # add to date frame
      x$model.coef[i] <- as.numeric(coef(m))
      x$model.se[i] <-summary(m)$coefficients[, 2]
    }
    return(x)
  })

df2 <- plyr::ldply(df.list, data.frame)

df2$x <- as.factor(df2$x)
df2$y <- as.factor(df2$y)

df2 %>%
  dplyr::select(x, y, model.coef, model.se) %>%
  dplyr::group_by(x, y) %>%
  data.frame() -> df.model.output


#####

res <- mapply(function(x,y){
  
  nls(y~(a/b)*(1-exp(-b*x)),
      start=list(a=1, b=0.1),
      trace= TRUE, data=data.frame(x, y))
},L1,L2, SIMPLIFY=FALSE)


################

df %>%
  filter(y == 25) %>%
  filter(x == 25) %>%
  data.frame() -> g
  
g$z <- g$z * -1

# model fit
rld <- g$rld
y <- g$z
 
m <- nls(y ~ -(beta^rld), start = list(beta = 1))

# from the model
plot( rld, predict(m))

# now I want to add those predicted values to the dataset
model.coef <- as.numeric(coef(m))
model.se <-summary(m)$coefficients[, 2]


g$predlm <- predict(m)

# plotting
x11()
ggplot(g, aes(x = rld, y = z))+
  geom_point()+
  # geom_smooth(method = "nls",
  #             formula = y ~  -(beta^x),
  #             method.args = list(start = list(beta = 1)),
  #             se = FALSE,
  #             color = "blue")+
  #stat_function(fun = function(x)  -1^x)
  geom_smooth(method = "nls",
              formula = y ~  -(beta^x),
              method.args = list(start = list(beta = 1)),
              se = FALSE,
              color = "blue")+
  geom_smooth(color = "red")+
  geom_line(aes(y = predlm), size = 1, color = "green")




x11()
# vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
ggplot2::ggplot(df.21, ggplot2::aes(x = x, y = z))+
  ggplot2::geom_tile(ggplot2::aes(fill = rld))+
  scale_fill_viridis(option = "D",
                     na.value = "white",
                     direction = -1,
                     )+
  # ggplot2::scale_fill_gradient(low="purple", high="yellow",
  #                              na.value = "white",
  #                              limits=c(0, max.vai),
  #                              name=vai.label)+
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  theme(legend.position = "top")+
  # ggplot2::xlim(10,100)+
  scale_y_reverse( lim=c(1,0))+
  ggplot2::xlab("Distance along transect (10 cm)")+
  ggplot2::ylab("Depth below ground (10 cm)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 6, height = 5)
p

#### 

df.z <- aggregate(rld ~ z, data = df.21, FUN = mean )

pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = rld, x = z))+
  #geom_bar(stat = "identity", color = "light grey")+
  # geom_ribbon(aes(ymin = df.z.vai$vai - df.z.vai$sd.vai, ymax = df.z.vai$vai + df.z.vai$sd.vai),
  #             alpha=0.2) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, formula = y ~ splines::ns(x, 2))+
  #geom_path(size = 1) +
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x= ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank(),
                 legend.title=element_blank())+
  ggplot2::coord_flip(xlim = c(0, 1), ylim = c(0, 70000), expand = TRUE)+
  scale_x_reverse( lim=c(1,0))+
  scale_colour_manual(name="legend", values=c("blue", "red"))+
  #ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ylab("RLD")+
  ggplot2::xlab("Depth below ground (10 cm)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 5, height = 8)
pavd

#### root complexity measures
#mean column leaf height that is the "heightBin" from Matlab code

m <- df.21
# put zeros back
m[is.na(m)] <- 0

# Rename a column in R
colnames(m)[colnames(m)=="x"] <- "xbin"
colnames(m)[colnames(m)=="z"] <- "zbin"


m$rld.z <- m$rld * (m$zbin +0.5)

h <- stats::setNames(stats::aggregate(rld.z ~ xbin, data = m, FUN = sum, na.rm = FALSE,  na.action = 'na.pass'), c("xbin", "rld.z.sum"))

e <- stats::setNames(stats::aggregate(rld ~ xbin, data = m, FUN = sum, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sum.rld"))

# this section joins all these guys together
p <- plyr::join_all(list(m, e, h), by = "xbin", type = "full")


p$height.bin <- p$rld.z.sum / p$sum.rld

#### Addtional summary data
# c <- stats::setNames(stats::aggregate(return_distance ~ xbin, data = df, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.ht"))

# maximum value of VAI in the column
d <- stats::setNames(stats::aggregate(rld ~ xbin, data = m, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.rld"))

# standard deviation of VAI for column
f <- stats::setNames(stats::aggregate(rld ~ xbin, data = m, FUN = stats::sd, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sd.rld"))

# this is height at which max vai occurs
g <- m$zbin[match(d$max.rld, m$rld)]
g <- data.frame(g)

colnames(g) <- c("max.rld.z")

q <- plyr::join_all(list(p, d, f), by = "xbin", type = "full")

q <- q[with(q, order(xbin)), ]
q <- cbind(q, g)


######
m <- q
df <- m
transect.length = max(m$xbin)

# first we create the std.bin numerator
df$std.bin.num <- (((df$zbin + 0.5) - df$height.bin)^2) * df$rld

j <- stats::aggregate(std.bin.num ~ xbin, data = df, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
#print(j)
j[is.na(j)] <- 0

super.size <- merge(m, j, by = "xbin")
#print(super.size[5,])

super.size$std.bin <- super.size$std.bin.num / super.size$sum.rld

super.size$std.bin.squared <- (super.size$std.bin^2)

super.size[is.na(super.size)] <- 0
#print(super.size)
std.std = mean(super.size$std.bin.squared)
# std.std = ((mean(super.size$std.bin^2))) / transect.length

# mean.std = mean(super.size$std.bin)
mean.std = mean(super.size$std.bin)



# HEIGHT VARIABLES
message("HEIGHT METRICS")

mean.height = mean(m$height.bin)
message("Mean Leaf Height (H) - plot mean of column mean leaf height")
print(mean.height)


height.2 <- stats::sd(m$height.bin)
message("Height2 (H[2]) - standard deviation of column mean leaf height")
print(height.2)

mean.height.var = stats::var(m$height.bin)
message("Mean Leaf Height variance (H[var]) - variance of column mean leaf height")
print(mean.height.var)

mean.height.rms = sqrt(mean(m$height.bin^2))
message("Root Mean Square Mean Leaf Height (H[rms]) - the root mean square or quadratic mean of column mean leaf height for the transect")
print(mean.height.rms)


message("AREA AND DENSITY METRICS")

mean.vai = mean(m$sum.rld)
message("Mean VAI - mean VAI for entire transect")
print(mean.vai)

mode.el = mean(m$max.vai.z)
message("Mean Height of VAI[max] - modeEl")
print(mode.el)

mode.2 <- stats::sd(m$max.vai.z)
message("Mode 2- The standard deviation of VAImax or MaxEl")
print(mode.2)

max.el = max(m$max.vai)
message("Maximum VAI for entire transect -- max el!")
print(max.el)

mean.peak.vai = mean(m$max.vai)
message("Mean Peak VAI for entire transect")
print(mean.peak.vai)

message("ARRANGEMENT METRICS")
porosity = sum(m$vai == 0) / length(m$vai)
message("Canopy porosity")
print(porosity)

message("HETEROGENEITY METRICS")


message("Square of leaf height variance (stdStd from old script)")
print(std.std)


message("Mean Standard deviation of leaf heights -- meanStd")
print(mean.std)

rugosity = (std.std - mean.std * mean.std)^0.5
message("Canopy Rugosity")
print(rugosity)


variable.list <- list(plot = filename,
                      transect.length = transect.length,
                      mean.height = mean.height,
                      height.2 = height.2,
                      mean.height.var = mean.height.var,
                      mean.height.rms = mean.height.rms,
                      mode.el = mode.el,
                      max.el = max.el,
                      mode.2 = mode.2,
                      mean.vai = mean.vai,
                      mean.peak.vai = mean.peak.vai,
                      porosity = porosity,
                      std.std = std.std,
                      mean.std = mean.std,
                      rugosity = rugosity)


#now to write to csv
variable.list <- data.frame(variable.list)
return(variable.list)


