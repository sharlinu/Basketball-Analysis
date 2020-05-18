library(dplyr)
library(gridExtra)
library(ggplot2)
library(plotly)
#install.packages("gridExtra")

# 2 dimensional plotting function - ggplot
twod = function(data, data.var, w.var){
  df <- data[, data.var]
  names(df) <- c("x","y")
  w <- data[, w.var]
  if (is.character(w)) {
    w = factor(w)
    df$w = w }
  else if (is.factor(w) | is.numeric(w)) {
    df$w = w}
  
  p <- ggplot(data=df, aes(x=x, y=y, color=w))
  p <- p + geom_point()
  if (is.factor(w)) {
    p = p + scale_color_manual(name=w.var, values=topo.colors(length(unique(w))))}
  else if (!is.factor(w)){
    p = p + scale_color_gradientn(name=w.var, colors=topo.colors(length(unique(w))))}
  p <- p + labs(title="", x="", y="") + theme_light()
  return(p)
}

#3 dimensional plotting function - ggplot
threed = function(data, data.var, w.var){
  
  df <- data[, data.var]
  names(df) <- c("x","y","z")
  w <- data[, w.var]
  if (is.character(w)) {
    w = factor(w)
    df$w = w }
  else if (is.factor(w) | is.numeric(w)) {
    df$w = w}
  
  p = ggplot(df, aes(x=x,y=y,z=z, color=w)) +
    axes_3D() +
    stat_3D() +
    theme_light() 
  if (is.factor(w)) {
    p = p + scale_color_manual(name=w.var, values=topo.colors(length(unique(w))))}
  else if (!is.factor(w)){
    p = p + scale_color_gradientn(name=w.var, colors=topo.colors(length(unique(w))))}

  return(p)
}


#################################################################################################
# Plotting all variables that we used in the clustering on two-dimensional MDS plots
# since these plots are meant to be a visualization, we choose two-dimensional plots here since they are more compact 

# preparing the dataset
df = pbox
df = dplyr::rename(df,difficulty=diff, ORp = ORp2, DRp = DRp2)
df[is.na(df)] = 0

# selecting all players that played more than 500 minutes and only the variables we included in the clustering
mMIN = 500
df = subset(df, MIN>=mMIN)
id = df$Player
df_select = subset(df,  select =c("FTp", "P2p", "P3p", "difficulty",  "ORp", "DRp", "USGp", "ASTp", "FGM_ASTp", "TPP", "STLm", "BLKm"))

# Calculate MDS and the points for the reduced two dimensions
points = MDS(df_select, 2, std=TRUE)[["points"]]
points = data.frame(points)
colnames(points) = c("X1","X2")

#add them to the end of our dataset
df = cbind(df, points)

# plotting all variables on this reduced dimensional space and arranging them in a 2x2 plot
p1 = twod(df, c("X1","X2"), "FTp")
p2 = twod(df, c("X1","X2"), "P2p")
p3 = twod(df, c("X1","X2"), "P3p")
p4 = twod(df, c("X1","X2"), "difficulty")
grid.arrange(p1, p2, p3, p4, nrow = 2)

o1 = twod(df, c("X1","X2"), "ORp")
o2 = twod(df, c("X1","X2"), "DRp")
o3 = twod(df, c("X1","X2"), "USGp")
o4 = twod(df, c("X1","X2"), "TPP")
grid.arrange(o1, o2, o3, o4, nrow = 2)

q1 = twod(df, c("X1","X2"), "ASTp")
q2 = twod(df, c("X1","X2"), "FGM_ASTp")
q3 = twod(df, c("X1","X2"), "STLm")
q4 = twod(df, c("X1","X2"), "BLKm")

grid.arrange(q1, q2, q3, q4, nrow = 2)



################################################################################################
# 3 dimensional interactive plots (plotly)

# here we are predefining how we want our axis to be displayed.
# we chose a minimalistic visualization since the axis depict non-interpretable values 
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = TRUE
)

# Plots an interactive plot on a 3-dimensional axis 
# x, y, z defining the axis, color defining the color scale of the nodes
fig = plot_ly(df, x = ~X1, y = ~X2, z = ~X3, color = ~MIN, 
              marker = list(size = 5),
              # we want to be able to get more information about every node by clicking on it 
              hoverinfo = 'text',
              # in this example we chose to display to arguements: the name of the player and total minutes played
              text = ~paste('</br> Player: ', Player,
                             '</br> Cluster: ', MIN))
# adding the predefined axis to the plot
fig = fig %>% layout(scene = list(xaxis= ax, yaxis = ax, zaxis=ax)) 
# adding the points as markers to the plot
fig = fig %>% add_markers()
fig


ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = TRUE
)



fig = plot_ly(df, x = ~X1, y = ~X2, z =~X3, color = ~bic_clust, 
              colors = colorRampPalette(brewer.pal(10,"Spectral"))(41),
              marker = list(size = 5),
              hoverinfo = 'text',
              text = ~paste('</br> Player: ', Player,
                            '</br> Cluster: ', bic_clust))
fig = fig %>% layout(scene = list(xaxis= ax, yaxis = ax, zaxis=ax), legend=list( title=list(text='<b> Cluster </b>'))) 
fig = fig %>% add_markers()
fig 

# custom grid style
axx <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = TRUE
)

# individual plots
fig1 <- plot_ly(points,x =~X1,y =~X2, z=~X3, color=~ASTp, scene='scene1',
                marker = list(size = 5),
                # we want to be able to get more information about every node by clicking on it 
                hoverinfo = 'text',
                # in this example we chose to display to arguements: the name of the player and total minutes played
                text = ~paste('</br> Player: ', ID)) 

fig2 <- plot_ly(points, x = ~X1, y=~X2, z=~X3, color =~BLKm, scene='scene2',
                marker = list(size = 5),
                hoverinfo = 'text',
                text = ~paste('</br> Player: ', ID)) 

fig3 <- plot_ly(z = ~volcano, scene='scene3') 
fig3 <- fig3 %>% add_surface(showscale=FALSE)

fig4 <- plot_ly(z = ~volcano, scene='scene4') 
fig4 <- fig4 %>% add_surface(showscale=FALSE)

# subplot and define scene
fig <- subplot(fig1, fig2, fig3, fig4) 
fig <- fig %>% layout(title = "3D Subplots",
                      scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),
                                   xaxis=axx, yaxis=axx, zaxis=axx,
                                   aspectmode='cube'),
                      scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),
                                    xaxis=axx, yaxis=axx, zaxis=axx,
                                    aspectmode='cube'),
                      scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),
                                    xaxis=axx, yaxis=axx, zaxis=axx,
                                    aspectmode='cube'),
                      scene4 = list(domain=list(x=c(0.5,1),y=c(0,0.5)),
                                    xaxis=axx, yaxis=axx, zaxis=axx,
                                    aspectmode='cube'))
fig