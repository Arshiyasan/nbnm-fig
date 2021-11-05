library("tidyverse")
library("ggraph")
library("igraph")
library("tidygraph")
library("RColorBrewer")

l_reg = c("l-Posterior Cingulate Cortex", "l-Ventromedial Prefrontal Cortex", "l-Lateral Temporal Cortex",
          "l-Inferior Parietal Lobule", "l-Precuneus",
          "l-Dorsolateral Prefrontal Cortex", "l-Ventrolateral Prefrontal Cortex", "l-Lateral Parietal Cortex",
          "l-Striatum", "l-Parahippocampus", "l-Nucleus Accumbens",
          "l-Inferior Frontal Gyrus", "l-Anterior Cingulate Cortex", "l-Insula")
r_reg = c("r-Posterior Cingulate Cortex", "r-Ventromedial Prefrontal Cortex", "r-Lateral Temporal Cortex",
          "r-Inferior Parietal Lobule", "r-Precuneus",
          "r-Dorsolateral Prefrontal Cortex", "r-Ventrolateral Prefrontal Cortex", "r-Lateral Parietal Cortex",
          "r-Striatum", "r-Parahippocampus", "r-Nucleus Accumbens",
          "r-Inferior Frontal Gyrus", "r-Anterior Cingulate Cortex", "r-Insula")
nets = c("Default Mode Network", "Default Mode Network", "Default Mode Network",
         "Default Mode Network", "Default Mode Network", "Executive Control Network",
         "Executive Control Network", "Executive Control Network", "Limbic Network",
         "Limbic Network", "Limbic Network", "Salience Network", "Salience Network",
         "Salience Network")

d1 <- data.frame(from="origin", to=c("Left", "Right"))
d2 <- data.frame(from=rep(d1$to, each=4), to=c(paste("l-", unique(nets)), rev(paste("r-", unique(nets)))))
d3 <- data.frame(from=c(paste("l-", nets), rev(paste("r-", nets))), to=c(l_reg, rev(r_reg)))

hierarchy <- rbind(d1, d2, d3)

vertices = data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))))
vertices$Group <- hierarchy$from[match(vertices$name, hierarchy$to)]

connect <- data.frame(From=c("l-Dorsolateral Prefrontal Cortex",
                             "r-Posterior Cingulate Cortex", "r-Posterior Cingulate Cortex",
                             "r-Posterior Cingulate Cortex", "r-Posterior Cingulate Cortex",
                             "r-Posterior Cingulate Cortex", "l-Posterior Cingulate Cortex",
                             "l-Posterior Cingulate Cortex", "l-Posterior Cingulate Cortex",
                             "l-Posterior Cingulate Cortex", "l-Posterior Cingulate Cortex",
                             "r-Dorsolateral Prefrontal Cortex", "r-Dorsolateral Prefrontal Cortex",
                             "r-Insula", "r-Insula", "r-Insula",
                             "l-Anterior Cingulate Cortex", "l-Anterior Cingulate Cortex",
                             "r-Anterior Cingulate Cortex",
                             "r-Lateral Parietal Cortex",
                             "r-Ventromedial Prefrontal Cortex", "l-Ventromedial Prefrontal Cortex",
                             "r-Ventromedial Prefrontal Cortex", "l-Ventromedial Prefrontal Cortex",
                             "r-Ventrolateral Prefrontal Cortex", "r-Ventrolateral Prefrontal Cortex",
                             "l-Dorsolateral Prefrontal Cortex",
                             "l-Ventromedial Prefrontal Cortex", "l-Ventromedial Prefrontal Cortex", 
                             "l-Ventromedial Prefrontal Cortex", "l-Ventromedial Prefrontal Cortex",
                             "l-Ventromedial Prefrontal Cortex", "l-Ventromedial Prefrontal Cortex",
                             "l-Ventromedial Prefrontal Cortex", "l-Ventromedial Prefrontal Cortex",
                             "r-Insula", "l-Insula",
                             "r-Insula", "l-Insula", "r-Insula", "l-Insula", "r-Insula", "l-Insula",
                             "r-Insula", "l-Insula", "l-Dorsolateral Prefrontal Cortex",
                             "r-Anterior Cingulate Cortex",
                             "r-Striatum", "r-Striatum", "r-Striatum", "l-Striatum", "l-Striatum",
                             "r-Nucleus Accumbens"),
                      To=c("r-Parahippocampus",
                           "r-Lateral Temporal Cortex", "r-Inferior Parietal Lobule", "r-Precuneus",
                           "l-Lateral Temporal Cortex", "l-Inferior Frontal Gyrus", "r-Lateral Temporal Cortex",
                           "r-Inferior Parietal Lobule", "r-Precuneus", "l-Lateral Temporal Cortex",
                           "l-Inferior Frontal Gyrus", "l-Lateral Temporal Cortex", "r-Lateral Parietal Cortex",
                           "r-Lateral Temporal Cortex", "r-Posterior Cingulate Cortex", "r-Precuneus",
                           "r-Anterior Cingulate Cortex", "r-Dorsolateral Prefrontal Cortex",
                           "r-Dorsolateral Prefrontal Cortex",
                           "r-Dorsolateral Prefrontal Cortex",
                           "l-Dorsolateral Prefrontal Cortex", "l-Dorsolateral Prefrontal Cortex",
                           "r-Inferior Parietal Lobule", "r-Inferior Parietal Lobule",
                           "r-Lateral Parietal Cortex", "r-Dorsolateral Prefrontal Cortex",
                           "l-Ventromedial Prefrontal Cortex",
                           "l-Striatum", "r-Striatum", "l-Insula", "r-Insula", "l-Anterior Cingulate Cortex",
                           "r-Anterior Cingulate Cortex", "l-Nucleus Accumbens", "r-Nucleus Accumbens",
                           "l-Precuneus", "r-Posterior Cingulate Cortex",
                           "r-Inferior Parietal Lobule", "r-Inferior Parietal Lobule",
                           "l-Lateral Temporal Cortex", "l-Lateral Temporal Cortex", "r-Precuneus", "r-Precuneus",
                           "r-Lateral Parietal Cortex", "r-Lateral Parietal Cortex", "l-Lateral Parietal Cortex",
                           "l-Posterior Cingulate Cortex",
                           "l-Striatum", "r-Nucleus Accumbens", "l-Nucleus Accumbens", "r-Nucleus Accumbens",
                           "l-Nucleus Accumbens", "l-Nucleus Accumbens"),
                      Study=c("Yang et al., 2017",
                              "Shahbabaie et al., 2018", "Shahbabaie et al., 2018", "Shahbabaie et al., 2018",
                              "Shahbabaie et al., 2018", "Shahbabaie et al., 2018", "Shahbabaie et al., 2018", 
                              "Shahbabaie et al., 2018", "Shahbabaie et al., 2018", "Shahbabaie et al., 2018",
                              "Shahbabaie et al., 2018", "Shahbabaie et al., 2018", "Shahbabaie et al., 2018",
                              "Shahbabaie et al., 2018", "Shahbabaie et al., 2018", "Shahbabaie et al., 2018",
                              "Holla et al., 2020", "Holla et al., 2020", "Holla et al., 2020",
                              "Bouchard et al., 2021",
                              "Garza-Villarreal et al., 2021", "Garza-Villarreal et al., 2021",
                              "Garza-Villarreal et al., 2021", "Garza-Villarreal et al., 2021",
                              "Jansen et al., 2015", "Jansen et al., 2015",
                              "Xingbao Li et al., 2017",
                              "Kearney-Ramos et al., 2018", "Kearney-Ramos et al., 2018",
                              "Kearney-Ramos et al., 2018", "Kearney-Ramos et al., 2018",
                              "Kearney-Ramos et al., 2018", "Kearney-Ramos et al., 2018",
                              "Kearney-Ramos et al., 2018", "Kearney-Ramos et al., 2018",
                              "Perini et al., 2020", "Perini et al., 2020",
                              "Su et al., 2020", "Su et al., 2020", "Su et al., 2020", "Su et al., 2020",
                              "Su et al., 2020", "Su et al., 2020", "Su et al., 2020", "Su et al., 2020",
                              "Su et al., 2020",
                              "De Ridder et al., 2011",
                              "Kearney-Ramos et al., 2019", "Kearney-Ramos et al., 2019",
                              "Kearney-Ramos et al., 2019", "Kearney-Ramos et al., 2019",
                              "Kearney-Ramos et al., 2019", "Kearney-Ramos et al., 2019"),
                      Type=c("Enhanced", "Decreased", "Decreased", "Decreased", "Decreased",
                             "Decreased","Decreased", "Decreased", "Decreased", "Decreased",
                             "Decreased", "Enhanced", "Enhanced", "Enhanced", "Enhanced",
                             "Enhanced", "Enhanced", "Enhanced", "Enhanced", "Enhanced",
                             "Enhanced", "Enhanced", "Enhanced", "Enhanced", "Enhanced",
                             "Enhanced", "Decreased", "Decreased", "Decreased", "Decreased",
                             "Decreased", "Decreased", "Decreased", "Decreased", "Decreased",
                             "Decreased", "Enhanced", "Decreased", "Decreased", "Decreased",
                             "Decreased", "Decreased", "Decreased", "Decreased", "Decreased",
                             "Enhanced", "Decreased", "State-Dependent", "State-Dependent",
                             "State-Dependent", "State-Dependent", "State-Dependent", "State-Dependent"))

connect$Type <- as_factor(connect$Type)

from = match(connect$From, vertices$name)
to = match(connect$To, vertices$name)

vertices$id <- NA
myleaves <- which(is.na( match(vertices$name, hierarchy$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)

vertices$angle <- 6.428571428571429 + 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse(vertices$angle < -90 , 1, 0)

# flip angle BY to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

graph <- graph_from_data_frame(hierarchy, vertices = vertices)

layout <- create_layout(graph, layout = "dendrogram", offset=pi/3, circular = TRUE)

x1=0
y1=0
x2 <- 1/3*cos(-0.1121997)
y2 <- 1/3*sin(-0.1121997)
x3=-x2
y3=y2
x4 <- 2/3*cos(pi/2-0.5609987)
y4 <- 2/3*sin(pi/2-0.5609987)
x11=-x4
y11=y4
x5 <- 2/3*cos(0.1121995)
y5 <- 2/3*sin(0.1121995)
x10=-x5
y10=y5
x6 <- 2/3*cos((3*pi/2)+1.009798)
y6 <- 2/3*sin((3*pi/2)+1.009798)
x9=-x6
y9=y6
x7 <- 2/3*cos((3*pi/2)+0.3365992)
y7 <- 2/3*sin((3*pi/2)+0.3365992)
x8=-x7
y8=y7

n <- 28
pts.circle <- t(sapply(1:n,function(r)c(cos((2*r*pi/n)+(pi/n)),sin((2*r*pi/n)+(pi/n)))))

layout[1,1:2]=c(x1,y1)
layout[2,1:2]=c(x2,y2)
layout[3,1:2]=c(x3,y3)
layout[4,1:2]=c(x4,y4)
layout[5,1:2]=c(x5,y5)
layout[6,1:2]=c(x6,y6)
layout[7,1:2]=c(x7,y7)
layout[8,1:2]=c(x8,y8)
layout[9,1:2]=c(x9,y9)
layout[10,1:2]=c(x10,y10)
layout[11,1:2]=c(x11,y11)

layout[12,1:2] = pts.circle[6,]
layout[13,1:2] = pts.circle[5,]
layout[14,1:2] = pts.circle[4,]
layout[15,1:2] = pts.circle[3,]
layout[16,1:2] = pts.circle[2,]
layout[17,1:2] = pts.circle[1,]
layout[18,1:2] = pts.circle[28,]
layout[19,1:2] = pts.circle[27,]
layout[20,1:2] = pts.circle[26,]
layout[21,1:2] = pts.circle[25,]
layout[22,1:2] = pts.circle[24,]
layout[23,1:2] = pts.circle[23,]
layout[24,1:2] = pts.circle[22,]
layout[25,1:2] = pts.circle[21,]
layout[26,1:2] = pts.circle[20,]
layout[27,1:2] = pts.circle[19,]
layout[28,1:2] = pts.circle[18,]
layout[29,1:2] = pts.circle[17,]
layout[30,1:2] = pts.circle[16,]
layout[31,1:2] = pts.circle[15,]
layout[32,1:2] = pts.circle[14,]
layout[33,1:2] = pts.circle[13,]
layout[34,1:2] = pts.circle[12,]
layout[35,1:2] = pts.circle[11,]
layout[36,1:2] = pts.circle[10,]
layout[37,1:2] = pts.circle[9,]
layout[38,1:2] = pts.circle[8,]
layout[39,1:2] = pts.circle[7,]
layout$x=layout$x*5
layout$y=layout$y*5


Image <- ggraph(layout)+ 
  geom_conn_bundle(data = get_con(from = from,
                                  to = to,
                                  Study = connect$Study,
                                  Connectivity_Alteration = connect$Type),
                   aes(colour=Study, linetype=Connectivity_Alteration),
                   tension = 0.8, width =0.8)+
  scale_edge_colour_brewer(type = "qual", palette = 3, direction=-1)+
  
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05))+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name,
                     angle = angle, hjust=hjust), size=2, alpha=1,
                 family="AvantGarde", fontface= "bold") +
  theme_void() +
  guides(edge_color = guide_legend(override.aes = list(edge_width = 5))) +
  
  theme(
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-8, 8), y = c(-8, 8))
Image
ggsave("my_plot.jpeg", width = 10, heigh =8, dpi=600)


