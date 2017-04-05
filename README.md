# Manhattan_Plot_New
```
## DATA

filter = read.table("freqdata.count.after.clean", header = F)
colnames(filter) = c("Chromosome", "Frequency", "Filter")
filter1 = filter[1:58990,]; f1 = filter1$Filter == 0; table(f1)
f = filter$Filter == 0; table(f)

gblup = read.table("snp_sol", header = F); dim(gblup)
colnames(gblup) = c("Trait","Effect","SNP","Chr","Pos",
"Solution","Weight","Variance")
ngblup = gblup[f,]; dim(ngblup)
ngblup$within = seq(1:nrow(ngblup))

## MANHATTHAN PLOT GBLUP

library(ggplot2)
gap = 0; xx = as.numeric(); x0 = 0

for(w in 1:29){
x1 = table(ngblup$Chr)[[w]]
x2 = gap + 0.5 * x1
x3 = x0 + x2
xx[w] = x3
x0 = x3 + 0.5 * x1
}

name = seq(1:29)

colo <- c(rep(c("blue2","darkorange2"),14),"blue2")
c <- ggplot(ngblup, aes(x = within, y = Variance, col = factor(Chr)))
c + scale_color_manual(values = colo) + 
geom_point( ) + ylim(0,1.0) +
ylab("% Genetic Variance\n") + xlab("\nChromosome") +
theme(axis.title.x = element_text(face = "bold", size = 16)) + 
theme(axis.title.y = element_text(face = "bold", size = 16)) +
theme(axis.text.x  = element_text(size = 12)) + 
theme(axis.text.y  = element_text(size = 14)) +
scale_x_continuous(breaks = xx, labels = name) + theme_bw() +
theme(legend.position = "none", panel.grid.minor.x=element_blank(),
           panel.grid.major.x=element_blank(), 
panel.grid.minor.y=element_blank(),
           panel.grid.major.y=element_blank(),
axis.title.y = element_text(face="bold", size=16),
axis.title.x = element_text(face="bold", size=16),
axis.text.x  = element_text(size=12),
axis.text.y  = element_text(size=14))

```
############ Subsetting a dataframe ##########################################
```
ef3=subset(snp, Effect ==3)
> ef4=subset(snp, Effect ==4)
```
