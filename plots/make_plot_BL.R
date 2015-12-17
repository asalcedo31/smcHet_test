library(BoutrosLab.plotting.general)
library(plyr)
tsv.dir = "scoring_metric_data/text_files/"
plot.dir = "/u/asalcedo/SMC-HET-Paper1/plots/"
tsv_dir = tsv.dir
plot_dir = plot.dir


setwd("/u/asalcedo/SMC-HET-Paper1/")
da = read.csv("plots/scoring1A_behavior.tsv",sep="\t",header=FALSE)
colnames(da) = c("Real","Pred","Error")


## 1A Beta plot ###################################################################################
real.legend <- list(
  legend = list(
    colours = default.colours(length(unique(da$Real))),
    labels = as.character(seq(0.1,1,0.1)),
    title = 'Actual Cellularity',
    border = 'transparent',
    size = 1
    )
  );
real.legend.grob <- legend.grob(
  legends = real.legend,
  title.cex=0.5,
  label.cex=0.5
  );
create.scatterplot(
  formula = Error ~ Pred,
  data = da,
  filename = "1A_beta",
  groups = da$Real,
  xlab.label = ("Predicted Cellularity"),
  ylab.label = ("Cellularity Error"),
  ylim = c(-0.0,1.01),
  xlim = c(-0.0,1.01),
  yat = seq(0,1,0.2),
  xat = seq(0,1,0.2),
  xlab.cex = 0.7,
  ylab.cex = 0.7,
  yaxis.cex = 0.5,
  xaxis.cex = 0.5,
  height = 3,
  type=c('p','l'),
  #add.xyline=TRUE,
  width = 4.49,
  resolution = 400,
  cex=0.2,
  legend = list(
    right = list( fun = real.legend.grob)
    ),
  col = default.colours(length(unique(da$Real))),
   style = 'Nature'
  )
## 1b beta plot ###################################################################################

db = read.csv("plots/scoring1B_behavior.tsv",sep="\t",header=FALSE)
colnames(db) = c("Real","Pred","Error")
png(file="1B.png")
ggplot(db,aes(x=Pred,y=Error,color=as.factor(Real))) + geom_point() + geom_line() + xlab("Predicted Number of Clusters") + 
  ylab("Cluster Error") + scale_color_discrete(name="Actual Number of Clusters") + ggtitle("1B Scoring")
dev.off()

real.legend <- list(
  legend = list(
    colours = default.colours(length(unique(db$Real))),
    labels = as.character(seq(1,5)),
    title = 'Actual Number of Clusters',
    border = 'transparent',
    size = 1
  )
);
real.legend.grob <- legend.grob(
  legends = real.legend,
  title.cex=0.5,
  label.cex=0.5
);
create.scatterplot(
  formula = Error ~ Pred,
  data = db,
  filename = "1b_beta",
  groups = db$Real,
  xlab.label = ("Predicted Number of Clusters"),
  ylab.label = ("Cluster Error"),
  ylim = c(-0.0,1.01),
  xlim = c(1,10),
  yat = seq(0,1,0.2),
  xat = seq(1,10),
  xlab.cex = 0.7,
  ylab.cex = 0.7,
  yaxis.cex = 0.5,
  xaxis.cex = 0.5,
  height = 3,
  type = c('p','l'),
  width = 4.49,
  resolution = 400,
  cex=0.2,
  legend = list(
    right = list( fun = real.legend.grob),
    y=1
  ),
  col = default.colours(length(unique(da$Real))),
  style = 'Nature'
)

## 1c beta plot phy sys error ###################################################################################

d = read.csv("plots/scoring1C_phi_sys_behavior.tsv",sep="\t",header=FALSE)
colnames(d) = c("Error","Metric")
png(file="1C_phi_systematic.png")
ggplot(d,aes(x=Error,y=Metric)) + geom_point() + xlab("Systematic Phi Error") + ylab("1C Metric") + 
  ggtitle("Systematic Phi Error")
dev.off()

create.scatterplot(
  formula = Metric ~ Error,
  data = d,
  filename = "1c_phi_sys_error",
  xlab.label = ("Systematic Phi Error"),
  ylab.label = ("1C Metric"),
  ylim = c(0.9,1),
  xlim = c(-0.1,0.1),
  yat = seq(0.9,1,0.025),
  xat = seq(-0.1,0.10,0.05),
  xlab.cex = 0.7,
  ylab.cex = 0.7,
  yaxis.cex = 0.5,
  xaxis.cex = 0.5,
  height = 3,
  width = 4.49,
  resolution = 400,
  cex=0.2,
    style = 'Nature'
)

## 1c Zero Mean Phi Noise ###################################################################################


d = read.csv("plots/scoring1C_phi_ZM_behavior.tsv",sep="\t",head=FALSE)
colnames(d) = c("Concentration","PhisUsed","Metric")
png(file="1C_phi_ZM.png")
ggplot(d, aes(x=as.ordered(Concentration), y=as.numeric(Metric))) + 
  geom_jitter(aes(color=as.ordered(Concentration)),position = position_jitter(height = 0, width=0.05)) +
  stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, geom="crossbar", width=0.7) +
  theme(legend.position="none") + xlab("Concentration Parameter") + ylab("1C Metric") + 
  ggtitle("Zero-mean Phi Noise")
dev.off()
d$Concentration<-as.factor(d$Concentration)
medians<-ddply(d, .(Concentration), summarize, Median=median(Metric))

create.stripplot(
  formula = Metric ~ Concentration,
  data = d,
  filename = "1c_zero_mean_phi_noise",
  xlab.label = ("Concentration Parameter"),
  ylab.label = ("1C Metric"),
  ylim = c(0.5,1),
  #xlim = c(0,1000),
  yat = seq(0.5,1,0.1),
#  xat = NULL,
  xlab.cex = 0.7,
  ylab.cex = 0.7,
  yaxis.cex = 0.5,
  xaxis.cex = 0.5,
  height = 3,
  width = 4,
  add.median = TRUE,
  median.values = medians$Median,
  resolution = 400,
  cex = 0.16,
  jitter.data=TRUE,
  jitter.factor = 0.3,
  style = 'Nature'
)

## 1c Zero Mean Error SSMs ###################################################################################

d = read.csv(file="plots/scoring1C_nssm_behavior.tsv", sep="\t",header=FALSE)
colnames(d) = c("Concentration","NssmsUsed","Metric")
png(file="1C_nssms_ZM.png")
ggplot(d, aes(x=as.ordered(Concentration), y=as.numeric(Metric))) + 
  geom_jitter(aes(color=as.ordered(Concentration)),position = position_jitter(height = 0, width=0.05)) +
  stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, geom="crossbar", width=0.7) +
  theme(legend.position="none") + xlab("Concentration Parameter") + ylab("1C Metric") +
  ggtitle("Zero-mean N_ssms Noise")
dev.off()

d$Concentration<-as.factor(d$Concentration)
medians<-ddply(d, .(Concentration), summarize, Median=median(Metric))

create.stripplot(
  formula = Metric ~ Concentration,
  data = d,
  filename = "1c_zero_mean_n_ssm",
  xlab.label = ("Concentration Parameter"),
  ylab.label = ("1C Metric"),
  ylim = c(0.75,1),
  #xlim = c(0,1000),
  yat = seq(0.75,1,0.05),
  #  xat = NULL,
  xlab.cex = 0.7,
  ylab.cex = 0.7,
  yaxis.cex = 0.5,
  xaxis.cex = 0.5,
  height = 3,
  width = 4,
  add.median = TRUE,
  median.values = medians$Median,
  resolution = 400,
  cex = 0.16,
  jitter.data=TRUE,
  jitter.factor = 0.3,
  style = 'Nature'
)

## 1c Cases ###################################################################################

d = read.csv(file="plots/scoring1C_cases.tsv", sep="\t",header=FALSE)
colnames(d) = c("Case","Metric")
png(file="1C_Cases.png")
ggplot(d,aes(y=1-Metric,x=as.factor(Case))) + 
  geom_bar(aes(fill=as.factor(Case)),stat="identity",width=.6) + 
  theme(legend.position="none") + ylab("1 - 1C Metric") +
  xlab("Case") + ggtitle("1C Cases") + coord_flip()
dev.off()

create.barplot(
  formula = Case ~ 1-Metric,
  data = d,
  plot.horizontal = TRUE,
  filename = "1c_cases",
  yaxis.lab=rev(c("Split3","Split2","Split1","Collapse23","Collapse123","Collapse12","All Clonal" )),
  xlab.cex = 0.7,
  ylab.cex = 0.7,
  yaxis.cex = 0.5,
  xaxis.cex = 0.5,
  xlim=c(0,1),
  xlab.label= c("1 - 1C Metric"),
  height = 3,
  width = 4,
  resolution = 400,
  style= 'Nature'
)

##2A cases barplots multiplot #####################################################################
tsv.dir = "scoring_metric_data/text_files/"
plot.dir = "/u/asalcedo/SMC-HET-Paper1/plots/"
method.names <- c("default",
                  "sqrt",
                  "pseudoV",
                  "sym_pseudoV",
                  "spearman",
                  "pearson",
                  "aupr",
                  "mcc")

barplots.methods <- list();
for (method in method.names){
	d = read.csv(file=paste(tsv.dir, "scoring2A_cases_", method, ".tsv", sep=""), sep="\t",header=FALSE);
	colnames(d) = c("Case","Metric");
	filename = paste(plot.dir, "2A_Cases_", method, ".tiff", sep="");
	d$CaseName <- c("Split Cluster", "Merge Cluster", "One Cluster", "N Clusters", "Small Extra", "Big Extra");
	d <- d[c(4,3,6,1,2,5),]
	rownames(d)<-c(1:6)
	barplots.methods[[method]] <- create.barplot(
		  formula =  Metric ~ CaseName,
		  data = d,
		#  plot.horizontal = TRUE,
		  xlab.cex = 1.2,
		  ylab.cex = 1.2,
		  yaxis.cex = 1,
		  xaxis.cex = 1,
		  ylim = c(-0.2,1.5),
		  ylab.label = c("Error Cases"),
		  xlab.label= c("Metric Score"),
		  height = 4.8,
		  width = 3.35,
		  sample.order = c( "N Clusters","One Cluster", "Big Extra","Split Cluster", "Merge Cluster", "Small Extra") ,
		  col = colour.gradient('slategrey',6),
		  style= 'Nature'
	);
}
	
	png("test_barplot2A_reordered.png")
	print(barplots.methods[['pseudoV']])
	dev.off()

create.multiplot(
	plot.objects = barplots.methods,
	plot.layout = c(2,4),
	filename = "2A_multiplot.tiff",
	y.spacing = -3.5,
	xat = list(c(1:6),c(1:6),c(),c(),c(),c(),c(),c()),
	xaxis.lab = list(c( "N Clusters","One Cluster", "Big Extra","Split Cluster", "Merge Cluster", "Small Extra"),c( "N Clusters","One Cluster", "Big Extra","Split Cluster", "Merge Cluster", "Small Extra") ,c(),c(),c(),c(),c(),c()),
	ylim = c(-0.35,1.4),
	xaxis.cex = 0.6,
	yaxis.cex = 0.6,
	xaxis.rot = 70,
	bottom.padding = 0.1,
	top.padding = 0.2,
	xlab.to.xaxis.padding = 0.1,
	ylab.padding = 0.5,
	left.padding = 0.5,
	ylab.label = "Score",
	ylab.cex = 0.75,,
	right.padding = 0.5,
	height = 4.6,
	width = 3.35,
	style = 'Nature'
	)
	
#### SC2A strip plot#################################################################################
all_metrics <- list()
for (method in method.names){
	d = read.csv(file=paste(tsv.dir, "scoring2A_cases_", method, ".tsv", sep=""), sep="\t",header=FALSE);
	colnames(d) = c("Case","Metric");
	all_metrics[[method]] <- d ;
}

for (method in method.names){
   d = read.csv(file=paste(tsv.dir, "scoring2A_big_cases_", method, ".tsv", sep=""), sep="\t",header=FALSE);
    colnames(d) = c("Case","Metric");
    all_metrics[[method]] <- d ;
}

all_metrics_df <- ldply(all_metrics, function(x) as.data.frame(x), .id = 'method')
all_metrics_df$case_num<-as.numeric(as.character(factor(all_metrics_df$Case,labels=c(3,5,1,2,6,4))))
all_metrics_df<-arrange(all_metrics_df,method,case_num)
create.scatterplot(
	formula = Metric ~ case_num,
	data = all_metrics_df,
	filename = "2A_big_cases_stripplot.png",
	type = c('p','l'),
	groups = all_metrics_df$method,
	col = default.colours(8),
	xaxis.rot = 70,
	xaxis.lab = c( "N Clusters","One Cluster", "Big Extra","Split Cluster", "Merge Cluster", "Small Extra"),
	xat = seq(1,6),
	ylim = c(-0.35,1.2),
	yaxis.cex = 0.6,
	xaxis.cex = 0.7,
	xlab.label = c(),
	ylab.label = 'Score',
	ylab.cex = 0.75,
	key = list (
        points = list(
	        pch = 19,
    	    col = default.colours(8),
    	    cex = 0.3
       ),
		text = list(
			lab = c("Average of MCC, Pearsion, PseudoV","Square-root","PseudoV","Symmetric PseudoV", "Spearman", "Pearson", "AUPR","MCC"),
			cex = 0.65,
			col = 'black',
			padding.text =  -0.2
			),
	x = 0,
	y = 1,
	padding.text = 0.5
),	
	left.padding = 0.1,
	ylab.padding = 0.1,
	bottom.padding = 0.22,
	cex = 0.4,
	height = 4.6,
	width = 3.35,
	style = 'Nature'
	)
#### SC2A Big Cases  #####################################################################
all_metrics <-list()
for (method in method.names){
   d = read.csv(file=paste(tsv.dir, "scoring2A_big_cases_", method, ".tsv", sep=""), sep="\t",header=FALSE);
    colnames(d) = c("Case","Metric");
    all_metrics[[method]] <- d ;
}

all_metrics_df <- ldply(all_metrics, function(x) as.data.frame(x), .id = 'method')
all_metrics_df$case_num<-as.numeric(as.character(factor(all_metrics_df$Case,labels=c(3,5,1,2,6,4))))
all_metrics_df<-arrange(all_metrics_df,method,case_num)
create.scatterplot(
    formula = Metric ~ case_num,
    data = all_metrics_df,
    filename = "2A_big_cases_stripplot",
    type = c('p','l'),
    groups = all_metrics_df$method,
    col = default.colours(8),
    xaxis.rot = 70,
    xaxis.lab = c( "N Clusters","One Cluster", "Big Extra","Split Cluster", "Merge Cluster", "Small Extra"),
    xat = seq(1,6),
    ylim = c(0,1.4),
    yaxis.cex = 0.6,
    xaxis.cex = 0.7,
    xlab.label = c(),
    ylab.label = 'Score',
    ylab.cex = 0.75,
    key = list (
        points = list(
            pch = 19,
            col = default.colours(8),
            cex = 0.3
       ),
        text = list(
            lab = c("Average of MCC, Pearsion, PseudoV","Square-root","PseudoV","Symmetric PseudoV", "Spearman", "Pearson", "AUPR","MCC"),
            cex = 0.6,
            col = 'black',
            padding.text =  -0.2
            ),
    x = 0,
    y = 1,
    padding.text = 0.5
),
    left.padding = 0.1,
    ylab.padding = 0.1,
    bottom.padding = 0.22,
    cex = 0.2,
    height = 4.6,
    width = 3.35,
    style = 'Nature'
    )

####SC2A Random reassignment ######################################################################
method.names <- c("default",
                  "sqrt",
                  "pseudoV",
                  "sym_pseudoV",
                  "spearman",
                  "pearson",
                  "aupr",
                  "mcc")

plot.random.reassignment <- function (method){
	d = read.csv(paste(tsv.dir, "scoring2A_random_reassignment_", method, ".tsv", sep=""),sep="\t",header=FALSE);
	colnames(d) = c("Error","Metric");
	d$Error<-as.ordered(d$Error);
	median.values <- unlist(daply(d, .(Error), summarise, median(Metric)));
	stripplot<-create.stripplot(
		Metric ~ Error,
		data = d,
#		filename = "2A_random_reassignment_BL",
		add.median = TRUE,
		median.values = median.values,
		cex = 0.25,
		colour.alpha = 0.75,
		jitter.data = TRUE,
		jitter.factor = 0.1,
		style = 'Nature'
		)
	return(stripplot)
	}
	
method.plots<-list()
	for (method in method.names){
		method.plots[[method]] <- plot.random.reassignment(method)
	}

create.multiplot(
	plot.objects = method.plots,
	filename = "2A_random_reassignment_multiplot.png",
	plot.layout = c(2,4),
	yaxis.cex = 0.5,
	xaxis.cex = 0.5,
	style = 'Nature'
	)
all_metrics<-list()

for (method in method.names){
	d = read.csv(paste(tsv.dir, "scoring2A_random_reassignment_", method, ".tsv", sep=""),sep="\t",header=FALSE);
	colnames(d) = c("Error","Metric");
	d$Error<-as.ordered(d$Error);
	all_metrics[[method]] <- d
	}
all_metrics_df <- ldply(all_metrics, function(x) as.data.frame(x), .id = 'method')
median.values <- unlist(daply(all_metrics_df, .(Error,method), summarise, median(Metric)));
create.stripplot(
	formula = Metric ~ Error,
	data = all_metrics_df,
	groups =  method,
	filename = "2A_random_test.png",
	col = default.colours(8),
	cex = 0.5,
	colour.alpha = 0,.75,
	jitter.data = TRUE,
	
	
	)

all_metrics_error <- ddply (all_metrics_df, .(Error, method),summarise, mean=mean(Metric), upper= range(Metric)[2], lower = range(Metric)[1])
median_df<-ddply(all_metrics_df, .(Error,method), summarise, median =median(Metric))
create.scatterplot (
	formula = Metric ~ Error,
	data = all_metrics_df,
	group = method,
	col = default.colours(8),
	add.points = TRUE,
	points.pch = 4,
	alpha = 0.15,
	points.x = median_df$Error,
	points.y = median_df$median,
	points.cex = 1.4,
	yaxis.cex = 0.8,
	xaxis.cex = 0.8,
	xlab.cex = 0.95,
	ylab.cex = 0.95,
	points.col = rep(default.colours(8),7),
	filename = "2A_random_test_sp.png",
	ylimits = c(-0.1,1.39),
	key = list (
		points = list(
			pch = 19,
			col = default.colours (8),
			cex = 0.8,
			alpha = 0.5
			),
		text = list(
            lab = c("Average of MCC, Pearson, PseudoV","Square-root","PseudoV","Symmetric PseudoV", "Spearman", "Pearson", "AUPR","MCC"),
            cex = 0.85,
            col = 'black'
            ),
			x = 0.01,
			y = 1,
			padding.text =0.5
			),

	style = 'Nature'
	)

#### SC3 Scoring barplots multiplot################################################################

setwd("/u/asalcedo/SMC-HET-Paper1/smc_het_eval");
bp_pseudoV<-plot.SC3.all(method="pseudoV",colour='turquoise4', title = "PseudoV")$bp
bp_pearson<-plot.SC3.all(method="pearson",colour= 'orangered3', title = "Pearson")$bp
bp_sym_pseudoV<-plot.SC3.all(method="sym_pseudoV",colour='firebrick4', title = "Symmertric PseudoV")$bp
#bp_orig<-plot.SC3.all(method="orig",colour='chartreuse4')$bp
bp_sqrt<-plot.SC3.all(method="sqrt", colour = 'royalblue4', title= "Square-root")$bp
bp_aupr<-plot.SC3.all(method="aupr",colour= 'darkmagenta', title ="AUPR")$bp
bp_mcc<-plot.SC3.all(method="mcc",colour='black', title = "MCC")$bp

legend.metrics1 = list (
  legend=list(
    colours = c('darkmagenta','firebrick4','turquoise4'),
    labels = c("AUPR","Symmertric PseudoV","PseudoV"),
#    title = c("Metrics"),
    size=0.75
    )
  )
  
legend.metrics2 = list (
  legend=list(
	colours = c('black','royalblue4','orangered3'),
	labels = c("MCC","Square-root","Pearson"),
	size=0.5
	)
  )

legend.metrics.grob <-legend.grob(
  legends= c(legend.metrics1,legend.metrics2),
  title.cex=0.5,
  label.cex=0.85,
  layout=c(2,1)
  )
tiff("sc3A_legend.tiff",
	type= 'cairo',
	height = 1,
	width = 4, 
	units= 'in',
	res = 800,
	compression = 'lzw'
	)
grid.draw(legend.metrics.grob)
dev.off()


create.multiplot(
  plot.objects = list(bp_pseudoV, bp_pearson, bp_sym_pseudoV, bp_sqrt, bp_aupr,bp_mcc),
  plot.layout = c(2,3),
  layout.skip = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
  xat = list(seq(1,27,7),seq(1,27,7),seq(1,27,7),seq(1,27,7),seq(1,27,7),seq(1,27,7)),
  xaxis.lab = list(rank$Copeland[order(rank$Copeland)][seq(1,27,7)],rank$Copeland[order(rank$Copeland)][seq(1,27,7)],NULL, NULL,NULL,NULL) ,
  ylab.label = c("Score"),
  xlab.label = c("Error Severity"),
  xaxis.cex = 0.7,
  yaxis.cex = 0.7,
  ylab.cex = 1,
  xlab.cex = 1,
  xlab.to.xaxis.padding = 0,
  xlab.padding = 5,
  y.spacing = c(-0.5,-0.5,-0.5),
  x.relation = 'free',
  width = 4,
  height = 6.4,
  ylab.padding = 1,
  print.new.legend = TRUE,
  filename ="SC3_barplot_multiplot_titles",
  style ='Nature'
  )

create.multiplot(
  plot.objects = list(bp_pseudoV, bp_pearson, bp_sym_pseudoV, bp_sqrt, bp_aupr,bp_mcc),
  plot.layout = c(2,3),
  layout.skip = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
  xat = list(seq(1,27,7),seq(1,27,7),seq(1,27,7),seq(1,27,7),seq(1,27,7),seq(1,27,7)),
  xaxis.lab = list(rank$Copeland[order(rank$Copeland)][seq(1,27,7)],rank$Copeland[order(rank$Copeland)][seq(1,27,7)],NULL, NULL,NULL,NULL) ,
  ylab.label = c("Score"),
  xlab.label = c("Error Severity"),
  xaxis.cex = 0.6,
  yaxis.cex = 0.6,
  ylab.cex = 1,
  xlab.cex = 1,
  y.spacing = c(-1,-1,-1),
  x.relation = 'free',
  width = 4,
  height = 5.4,
  ylab.padding = 1,
# legend = list(right = list(fun=legend.metrics.grob,x=95,y=85)),
  print.new.legend = TRUE,
  filename ="SC3_barplot_multiplot_no_legend",
  style ='Nature'
  )

###Spearman_plot#######
plot.diff.AS <- function(diff.tot){
  diff.colours <- colour.gradient("chartreuse4", dim(diff.tot)[2])
  plot.labels <- c('Symmetric PseudoV', 'AUPR, Square-root, Symmetric PseudoV', 'PseudoV, Pearson, Symmetric PseudoV', 'AUPR, Square-root, Symmetric PseudoV, Pearson', 'AUPR', 'PseudoV, MCC, Pearson','Square-root', 'PseudoV CCM', 'PseudoV',  'Pearson', 'MCC', 'Original CCM' ,'Original')
  
#  plot.labels <- c('Original' , 'Original CCM' , 'PseudoV CCM', 'PseudoV', 'Square-root', 'Symmetric PseudoV', 'Pearson', 'AUPR', 'MCC', 'PseudoV, MCC, Pearson', 'PseudoV, Pearson, Symmetric PseudoV', 'AUPR, Square-root, Symmetric PseudoV', 'AUPR, Square-root, Symmetric PseudoV, Pearson')
#  diff.colours <- default.colours(number.of.colours=13, palette.type=c('seq'))
#  diff.colours <- c(diff.colours,rep('gainsboro',9))
#  diff.colours<-unlist(c(diff.colours,default.colours(number.of.colours=2,palette.type='pastel')))
  diff.tot <- diff.tot[,order(diff.tot["Copeland",])]
  # data for bar plot
  xdata <- colnames(diff.tot)
  ydata <-  diff.tot[1,]
  bp <- create.barplot(ydata ~ xdata, 
                 diff.tot,
          #      main = "Difference Between Metric Rankings and Desired Rankings",
           #      main.cex = 1.5,
		   		 xlab.label = c(),
                 xaxis.rot = 70,
                 xaxis.cex = 0.79,
                 yaxis.cex = 0.7,
                 xlab.cex = 04,
                 ylab.label = "1 -Spearman",
				 right.padding = 0,
				 left.padding = 0,
				 ylab.axis.padding = 0.3,
                 ylab.cex = 1,
				 width = 3.1,
				 height = 6,
                 col = diff.colours,
#				 xaxis.lab = rev(plot.labels),
                 sample.order = "increasing",
                 ylimits = c(0,1.1*max(ydata)),
                 filename = "Sc3_Spearman",
				 style = 'Nature'
  )
  
 # print(bp)
  return(bp)
}
main()
