library(BoutrosLab.plotting.general)
setwd("/u/asalcedo/SMC-HET-Paper1//Subchallenge1 plots")
da = read.csv("scoring1A_behavior.tsv",sep="\t",header=FALSE)
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

db = read.csv("scoring1B_behavior.tsv",sep="\t",header=FALSE)
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

d = read.csv("scoring1C_phi_sys_behavior.tsv",sep="\t",header=FALSE)
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


d = read.csv("scoring1C_phi_ZM_behavior.tsv",sep="\t",head=FALSE)
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

d = read.csv(file="scoring1C_nssm_behavior.tsv", sep="\t",header=FALSE)
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

d = read.csv(file="scoring1C_cases.tsv", sep="\t",header=FALSE)
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
#### SC3 Scoring barplots multiplot################################################################
setwd("/u/asalcedo/SMC-HET-Paper1/smc_het_eval");
bp_pseudoV<-plot.SC3.all(method="pseudoV",colour='turquoise4', title = "PseudoV")$bp
bp_pearson<-plot.SC3.all(method="pearson",colour= 'orangered3', title = "Pearson")$bp
bp_sym_pseudoV<-plot.SC3.all(method="sym_pseudoV",colour='firebrick4', title = "Symmertric PseudoV")$bp
#bp_orig<-plot.SC3.all(method="orig",colour='chartreuse4')$bp
bp_sqrt<-plot.SC3.all(method="sqrt", colour = 'royalblue4', title= "Square-root")$bp
bp_aupr<-plot.SC3.all(method="aupr",colour= 'darkmagenta', title ="AUPR")$bp
bp_mcc<-plot.SC3.all(method="mcc",colour='black', title = "MCC")$bp

legend.metrics = list (
  legend=list(
    colours = c('turquoise4','orangered3', 'firebrick4','royalblue4','darkmagenta','black'),
    labels = c("PseudoV","Peason","Symmertric PseudoV","Square-root","AUPR","MCC"),
    title = c("Metrics"),
    size=0.5
    )
  )
legend.metrics.grob <-legend.grob(
  legends= legend.metrics,
  title.cex=0.5,
  label.cex=0.5
  )
create.multiplot(
  plot.objects = list(bp_pseudoV, bp_pearson, bp_sym_pseudoV, bp_sqrt, bp_aupr,bp_mcc),
  plot.layout = c(2,3),
  layout.skip = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
  xat = list(seq(1,27,7),TRUE,TRUE,TRUE,TRUE,TRUE),
  xaxis.lab = list(rank$Copeland[order(rank$Copeland)],rank$Copeland[order(rank$Copeland)],NULL, NULL,NULL,NULL) ,
  ylab.label = c("Score"),
  xlab.label = c("Copeland Ranking"),
  xaxis.cex = 0.5,
  yaxis.cex = 0.5,
  ylab.cex = 0.75,
  xlab.cex = 0.75,
  y.spacing = c(-1,-1,-1),
  x.relation = 'free',
  width = 6.5,
  height = 7,
  ylab.padding = 1,
 legend = list(right = list(fun=legend.metrics.grob,x=95,y=85)),
  print.new.legend = TRUE,
  filename ="SC3_barplot_multiplot",
  style ='Nature'
  )
