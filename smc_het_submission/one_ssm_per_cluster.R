# The Package package is copyright (c) Year Ontario Institute for Cancer Research (OICR).
# This package and its accompanying libraries is free software; you can redistribute it and/or modify
# it under the terms of the GPL (either version 1, or at your option, any later version) or the
# Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental
# in nature and is provided WITHOUT WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE
# OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION OR WARRANTY THAT THE USE
# OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim,
# damage or liability, of whatsoever kind or nature, which may arise from your Institution's
# respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for
# Cancer Research be acknowledged and/or credit be given to OICR scientists, as scientifically appropriate.

#! /.mounts/labs/boutroslab/private/Software/NightlyBuild_current/Rscript
### one_ssm_per_cluster.R ##########################################################################
# Description

### HISTORY ########################################################################################
# Version       Date            Developer           Comments
# 0.01

### NOTES ##########################################################################################
# Analysis Plan

### PREAMBLE #######################################################################################
# Setting the Environment



library(VariantAnnotation);
commandArgs = TRUE;
args <- commandArgs(trailingOnly=TRUE);
vcf_file <-args[1]
vcf<-readVcf(vcf_file,"hg19");

num_ssm <- as.numeric(summary(vcf)[1])

#SC 2A

sc2A <- data.frame(ssms=seq_len(num_ssm));
sc2B <- matrix(ncol = num_ssm, nrow = num_ssm, rep(0,num_ssm^2));


#SC3

FA <- unlist(geno(vcf)$FA[(num_ssm+1):(2*num_ssm)]);
FA_ranks <- rank(FA, ties.method="random");
FA_ranks_rev <- 1230-FA_ranks
sc3A <- data.frame(ssms=seq_len(num_ssm), ancestor=(FA_ranks_rev))

assign_ancestry <- function(x){
	y <- rep(0, num_ssm)
	y[sc3A$ssm[sc3A$ancestor>x[2]]] <- 1
	return(y)
	}
	
sc3B <- t(apply(sc3A, 1, function(x) assign_ancestry(x)));



write.table(sc2A, "sc2A.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep='');
write.table(sc2B, "sc2B.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep='');
write.table(sc3A, "sc3A.txt", row.names=FALSE, col.names=FALSE, quote=FALSE);
write.table(sc3B, "sc3B.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep='');
















