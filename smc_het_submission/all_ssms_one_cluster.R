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

#SC 1B
sc1B <- 1;

#SC 1C
sc1C <-num_ssm;

#SC 2A

sc2A <- data.frame(ssms=rep(1,num_ssm));
sc2B <- matrix(ncol = num_ssm, nrow = num_ssm, rep(1,num_ssm^2));

#SC 3A

sc3A <- 10;
sc3B <-  matrix(ncol = num_ssm, nrow = num_ssm, rep(0,num_ssm^2));

write.table(sc1B, "sc1B.txt", row.names=FALSE, col.names=FALSE, quote=FALSE);
write.table(sc1C, "sc1C.txt", row.names=FALSE, col.names=FALSE, quote=FALSE);
write.table(sc2A, "sc2A.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep='');
write.table(sc2B, "sc2B.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep='');
write.table(sc3A, "sc3A.txt", row.names=FALSE, col.names=FALSE, quote=FALSE);
write.table(sc3B, "sc3B.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep='');
















