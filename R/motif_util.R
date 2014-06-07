
#' Gets all genes from UCSC.dm3.ensGene genome
#'
#' Returns all genes as a GeneList
#'
#' @param
#' @keywords genes
#' @export
getDmelGenes <- function(){
	library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
	txdb <- TxDb.Dmelanogaster.UCSC.dm3.ensGene
	gn <- sort(genes(txdb))
	return(gn)
}

#' Gets flanking regions of given genes
#'
#' Returns the upstream sequences given a list of genes
#'
#' @param genes A GRanges object of genes of interest (e.g. as produced by getDmelGenes)
#' @param len	Length of upstream sequence to retrieve
#' @return A DNAStringSet contining upstream sequences
#' @keywords genes
#' @export
getUpstreamSeq <- function(genes, len){
	ups <- flank(genes, width=len)
	library(BSgenome.Dmelanogaster.UCSC.dm3)
	genome <- BSgenome.Dmelanogaster.UCSC.dm3
	return( getSeq(genome, ups))
}

#' Clusters genes by expression using k-means
#'
#' Returns a list of data.frames split by cluster
#'
#' @param df	A data.frame containing DE values, one gene per line (gene as rowname)
#' @param clusters	Number of clusters to generate
#' @keywords genes, clustering
#' @return A list of data.frames
#' @export
clusterDEList <- function(df, clusters){
	# Perform clustering
	k <- kmeans(df, clusters)

	# df$gene <- rownames(df)
	# Transfer cluster number to DF of dgro logFCs
	df$cluster <- as.factor(k$cluster)
	df.split <- split(df, df$cluster)
	return(df.split)
}

# pullAndWrite <- function(upstream.seq, df.split, len){
# 	my.seq <- lapply(df.split, function(dg) {
# 		return( upstream.seq[names(upstream.seq) %in% rownames(dg)])
# 	})

# 	# Should be true (one sequence pulled per gene)
# 	stopifnot(identical(sapply(df.split, nrow), sapply(my.seq, length)))

# 	# Write to fastas using appropriate filenames
# 	for( i in seq(1,length(df.split))){
# 		myname <- paste("dgro_len_", len, "_cluster_", i, ".fasta", sep="")
# 		writeXStringSet(my.seq[[i]], myname)
# 	}

# }

# ###############
# # Sample run

# my.file <- "edger_130822.de"
# # Number of clusters to use
# clusters <- 8

# de <- loadDEData(my.file)
# de.split <- clusterDEList(de, clusters)
# genes <- getGenes()

# for(i in seq(50, 1000, 50)){
# 	print(i)
# 	pullAndWrite(getUpstreamSeq(genes, i), de.split, i)
# }


# ########
# ########
# # Get flanking sequences of all genes
# # Stored in file ups.df
# library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
# txdb <- TxDb.Dmelanogaster.UCSC.dm3.ensGene
# gn <- sort(genes(txdb))
# ups <- flank(gn, width=len)
# library(BSgenome.Dmelanogaster.UCSC.dm3)
# genome <- BSgenome.Dmelanogaster.UCSC.dm3
# ups.seq <- getSeq(genome, ups)

# #### Write out to file if desired
# # write.table(as.data.frame(ups.seq), file="output.txt", col.names=F, quote=F)
# ####

# #######
# #######
# # Pull edger data into file, cluster using kmeans
# edger <- read.table(my.file)
# dgro <- edger[,grepl("logFC.dgro*", colnames(edger))]


# # Perform clustering; 10 clusters
# k <- kmeans(dgro, clusters)

# dgro$gene <- rownames(dgro)
# # Transfer cluster number to DF of dgro logFCs
# dgro$cluster <- as.factor(k$cluster)

# # Inspect clusters if desired using line plot
# # library(reshape2)
# # library(ggplot2)
# # dgromelt <- melt(dgro, id.vars=c("gene", "cluster"))
# # ggplot(data=dgromelt) + geom_line(aes(x=variable, y=value, group=gene), alpha=1/20) + facet_wrap( ~ cluster, nrow=1)

# # Pull interesting clusters
# dgro.split <- split(dgro, dgro$cluster)


# pullAndWrite <- function(upstream, genome, genes, len){


# 	my.seq <- lapply(genes, function(dg) {
# 		return( dna[names(dna) %in% rownames(dg)])
# 	})

# 	# Should be true (one sequence pulled per gene)
# 	identical(sapply(genes, nrow), sapply(my.seq, length))

# 	# Write to fastas using appropriate filenames

# 	for( i in seq(1,clusters)){
# 		myname <- paste("dgro_len_", len, "_cluster_", i, ".fasta", sep="")
# 		writeXStringSet(my.seq[[i]], myname)
# 	}

# }




# Queue jobs on hoffman
# qsub -l h_data=2G,time=8:00:00 -N all100_01   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_01_motifs_sample   -mod anr -revcomp -nmotifs 01   -maxsize 10000000 > all100_01_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_05   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_05_motifs_sample   -mod anr -revcomp -nmotifs 05   -maxsize 10000000 > all100_05_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_10   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_10_motifs_sample   -mod anr -revcomp -nmotifs 10   -maxsize 10000000 > all100_10_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_20   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_20_motifs_sample   -mod anr -revcomp -nmotifs 20   -maxsize 10000000 > all100_20_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_30   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_30_motifs_sample   -mod anr -revcomp -nmotifs 30   -maxsize 10000000 > all100_30_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_40   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_40_motifs_sample   -mod anr -revcomp -nmotifs 40   -maxsize 10000000 > all100_40_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_50   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_50_motifs_sample   -mod anr -revcomp -nmotifs 50   -maxsize 10000000 > all100_50_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_60   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_60_motifs_sample   -mod anr -revcomp -nmotifs 60   -maxsize 10000000 > all100_60_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_70   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_70_motifs_sample   -mod anr -revcomp -nmotifs 70   -maxsize 10000000 > all100_70_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_80   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_80_motifs_sample   -mod anr -revcomp -nmotifs 80   -maxsize 10000000 > all100_80_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_90   -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_90_motifs_sample   -mod anr -revcomp -nmotifs 90   -maxsize 10000000 > all100_90_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_100  -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_100_motifs_sample  -mod anr -revcomp -nmotifs 100  -maxsize 10000000 > all100_100_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all100_1000 -cwd -b y  "~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_1000_motifs_sample -mod anr -revcomp -nmotifs 1000 -maxsize 10000000 > all100_1000_sample.motifs"

# qsub -l h_data=2G,time=8:00:00 -N all1000_01   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_01_motifs_sample   -mod anr -revcomp -nmotifs 01   -maxsize 10000000 > all1000_01_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_05   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_05_motifs_sample   -mod anr -revcomp -nmotifs 05   -maxsize 10000000 > all1000_05_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_10   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_10_motifs_sample   -mod anr -revcomp -nmotifs 10   -maxsize 10000000 > all1000_10_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_20   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_20_motifs_sample   -mod anr -revcomp -nmotifs 20   -maxsize 10000000 > all1000_20_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_30   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_30_motifs_sample   -mod anr -revcomp -nmotifs 30   -maxsize 10000000 > all1000_30_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_40   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_40_motifs_sample   -mod anr -revcomp -nmotifs 40   -maxsize 10000000 > all1000_40_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_50   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_50_motifs_sample   -mod anr -revcomp -nmotifs 50   -maxsize 10000000 > all1000_50_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_60   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_60_motifs_sample   -mod anr -revcomp -nmotifs 60   -maxsize 10000000 > all1000_60_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_70   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_70_motifs_sample   -mod anr -revcomp -nmotifs 70   -maxsize 10000000 > all1000_70_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_80   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_80_motifs_sample   -mod anr -revcomp -nmotifs 80   -maxsize 10000000 > all1000_80_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_90   -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_90_motifs_sample   -mod anr -revcomp -nmotifs 90   -maxsize 10000000 > all1000_90_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_100  -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_100_motifs_sample  -mod anr -revcomp -nmotifs 100  -maxsize 10000000 > all1000_100_sample.motifs"
# qsub -l h_data=2G,time=8:00:00 -N all1000_1000 -cwd -b y  "~/bin/meme/bin/meme all1000_sample.fasta -dna -o all1000_1000_motifs_sample -mod anr -revcomp -nmotifs 1000 -maxsize 10000000 > all1000_1000_sample.motifs"

# qsub -l h_data=2G,time=8:00:00 -N all100_05_n   -cwd -b y  "time ~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_05_motifs_sample_n   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=2G,time=8:00:00 -N all100_05_n_nor   -cwd -b y  "time ~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_05_motifs_sample_n_rev   -mod anr -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=1G,h_cpu=8,time=8:00:00 -N all100_05_p   -cwd -b y  "time ~/bin/meme/bin/meme all100_sample.fasta -dna -o all100_05_motifs_sample_p   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=1G,h_cpu=8,time=8:00:00 -N all100_05_p2   -cwd -b y  "time ~/bin/meme/bin/meme all100_sample.fasta -p 8 -dna -o all100_05_motifs_sample_p2   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=2G,h_cpu=8,time=8:00:00 -N all100_05_p3   -cwd -b y  "time ~/bin/meme/bin/meme all100_sample.fasta -p 8 -dna -o all100_05_motifs_sample_p3   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=8G,h_cpu=8,time=8:00:00 -N all100_05_p4   -cwd -b y  "time ~/bin/meme/bin/meme_p all100_sample.fasta -p 8 -dna -oc all100_05_motifs_sample_p4   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=8G,h_rt=8:00:00 -pe shared 8 -N all100_05_p5   -cwd -b y  "time ~/bin/meme/bin/meme_p all100_sample.fasta -p 8 -dna -oc all100_05_motifs_sample_p5   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=1G,h_rt=8:00:00 -pe shared 8 -N all100_05_p6   -cwd -b y  "time ~/bin/meme/bin/meme_p all100_sample.fasta -p 8 -dna -oc all100_05_motifs_sample_p6   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=2G,h_rt=8:00:00 -pe shared 8 -N all100_05_p7   -cwd -b y  "time ~/bin/meme/bin/meme_p all100_sample.fasta -p 8 -dna -oc all100_05_motifs_sample_p7   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=1G,h_rt=8:00:00 -pe shared 4 -N all100_05_p8   -cwd -b y  "time ~/bin/meme/bin/meme_p all100_sample.fasta -p 4 -dna -oc all100_05_motifs_sample_p8   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"
# qsub -l h_data=2G,h_rt=8:00:00 -pe shared 4 -N all100_05_p9   -cwd -b y  "time ~/bin/meme/bin/meme_p all100_sample.fasta -p 4 -dna -oc all100_05_motifs_sample_p9   -mod anr -revcomp -nmotifs 05   -maxsize 10000000"

