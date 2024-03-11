find_markers <- function(counts, labels, n_markers = NULL){
    library(Seurat)
    library(dplyr)
    dat <- CreateSeuratObject(counts = counts, min.cells = 3, min.features = 0)
    dat <- NormalizeData(dat)
    dat <- FindVariableFeatures(dat, selection.method = "vst", nfeatures = 2000)
    all.genes <- rownames(dat)
    dat <- ScaleData(dat, features = all.genes)
    Idents(dat) <- labels
    markers <- FindAllMarkers(dat, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
    unique_labels <- unique(labels)
    markers_list <- lapply(unique_labels, function(x){
        temp <- markers[markers$cluster == x,]
        temp <- temp[order(temp$avg_log2FC, decreasing = TRUE),]
        temp})
    names(markers_list) <- unique_labels
    if (!is.null(n_markers)){
        marker_genes <- lapply(markers_list, function(x){
            temp <- x$gene
            n_markers <- min(n_markers, length(temp))
            temp[1:n_markers]})
    } else{
        marker_genes <- lapply(markers_list, function(x){x$gene})
    }
    return(marker_genes)
}



enrichment_celltype <- function(proportion, regions, n_shuffle = 10000){
    unique_regions <- sort(unique(regions))
    true_mean <- t(sapply(unique_regions, function(x){colMeans(proportion[regions == x,])}))
    null_mean <- sapply(seq(n_shuffle), function(i){
        permute_i <- sample(seq(length(regions)), length(regions))
        temp <- t(sapply(unique_regions, function(x){colMeans(proportion[permute_i[regions == x],])}))
        temp
    }, simplify = "array")
    diff_mean <- sapply(seq(n_shuffle), function(i){true_mean - null_mean[,,i]}, simplify = "array")
    std <- apply(diff_mean, 1:2, sd)
    diff_mean <- apply(diff_mean, 1:2, mean)
    diff_mean <- diff_mean / std
    return(list(diff_mean = diff_mean, true_mean = true_mean, null_mean = null_mean))
}

