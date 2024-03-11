plot_scatter_label <- function(data, location, img = NULL, use_color = NULL, fname = "scatter_label", save_dir = ".", reverse_x = FALSE, reverse_y = TRUE, n_cols = 1, point_size = 1, width = 7, height = 7){
    library(ggplot2)
    library(reshape2)
    library(ggnewscale)
    plot_df <- as.data.frame(cbind(location[rownames(data), c("x", "y")], data))
    plot_df <- melt(plot_df, id.vars = c("x", "y"), variable.name = "feature", value.name = "value")

    if (is.null(use_color)){
        if (length(unique(c(data))) <= 9){
            use_color <- RColorBrewer::brewer.pal(length(unique(c(data))), "Set1")
        } else{
            use_color <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(unique(c(data))))
        }
    }

    if (!is.null(img)){
        red <- img[,,1]
        green <- img[,,2]
        blue <- img[,,3]
        img_df <- data.frame(x = rep(1:ncol(img), each = nrow(img)), y = rep(1:nrow(img), ncol(img)), red = as.vector(red), green = as.vector(green), blue = as.vector(blue))
    }

    pdf(paste(save_dir, paste(fname, "pdf", sep = "."), sep = "/"), width = width, height = height)
    fig <- ggplot()
    if (!is.null(img)){
        fig <- fig +
                geom_raster(data = img_df, aes(x = x, y = y, fill = rgb(red, green, blue))) +
                scale_fill_identity() +
                new_scale_fill()
    }
    fig <- fig +
            geom_point(data = location, aes(x = x, y = y), fill = "grey80", size = point_size, colour = "black", shape = 21, stroke = 0.1) +
            geom_point(data = plot_df, aes(x = x, y = y, fill = as.factor(value)), size = point_size, colour = "black", shape = 21, stroke = 0.1) +
            coord_fixed() + 
            theme(panel.background = element_blank(),
                  panel.border = element_blank(),
                  plot.background  = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  strip.text = element_text(size = 15, face = "bold"),
                  strip.background = element_blank(),
                  legend.title = element_blank(),
                  legend.position = "right") + 
            scale_fill_manual(values = use_color) +
            facet_wrap(~feature, ncol = n_cols) + 
            labs(title = fname)
    if (reverse_y){
        fig <- fig + scale_y_continuous(trans = scales::reverse_trans())
    }
    if (reverse_x){
        fig <- fig + scale_x_continuous(trans = scales::reverse_trans())
    }
    print(fig)
    dev.off()
}



plot_scatterpie <- function(data, location, img = NULL, use_color = NULL, fname = "scatterpie", save_dir = ".", reverse_x = FALSE, reverse_y = TRUE, pie_r = 2, point_size = 2, width = 7, height = 7){
    library(ggplot2)
    library(scatterpie)
    library(RColorBrewer)
    library(ggpubr)
    library(ggnewscale)
    plot_df <- as.data.frame(apply(cbind(location[rownames(data),], data), 2, as.numeric))
    rownames(plot_df) <- rownames(data)
    unique_labels <- sort(colnames(data))

    if (is.null(use_color)){
        if (length(unique_labels) <= 9){
            use_color <- RColorBrewer::brewer.pal(length(unique_labels), "Set1")
        } else{
            use_color <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(unique_labels))
        }
    }

    if (!is.null(img)){
        red <- img[,,1]
        green <- img[,,2]
        blue <- img[,,3]
        img_df <- data.frame(x = rep(1:ncol(img), each = nrow(img)), y = rep(1:nrow(img), ncol(img)), red = as.vector(red), green = as.vector(green), blue = as.vector(blue))
    }

    pdf(paste(save_dir, paste(fname, "pdf", sep = "."), sep = "/"), width = width, height = height)
    fig <- ggplot()
    if (!is.null(img)){
        fig <- fig + 
                geom_raster(data = img_df, aes(x = x, y = y, fill = rgb(red, green, blue)), alpha = 0.5) +
                scale_fill_identity() +
                new_scale_fill()
    }
    fig <- fig +
            geom_point(data = location, aes(x = x, y = y), fill = "grey90", size = point_size, colour = "grey90", shape = 21, stroke = 0.01) +
            geom_scatterpie(data = plot_df, aes(x = x, y = y, r = pie_r), cols = unique_labels, color = NA) + 
            coord_fixed() +
            scale_fill_manual(values = use_color) +
            theme(panel.background = element_blank(),
                  panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
                  plot.background  = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  strip.text = element_text(size = 15, face = "bold"),
                  strip.background = element_blank(),
                  legend.title = element_blank(),
                  legend.position = "right") + 
            labs(title = fname)
    if (reverse_y){
        fig <- fig + scale_y_continuous(trans = scales::reverse_trans())
    }
    if (reverse_x){
        fig <- fig + scale_x_continuous(trans = scales::reverse_trans())
    }
    print(fig)
    dev.off()
}



plot_scatter_heatmap <- function(data, location, img = NULL, feature = NULL, scale = FALSE, fname = "scatter_heatmap", save_dir = ".", reverse_x = FALSE, reverse_y = TRUE, n_cols = 4, point_size = 1.5, width = 7, height = 7, legend_position = "right", panel_border = TRUE){
    library(ggplot2)
    library(reshape2)
    library(ggnewscale)
    if (is.null(feature)){
        feature <- colnames(data)
    }
    if (scale){
        data <- apply(data[, feature, drop = FALSE], 2, function(y){(y - min(y)) / (max(y) - min(y))})
    }
    plot_df <- as.data.frame(apply(cbind(location[rownames(data), c("x", "y")], data[, feature, drop = FALSE]), 2, as.numeric))
    plot_df <- melt(plot_df, id.vars = c("x", "y"), variable.name = "feature", value.name = "value")
    plot_df$feature <- factor(plot_df$feature, levels = feature)

    if (!is.null(img)){
        red <- img[,,1]
        green <- img[,,2]
        blue <- img[,,3]
        img_df <- data.frame(x = rep(1:ncol(img), each = nrow(img)), y = rep(1:nrow(img), ncol(img)), red = as.vector(red), green = as.vector(green), blue = as.vector(blue))
    }

    pdf(paste(save_dir, paste(fname, "pdf", sep = "."), sep = "/"), width = width, height = height)
    fig <- ggplot()
    if (!is.null(img)){
        fig <- fig +
                geom_raster(data = img_df, aes(x = x, y = y, fill = rgb(red, green, blue)), alpha = 1) +
                scale_fill_identity() +
                new_scale_fill()
    }
    fig <- fig +
            geom_point(data = location, aes(x = x, y = y), fill = "grey90", size = point_size, colour = "black", shape = 21, stroke = 0.05) +
            geom_point(data = plot_df, aes(x = x, y = y, fill = value), size = point_size, colour = "black", shape = 21, stroke = 0.05) +
            coord_fixed() + 
            scale_fill_gradientn(colors = c("#5E4FA2", "#3E96B6", "#99D5A4", "#FDFEBD", "#FDDB88", "#F67948", "#9E0142")) +
            facet_wrap(~feature, ncol = n_cols) + 
            labs(title = fname)
    if (panel_border){
        fig <- fig +
                theme(panel.background = element_blank(),
                    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
                    plot.background  = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    strip.text = element_text(size = 15, face = "bold"),
                    strip.background = element_blank(),
                    legend.title = element_blank(),
                    legend.position = legend_position)
    } else{
        fig <- fig +
                theme(panel.background = element_blank(),
                      panel.border = element_blank(),
                    plot.background  = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    strip.text = element_text(size = 15, face = "bold"),
                    strip.background = element_blank(),
                    legend.title = element_blank(),
                    legend.position = legend_position)
    }
    if (reverse_y){
        fig <- fig + scale_y_continuous(trans = scales::reverse_trans())
    }
    if (reverse_x){
        fig <- fig + scale_x_continuous(trans = scales::reverse_trans())
    }
    print(fig)
    dev.off()
}



plot_mu_reference <- function(mu_list, proportion, sc_counts, sc_labels, archetypes_list = NULL, min_prop = 0.2, f_name = "mu_reference", save_dir = ".", return_res = FALSE, n_cols = 4, width = 7, height = 7){

    library(ggplot2)
    library(ggrepel)

    if (!dir.exists(save_dir)){
        dir.create(save_dir, recursive = TRUE)
    }

    Z <- sc_counts / rowSums(sc_counts)

    gene_intersect <- intersect(colnames(Z), colnames(mu_list[[1]]))
    Z <- Z[, gene_intersect]
    mu_list <- lapply(mu_list, function(x){x[, gene_intersect]})

    unique_labels <- names(mu_list)
    pca_res_list <- list()
    umap_res_list <- list()
    plot_df <- Reduce(rbind, lapply(unique_labels, function(x){
        idx_sc <- which(sc_labels == x)
        idx_gene <- which(colSums(Z[idx_sc,]) != 0)
        pca_res <- prcomp(Z[idx_sc, idx_gene], center = TRUE, scale. = TRUE)
        pca_res_list[[x]] <- pca_res
        ndims <- min(30, length(idx_sc))
        Z_pca <- as.data.frame(pca_res$x[, 1:ndims])
        umap_settings <- umap::umap.defaults
        umap_settings$n_neighbors <- min(15, length(idx_sc))
        umap_res <- umap::umap(as.matrix(Z_pca), config = umap_settings)
        umap_res_list[[x]] <- umap_res
        Z_umap <- as.data.frame(umap_res$layout)
        idx_mu <- which(proportion[, x] > min_prop)
        if (length(idx_mu) > 0){
            mu <- mu_list[[x]][idx_mu, idx_gene, drop = FALSE]
            mu_pca <- as.data.frame(predict(pca_res, mu)[, 1:ndims, drop = FALSE])
            mu_umap <- as.data.frame(predict(umap_res, mu_pca))
            temp <- rbind(Z_umap, mu_umap)
            domain <- c(rep("sc", length(idx_sc)), rep("st", length(idx_mu)))
            prop <- c(rep(0, length(idx_sc)), proportion[idx_mu, x])
            name <- c(rownames(Z[idx_sc,]), rownames(mu))
            temp <- as.data.frame(cbind(temp, domain, prop, x, name))
            colnames(temp) <- c("dim1", "dim2", "domain", "prop", "label", "name")
        } else{
            temp <- Z_umap
            domain <- rep("sc", length(idx_sc))
            prop <- rep(0, length(idx_sc))
            name <- rownames(Z[idx_sc,])
            temp <- as.data.frame(cbind(temp, domain, prop, x, name))
            colnames(temp) <- c("dim1", "dim2", "domain", "prop", "label", "name")
        }
        if (!is.null(archetypes_list)){
            arche <- archetypes_list[[x]][, gene_intersect[idx_gene]]
            arche_pca <- as.data.frame(predict(pca_res, arche)[, 1:ndims])
            arche_umap <- as.data.frame(predict(umap_res, arche_pca))
            arche_temp <- as.data.frame(cbind(arche_umap, "archetype", 0, x, substring(rownames(arche), nchar(x) + 2)))
            colnames(arche_temp) <- c("dim1", "dim2", "domain", "prop", "label", "name")
            temp <- rbind(temp, arche_temp)
        }
        temp
    }))

    pdf(paste(save_dir, paste(f_name, "reference_only.pdf", sep = "_"), sep = "/"), width = width, height = height)
    fig <- ggplot() + 
            geom_point(aes(x = dim1, y = dim2, color = as.factor(domain), shape = as.factor(domain), size = as.factor(domain)), data = plot_df[(plot_df$domain == "sc") | (plot_df$domain == "archetype"),]) + 
            geom_text_repel(aes(x = dim1, y = dim2, label = name), data = plot_df[plot_df$domain == "archetype",], fontface = "bold") +
            labs(title = paste(f_name, "reference_only", sep = "_"), x = "UMAP_1", y = "UMAP_2") + 
            theme(panel.background = element_blank(),
                  panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
                  plot.background  = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  strip.text = element_text(size = 10, face = "bold"),
                  legend.position = "right") + 
            scale_color_manual(values = c(sc = "#989797", archetype = "#00A3D5")) +
            scale_shape_manual(values = c(sc = 16, archetype = 17)) + 
            scale_size_manual(values = c(sc = 2, archetype = 2.5)) +
            facet_wrap(~label, scales = "free", ncol = n_cols)
    print(fig)
    dev.off()

    pdf(paste(save_dir, paste(f_name, "pdf", sep = "."), sep = "/"), width = width, height = height)
    fig <- ggplot() + 
            geom_point(aes(x = dim1, y = dim2), color = "#989797", shape = 16, size = 2, data = plot_df[plot_df$domain == "sc",]) +
            geom_point(aes(x = dim1, y = dim2, color = prop), shape = 16, size = 2, data = plot_df[plot_df$domain == "st",]) + 
            labs(title = f_name, x = "UMAP_1", y = "UMAP_2") + 
            theme(panel.background = element_blank(),
                  panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
                  plot.background  = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  strip.text = element_text(size = 10, face = "bold"),
                  legend.position = "right") + 
            scale_color_gradient(low = "#fae1df", high =  "#f7061a") + 
            facet_wrap(~label, scales = "free", ncol = n_cols)
    print(fig)
    dev.off()

    if (return_res){
        return(list(plot_df = plot_df, pca_res_list = pca_res_list, umap_res_list = umap_res_list))
    }
}



plot_enrichGO <- function(genes, n_category = 15, object = "human", fname = "enrichment", save_dir = ".", width = 7, height = 7, return_results = FALSE){
    library(clusterProfiler)
    library(ggplot2)
    if (object == "mouse"){
        library(org.Mm.eg.db)
        OrgDb <- "org.Mm.eg.db"
    } else{
        library(org.Hs.eg.db)
        OrgDb <- "org.Hs.eg.db"
    }
    ids <- bitr(genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = OrgDb)
    ego <- enrichGO(ids$ENTREZID, OrgDb = OrgDb, keyType = "ENTREZID", ont = "BP", pAdjustMethod = "BH", readable = TRUE)
    if (dim(ego)[1] > 0){
        write.csv(ego, file = paste(save_dir, paste(fname, "csv", sep = "."), sep = "/"))
        pdf(paste(save_dir, paste(fname, "pdf", sep = "."), sep = "/"), width = width, height = height)
        fig <- barplot(ego, showCategory = n_category, drop = TRUE) + 
                labs(title = fname) +
                theme(axis.text.y = element_text(size = 10))
        print(fig)
        dev.off()
    } else{
        fig <- NULL
    }
    if (return_results){
        return(list(ego = ego, fig = fig))
    }
}
