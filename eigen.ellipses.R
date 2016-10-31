eigenEllipses <- function(model.type, sigma.name, mu.name){
  # Two setups: 
  # eigen.ellipses("multi","Sigma","mu")
  # eigen.ellipses("hier","Sigma_pft","mu_pft")
  
  library(mvtraits)
  library(RColorBrewer)
  library(grid)
  library(gridExtra)
  ##################################
  # Hardcoding for setting different x and y limits depending on trait 
  lims <- list()
  lims[["LL"]] <- c(0.0, 4.5)
  lims[["LMA"]] <- c(-5.2, 1.0)
  lims[["Nmass"]] <- c(1.75, 3.75)
  lims[["Pmass"]] <- c(-1.0, 1.7)
  lims[["Rdmass"]] <- c(-6.0, -2.5)

  ellipse.scale = 1.96 # 95% 
  ##################################
  
  all_summary <- readRDS("processed_output/summary.rds")
  pft.biome <- all_summary[model_type == model.type][,c("PFT","Biome"),with=F] %>%
    distinct(.) 
  pfts <- pft.biome$PFT
  # For testing
  # pfts <- pfts[1:3]
  
  trait.pairs <- combn(traits_nolog, 2)
  p <- list()
  
  # Setting up plot colors themes for Biomes 
  biome.color <- data.table(Biome = unique(all_summary[,Biome]),
                            color = c(brewer.pal(length(unique(pft.biome$Biome))-1,"Set1"),"black"))
  pft.biome.color <-  right_join(pft.biome,biome.color, by="Biome")

  BiomeColorsPFT <- pft.biome.color$color
  names(BiomeColorsPFT) <- pft.biome.color$PFT

  BiomeColors <- biome.color$color
  names(BiomeColors) <- biome.color$Biome 
  
  for(i in 1:dim(trait.pairs)[2]){
    
    ctrs <- as.data.frame(matrix(NA,length(pfts),2))
    colnames(ctrs) <- c("x","y")
    rownames(ctrs) <- pfts
    ctrs$shape <- rep(16,length(pfts)) # shape 16 is a point 
    
    ells.list <- list()
    eigs.list <- list()
    
    for(j in 1:length(pfts)){
      dt <- all_summary[model_type == model.type][PFT == pfts[j]]
      # Calculate the center
      if(pfts[j]=="global" & model.type=="hier"){#gross exception
        ctrs[j,1:2] <- dt[var_type == "mu_global"][trait == trait.pairs[1,i]|trait == trait.pairs[2,i]][,Mean]
      }else{
        ctrs[j,1:2] <- dt[var_type == mu.name][trait == trait.pairs[1,i]|trait == trait.pairs[2,i]][,Mean]
      }
      # Calculate the covariance matrix
      if(pfts[j]=="global" & model.type=="hier"){#gross exception
        cov_matrix <- dt[var_type == "Sigma_global"][PFT == pfts[j]] %>% 
          tab2mat %>% .[trait.pairs[,i], trait.pairs[,i]]
      }else{
        cov_matrix <- dt[var_type == sigma.name][PFT == pfts[j]] %>% 
          tab2mat %>% .[trait.pairs[,i], trait.pairs[,i]]
        }
      # Calculate Eigenvectors and Eigenvalues
      eig <- eigen(cov_matrix)
      evals  <- eig$values
      evecs  <- eig$vectors
      # "Note that when plotting confidence ellipses for data, the ellipse-axes 
      #  are usually scaled to have length = square-root of the corresp eigenvalues"
      evecs.scaled <- evecs %*% diag(sqrt(evals))
      evacs.use <- evecs.scaled
      eig.points <- as.data.frame(cbind(rbind(ctrs[j,1] + evecs.scaled[1, ], 
                                                ctrs[j,1] - evecs.scaled[1, ]),
                                          rbind(ctrs[j,2] + evecs.scaled[2, ],
                                                ctrs[j,2] - evecs.scaled[2, ])))
      colnames(eig.points) <- c("x1","x2","y1","y2")
      eigs.list[[pfts[j]]] <- eig.points
      
      if(!all(eig.points < 100)){
        ctrs$shape[j] <- 8 # Change point to *
      }
      # Calculate normal ellipse centered at the means, rotated 
      alpha <- atan(evecs[,1][2] / evecs[,1][1])
      theta <- seq(0, 2 * pi, length=(50))
      ell.normal <- ellipse.scale * cbind(sqrt(evals[1])*cos(theta), sqrt(evals[2])*sin(theta)) # normal ellipse scaled to 1.96
      ell.rotate <- evecs %*% t(ell.normal) # rotated ellipse
      # Either use the ellipse that is calculated by hand
      ell <- as.data.table(t(ell.rotate+unlist(ctrs[j,1:2]))) %>% 
        bind_cols(.,as.data.frame(rep(pfts[j],50))) %>%
        setNames(.,c("x","y","pft"))
      # OR the built in function (I did both to make sure I understood)
      ell.alt <- as.data.frame(ellipse(
        center = c(ctrs[j,1],ctrs[j,2]), 
        shape = cov_matrix, draw = F, radius = ellipse.scale))%>% 
        bind_cols(.,as.data.frame(rep(pfts[j],dim(.)[1]))) %>%
        setNames(.,c("x","y","pft"))
      ells.list[[pfts[j]]] <- ell
    } # End loop over pfts
    
    # Create ggplot elements
    add_centers <- lapply(setdiff(pfts,"global"), function(pft){
      geom_point(data = ctrs[pft,1:2], aes_q(x=as.name("x"),y=as.name("y"), colour=pft), 
                 shape=ctrs[pft,3], size=1)
    } )
    add_ellipses <- lapply(setdiff(pfts,"global"), function(pft){
      geom_polygon(data = ells.list[[pft]], aes_q(x=as.name("x"),y=as.name("y"), colour = pft), fill = NA)
    })
    add_global_ellipse <- geom_polygon(data = ells.list[["global"]],
                                       aes(x=x,y=y),
                                       fill = "black",
                                       alpha = 0.2)
    add_global_eig <- list(
      geom_line(data = eigs.list[["global"]],
                aes(x = x1, y = y1, colour = "global"), 
                size = 0.5, 
                alpha = 0.6),
      geom_point(data = ctrs["global", 1:2],
                 aes(x = x, y = y,colour ="global"), 
                 shape = ctrs["global", 3],
                 size = 2)
    )
    add_eig1 <- lapply(setdiff(pfts,"global"), function(pft){
      geom_line(data = eigs.list[[pft]],
                aes_q(x=as.name("x1"),y=as.name("y1"), colour = pft), size=.5, alpha=.6)
    })
    add_eig2 <- lapply(setdiff(pfts,"global"), function(pft){
      geom_line(data = eigs.list[[pft]],
                aes_q(x=as.name("x2"),y=as.name("y2"), colour = pft), size=.5)
    })
    add_xlim <- xlim(lims[[trait.pairs[1,i]]][1],lims[[trait.pairs[1,i]]][2])
    add_ylim <- ylim(lims[[trait.pairs[2,i]]][1],lims[[trait.pairs[2,i]]][2])
    
    p[[trait.pairs[1,i]]][[trait.pairs[2,i]]] <- 
      ggplot() + 
      add_global_ellipse +
      # add_ellipses +
      add_centers + 
      add_eig1 + 
      # add_eig2 +
      add_global_eig +
      scale_colour_manual(name = pfts, values = BiomeColorsPFT) + 
      add_xlim + add_ylim +
      global_theme + theme(legend.position="none")
    
  } # End loop over trait pairs

  return(p)
}

