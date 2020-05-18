filterpca = function(x,npc=NULL,pcs=NULL,scale.=F,
	method=c("k","t"),resulttype=c("p","d","b"),lambda=NULL) {
	pca <- prcomp(x,scale.=scale.)
	nc <- ncol(pca$x)
	if (!is.null(npc)) {
		npc <- as.integer(npc[1])
		if (is.na(npc)) npc <- nc
		if (npc>nc) npc <- nc
		if (npc<1) npc <- max(1L,nc+npc)
	}
	if (!is.null(pcs)) {
		pcs <- as.integer(pcs)
		pcs <- pcs[!is.na(pcs)]
		if (length(pcs[pcs>0])) {
			pcs <- intersect(1:nc,pcs[pcs>0])
		} else if (length(pcs[pcs<0])) {
			pcs <- setdiff(1:nc,-pcs[pcs<0])			
		} else pcs <- 1:nc
		if (length(pcs)==0) pcs <- 1
	} else {
		pcs <- 1:nc
		if (!is.null(npc)) {
			pcs <- pcs[1:npc]
		}
	}
	rotation <- as.matrix(pca$rotation[,pcs])
	scores <- as.matrix(pca$x[,pcs])
	if (grepl("t",method[1],T)) {
		if (!is.numeric(lambda)) lambda <- mean(abs(scores))*0.25
		scores[abs(scores)<lambda] <- 0
	}
	recon <- scores %*% t(rotation)
	if (!(scale.==FALSE)) {
		recon <- sweep(recon,2,pca$scale,`*`)
	}
	recon <- sweep(recon,2,pca$center,"+")
	if (grepl("p",resulttype[1],T)) return(recon)
	mdist <- sqrt(rowSums(as.matrix(pca$x[,pcs]^2)))
	resid1 <- sqrt(rowSums((recon - x)^2))
	dist1 <- data.frame(mahaldist=mdist,residuals=resid1)
	rownames(dist1) <- rownames(x)
	if (grepl("d",resulttype[1],T)) return(dist1)
	list(projection=recon,distance=dist1)
}
