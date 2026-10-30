# Generates all shared dummy datasets into data/. Run: Rscript make_dummy_data.R
set.seed(42); dir.create("data", showWarnings = FALSE)

# xy: scatter/bubble/histogram/density/Q-Q/ECDF/HR/Hubble
n <- 200; g <- sample(c("A","B","C"), n, TRUE)
xy <- data.frame(x = rnorm(n), group = g, size = runif(n, 1, 10))
xy$y <- 0.8*xy$x + rnorm(n, sd = 0.6) + (g == "B")
write.csv(xy, "data/xy.csv", row.names = FALSE)

# ts: time series
ts <- data.frame(date = seq(as.Date("2000-01-01"), by = "month", length.out = 300))
ts$value <- cumsum(rnorm(300, 0.05)) + 3*sin(2*pi*(1:300)/12)
ts$group <- rep(c("Site1","Site2","Site3"), each = 100)
write.csv(ts, "data/ts.csv", row.names = FALSE)

# grp: group/category values with replicates
grp <- data.frame(group = rep(c("Control","DrugA","DrugB","DrugC"), each = 30))
grp$value <- rnorm(120, mean = rep(c(5,6.5,4,8), each = 30), sd = 1.2)
grp$subgroup <- sample(c("Male","Female"), 120, TRUE)
write.csv(grp, "data/grp.csv", row.names = FALSE)

# de: differential expression
ng <- 5000
de <- data.frame(gene = paste0("GENE", 1:ng), baseMean = rexp(ng, 1/500)+1,
                 log2FC = rnorm(ng, 0, 1.2))
de$pvalue <- pmin(1, 2*pnorm(-abs(de$log2FC/0.5 + rnorm(ng))))
de$padj <- p.adjust(de$pvalue, "BH")
write.csv(de, "data/de.csv", row.names = FALSE)

# gwas
chr_len <- c(250,243,198,191,181,171,159,146,141,136,135,134,115,107,102,90,81,78,59,63,48,51)
gwas <- do.call(rbind, lapply(1:22, function(c) data.frame(
  chr = c, pos = sort(sample(1e6:(chr_len[c]*1e6), 400)), p = runif(400)^1.2)))
gwas$p[sample(nrow(gwas), 6)] <- 10^-runif(6, 8, 14)
gwas$snp <- paste0("rs", sample(1e6:9e6, nrow(gwas)))
write.csv(gwas, "data/gwas.csv", row.names = FALSE)

# mat: expression matrix 200 genes x 30 samples, 3 conditions
cond <- rep(c("Ctrl","Treat1","Treat2"), each = 10)
mat <- matrix(rnorm(200*30), 200, 30, dimnames = list(paste0("G",1:200), paste0("S",1:30)))
mat[1:40, cond=="Treat1"] <- mat[1:40, cond=="Treat1"] + 2
mat[41:80, cond=="Treat2"] <- mat[41:80, cond=="Treat2"] - 2
write.csv(mat, "data/mat.csv")
write.csv(data.frame(sample = colnames(mat), condition = cond), "data/mat_meta.csv", row.names = FALSE)

# surv
surv <- data.frame(id = 1:150, arm = rep(c("Placebo","Treatment"), each = 75))
surv$time <- rexp(150, rate = ifelse(surv$arm == "Placebo", 1/12, 1/20))
surv$event <- rbinom(150, 1, 0.75)
write.csv(surv, "data/surv.csv", row.names = FALSE)

# clf: classifier scores
clf <- data.frame(truth = rbinom(500, 1, 0.3))
clf$score <- plogis(rnorm(500, mean = ifelse(clf$truth == 1, 1.2, -0.8)))
clf$method_a <- rnorm(500, 100, 15); clf$method_b <- clf$method_a + rnorm(500, 1, 5)
write.csv(clf, "data/clf.csv", row.names = FALSE)

# meta
meta <- data.frame(study = paste("Study", LETTERS[1:12]), n = sample(50:800, 12))
meta$se <- 1/sqrt(meta$n)*2; meta$effect <- rnorm(12, 0.35, meta$se)
meta$lower <- meta$effect - 1.96*meta$se; meta$upper <- meta$effect + 1.96*meta$se
write.csv(meta, "data/meta.csv", row.names = FALSE)

# sets
sets <- data.frame(item = paste0("item", 1:300))
for (s in c("SetA","SetB","SetC","SetD")) sets[[s]] <- rbinom(300, 1, runif(1, .2, .5)) == 1
write.csv(sets, "data/sets.csv", row.names = FALSE)

# net: edge list
net <- data.frame(from = sample(paste0("N",1:40), 120, TRUE), to = sample(paste0("N",1:40), 120, TRUE),
                  weight = runif(120))
write.csv(net[net$from != net$to,], "data/net.csv", row.names = FALSE)

# flow: alluvial/chord
flow <- expand.grid(from = c("Stage I","Stage II","Stage III"), to = c("Responder","Stable","Progressed"))
flow$n <- sample(5:60, nrow(flow)); write.csv(flow, "data/flow.csv", row.names = FALSE)

# abund: microbiome counts (taxa x samples) + metadata
abund <- matrix(rnbinom(30*40, mu = 50, size = 0.6), 30, 40,
                dimnames = list(paste0("Taxon", 1:30), paste0("Smp", 1:40)))
write.csv(abund, "data/abund.csv")
write.csv(data.frame(sample = colnames(abund), group = rep(c("Healthy","Disease"), each = 20),
                     depth = colSums(abund)), "data/abund_meta.csv", row.names = FALSE)

# seq: aligned motif sequences
aa <- c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y")
seq <- replicate(100, paste(c(sample(aa,2,T), sample(c("K","R"),1,prob=c(.7,.3)), sample(aa,1),
                              sample(c("L","I"),1,prob=c(.8,.2)), sample(aa,3,T)), collapse = ""))
writeLines(seq, "data/seq.txt")

# tree (needs ape)
if (requireNamespace("ape", quietly = TRUE)) {
  tr <- ape::rcoal(40, tip.label = paste0("Isolate", 1:40)); ape::write.tree(tr, "data/tree.nwk")
  write.csv(data.frame(label = tr$tip.label, host = sample(c("Bat","Human","Pig"), 40, TRUE),
                       region = sample(c("West","East","North"), 40, TRUE)), "data/tree_meta.csv", row.names = FALSE)
}
message("Dummy data written to data/")
