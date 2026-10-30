# Deep Learning Transformer Self-Attention & Saliency Map
# Tier 4 (Mind-Blowing) - Machine learning

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Multi-head self-attention token weights (Vaswani et al. transformer standard)
  # Clinical text tokens / biological sequence tokens
  tokens <- c("[CLS]", "Patient", "shows", "acute", "myocardial", "infarction", "with", "ST", "elevation", "[SEP]")
  n_tok <- length(tokens)
  
  # Simulate scaled dot-product attention weights Softmax(Q * K^T / sqrt(d_k))
  set.seed(42)
  attn_mat <- matrix(runif(n_tok * n_tok, 0.01, 0.08), n_tok, n_tok)
  
  # High cross-attention between "infarction", "ST", "elevation", and "acute"
  diag_idx <- cbind(1:n_tok, 1:n_tok)
  attn_mat[diag_idx] <- attn_mat[diag_idx] + 0.25 # Self-token attention
  
  attn_mat[6, 5] <- attn_mat[5, 6] <- 0.72 # myocardial <-> infarction
  attn_mat[6, 8] <- attn_mat[8, 6] <- 0.65 # infarction <-> ST
  attn_mat[8, 9] <- attn_mat[9, 8] <- 0.85 # ST <-> elevation
  attn_mat[1, 6] <- 0.60 # [CLS] pooled token focusing on infarction
  
  # Row-normalize to simulate Softmax probability distribution (sum = 1.0 per row)
  attn_norm <- sweep(attn_mat, 1, rowSums(attn_mat), "/")
  
  df_attn <- expand.grid(Query = tokens, Key = tokens)
  df_attn$AttentionWeight <- as.vector(t(attn_norm))
  df_attn$Query <- factor(df_attn$Query, levels = rev(tokens))
  df_attn$Key <- factor(df_attn$Key, levels = tokens)
  
  p <- ggplot(df_attn, aes(x = Key, y = Query, fill = AttentionWeight)) +
    geom_tile(color = "#FFFFFF", linewidth = 0.45) +
    geom_text(aes(label = sprintf("%.2f", AttentionWeight)), 
              size = 2.7, fontface = "bold",
              color = ifelse(df_attn$AttentionWeight > 0.35, "#FFFFFF", "#1E293B")) +
    scale_fill_gradientn(
      colors = c("#EFF6FF", "#93C5FD", "#3B82F6", "#1D4ED8", "#1E3A8A"),
      name = "Softmax Attention Weight (0 to 1):"
    ) +
    labs(
      title = "Transformer Multi-Head Self-Attention Weight Saliency Matrix",
      subtitle = "Scaled dot-product attention scores Softmax(Q·K^T / √d_k) tracking token-to-token contextual focus",
      x = "Key Input Token (Receptive Context)",
      y = "Query Input Token (Attending Sequence)",
      caption = "Source: Simulated clinical BERT / BioGPT self-attention layer matrix (Vaswani standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = rel(0.9)),
        axis.text.y = element_text(face = "bold", size = rel(0.9)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.key.width = unit(1.6, "cm"),
        legend.key.height = unit(0.35, "cm")
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "Transformer Multi-Head Self-Attention Saliency Matrix",
    how_to_read = "A 2D attention heat matrix where each cell denotes the mathematical Softmax weight allocated by a Query token (y-axis) onto a Key token (x-axis). High-intensity blue tiles identify the contextual dependencies that the neural network leverages to predict biological functions or diagnostic labels.",
    when_to_use = "The indispensable explainability figure for transformer architectures in medicine, genomics, and NLP (AlphaFold, ESM-2, ClinicalBERT) to confirm that the model attends to valid clinical biomarkers rather than spurious confounders.",
    pitfalls = "Attention weights do not always correlate perfectly with gradient-based feature attribution; compare attention maps against integrated gradients or Grad-CAM saliency for rigorous verification.",
    journal_archetype = "Nature Machine Intelligence, Science, and Cell deep learning interpretability papers.",
    data_recipe = "Requires extracted transformer multi-head attention weight tensors across input tokens."
  )
}
