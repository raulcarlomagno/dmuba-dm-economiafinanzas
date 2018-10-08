calculate_profit <- function(cutoff, probabilities, true_classes){
	sum((probabilities > cutoff) * ifelse(true_classes == 1, 11000, -300))
}