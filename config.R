
is_linux <- (Sys.info()[['sysname']] == 'Linux')

CONFIG <- list(
	IS_LINUX = is_linux,
	TRAIN_DIR = ifelse(is_linux, '/home/rcarlomagno/train_dir', 'D:\\maestriadm\\dm economia finanzas\\bankchurn\\train_dir'),
	DATASET_FILE = ifelse(is_linux, "/home/rcarlomagno/data/dataset.sqlite", "D:\\maestriadm\\dm economia finanzas\\bankchurn\\dias\\dataset.sqlite"),
	DEFAULT_CUTOFF = 0.025
)