
is_linux <- (Sys.info()[['sysname']] == 'Linux')

CONFIG <- list(
	IS_LINUX = is_linux,
	TRAIN_DIR = ifelse(is_linux, '/home/rcarlomagno/train_dir/', 'D:\\maestriadm\\dm economia finanzas\\bankchurn\\train_dir\\'),
	DATASETS_PATH = ifelse(is_linux, '/home/rcarlomagno/data/dias/', 'D:\\maestriadm\\dm economia finanzas\\bankchurn\\dias\\'),
	DATASET_FILE = ifelse(is_linux, "/home/rcarlomagno/data/dataset.sqlite", "D:\\maestriadm\\dm economia finanzas\\bankchurn\\dias\\dataset.sqlite"),
	RESULTS_FILE = ifelse(is_linux, "/home/rcarlomagno/data/results.db", "D:\\maestriadm\\dm economia finanzas\\bankchurn\\results.db"),
	RESULTS_FILE_WO_MBO = ifelse(is_linux, "/home/rcarlomagno/data/results_wo_mbo.db", "D:\\maestriadm\\dm economia finanzas\\bankchurn\\results_wo_mbo.db"),
	DEFAULT_CUTOFF = 0.025,
	WORK_PATH = ifelse(is_linux, "/home/rcarlomagno/cloud/cloud1/work/", "D:\\maestriadm\\dm economia finanzas\\bankchurn\\work\\")
)