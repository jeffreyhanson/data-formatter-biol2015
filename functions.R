### File system interface functions
make_dir_FUN=function(dir_CHR) {
	if (!file.exists(dir_CHR))
		dir.create(dir_CHR,recursive=TRUE)
}

init_dir_FUN=function() {
	sapply(apply(expand.grid(week_numbers_VCHR,project_names_VCHR,group_colors_VCHR,c("raw","clean","formatted","compiled")),1,paste,collapse="/"), make_dir_FUN)
}

### Miscellaneous functions
is.empty=function(x) {
	return(length(x)==0)
}

convert2bool=function(x) {
	x=as.logical(x)
	x[which(is.na(x))]=FALSE
	return(as.numeric(x))
}

phrase_FUN=function(x, last_word="or") {
	x=paste0('"',x,'"')
	return(paste0(paste(x[seq_len(length(x)-1)], collapse=", "), ", ", last_word, " ", x[length(x)]))
}

is.outlier=function(x) {
	return(2<abs((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)))
}
