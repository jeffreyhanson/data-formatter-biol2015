### File system interface functions
make_dir_FUN=function(dir_CHR)=function() {
	if (!file.exists(dir_CHR))
		dir.create(dir_chr,recursive=TRUE)
}

init_dir_FUN=function(dir_CHR) {
	make_dir_FUN(dir_CHR)
	sapply(apply(expand.grid(week_numbers_VCHR,project_names_VCHR,group_colors_VCHR,c("raw","clean","formatted","compiled")),1,paste,collapse="/"), make_dir)
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
	x=paste0('"',values,'"') 
	return(paste0(paste(x[seq_len(length(x)-1)], collapse=","), ", ", last_word, " ", x[length(x)]))
}
