### File system interface functions
make_dir_FUN=function(dir_CHR) {
	if (!file.exists(dir_CHR))
		dir.create(dir_CHR,recursive=TRUE)
}

init_dir_FUN=function() {
	# create raw, clean, formatted dirs
	sapply(c('raw', 'cleaned', 'formatted'), function(x) {	
		sapply(apply(expand.grid(x, project_names_VCHR, week_numbers_VCHR, group_colors_VCHR),1,paste,collapse='/'), make_dir_FUN)
	})
	
	# create compiled dirs
	sapply(apply(expand.grid('compiled', project_names_VCHR, week_numbers_VCHR),1,paste,collapse='/'), make_dir_FUN)
	
	# create master dirs
	sapply(apply(expand.grid('master', project_names_VCHR),1,paste,collapse='/'), make_dir_FUN)
	
}

### Miscellaneous functions
is.empty=function(x) {
	return(length(x)==0)
}

is.blank=function(x) {
	if (all(is.na(x)))
		return(TRUE)
	if (is.character(x)) {
		return(all(nchar(x)==0))
	} else {
		return(FALSE)
	}
}


convert2bool=function(x) {
	x=as.logical(x)
	x[which(is.na(x))]=FALSE
	return(as.numeric(x))
}

phrase_FUN=function(x, last_word="or") {
	return(paste0(paste(x[seq_len(length(x)-1)], collapse=", "), ", ", last_word, " ", x[length(x)]))
}

is.outlier=function(x) {
	return(2<abs((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)))
}

extractValue=function(inpDF, refCol, refValue, extractCol) {
	return(last(inpDF[[extractCol]][which(inpDF[[refCol]]==refValue)]))
}

