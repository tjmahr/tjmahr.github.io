.PHONY: targets clean

targets:
	Rscript --vanilla -e "targets::tar_make(); cli::cat_rule('Makefile finished', col = 'yellow')"

clean:
	Rscript --vanilla -e "targets::tar_prune(); targets::tar_deduplicate(); cli::cat_rule('Makefile finished', col = 'yellow')"
