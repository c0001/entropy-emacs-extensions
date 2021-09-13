CMD=bash Make.sh
help:
	$(CMD)
all:
	$(CMD) all
init:
	$(CMD) init
tidy-branches:
	$(CMD) tidy-branches
toggle-branches:
	$(CMD) toggle-branches
patch-recipes:
	$(CMD) patch-recipes
build-recipes:
	$(CMD) build-recipes
build-elpa_recipes:
	$(CMD) build-elpa_recipes
build-eemacs_recipes:
	$(CMD) build-eemacs_recipes
make-infos:
	$(CMD) make-infos
clean:
	$(CMD) clean

# maintainability part
sb-upsuggest:
	$(CMD) sb-upsuggest
init-elpa:
	$(CMD) init-elpa
update-elpa:
	$(CMD) update-elpa
clean-elpa:
	$(CMD) clean-elpa
