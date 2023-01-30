CMD=bash Make.sh

.PHONY:help release init tidy-branches patch-recipes build-recipes build-elpa_recipes build-eemacs_recipes make-infos clean sb-upsuggest init-elpa update-elpa clean-elpa

help:
	$(CMD)
all:
	$(CMD) all
	$(CMD) release
release:
	$(CMD) release
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
fetch-new:
	$(CMD) fetch-new
init-elpa:
	$(CMD) init-elpa
update-elpa:
	$(CMD) update-elpa
clean-elpa:
	$(CMD) clean-elpa
