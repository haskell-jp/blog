.PHONY: build clean deploy release site watch
all: site

#############
# Variables #
#############

# Path to the directory that `stack` uses to install binaries locally.
STACK_LOCAL_INSTALL_PATH = $(shell stack path --local-install-root)

# Path to the `site` binary.
SITE_PROG_PATH = $(STACK_LOCAL_INSTALL_PATH)/site

# The current commit's git hash.
GIT_HASH = $(shell git rev-parse --short HEAD)

################################
## Targets for specific files ##
################################

# target for the `site` binary.  This binary is used to actually create the
# html files.
$(SITE_PROG_PATH): src/site.hs
	@echo "Building..."
	@stack build
	@echo "Built."

#####################
## General targets ##
#####################

# Build the `site` binary.  The `site` binary is used to build the actual .html
# files for the site.
build: $(SITE_PROG_PATH)

# Clean all generated files.
clean:
	@echo "Cleaning..."
	-@stack exec -- site clean 2>/dev/null || true
	@rm -rf .hakyll-cache/ generated-site/
	@stack clean
	@echo "Clean."

# Deploy the site.
# Commit the generated-site directory to the gh-pages git branch.
# The way this is done is pretty hacky, but it works.
deploy: site
	# Make sure this temporary working directory is empty.
	# (TODO: Really we should be using a directory with a random filename,
	# generated with something like mktemp.)
	rm -rf /tmp/haskell-jp-blog-deploy/
	mkdir /tmp/haskell-jp-blog-deploy/
	# Copy the generated site to the temp directory.
	cp -r generated-site /tmp/haskell-jp-blog-deploy/
	# Checkout the gh-pages branch.
	git checkout gh-pages
	# Remove the pages for the current site.
	git rm -r -f --ignore-unmatch *
	git status
	# Copy all of the generated site's files to the current directory.
	cp -r /tmp/haskell-jp-blog-deploy/generated-site/* ./
	# Add everything back.  (A lot of files probably won't change, so, for
	# instance, they won't show up on 'git status' even though we just did 'git
	# rm -rf *'.  A 'git rm -rf FILE' followed by 'git add FILE' is a noop if
	# the file hasn't changed.)
	git add -A .
	git status
	# Do the commit and push.
	git commit -m "Release $(GIT_HASH) on `date`."
	git push origin gh-pages
	# Go back to master.
	git checkout master
	rm -rf /tmp/haskell-jp-blog-deploy

# Alias for deploy.
release: deploy

# Generate the .html files for our blog.
site: $(SITE_PROG_PATH) 
	@# We don't actually need to use rebuild here, we could just use build.
	@# If this blog becomes really big and produces tons of pages, then switching
	@# to 'build' here (and adding an additional site-rebuild target) would be a
	@# good idea.
	stack exec -- site rebuild

test:
	stack test

# Run a test webserver on http://0.0.0.0:8000 serving up the content of our
# blog.  If the content changes, it is automatically rebuilt.
watch: $(SITE_PROG_PATH)
	stack exec -- site watch --host 0.0.0.0
