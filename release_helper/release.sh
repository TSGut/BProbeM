# TODO: change version in "project.json"

# create a git tag $tag before doing this
tag="v0.1.0"

git archive --format tar.gz ${tag} --output /tmp/BProbe-${tag}.tar.gz --prefix=BProbe/

# TODO:
# remove "Install" folder
# create "Documentation" folder (store wiki-converted html files in it)
