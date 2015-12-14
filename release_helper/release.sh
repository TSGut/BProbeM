#!/bin/sh


# TODO: pull latest wiki docs

tag=v${1}	# versiont ag

# update version in project.json file
./update_version.rb ${1}

read -p "Warning: git commit and 'tag v${1}' are to be created. Are you sure? (You should pull latest wiki docs before doing this)"

cd ../

git commit ./project.json -m "update version to ${tag}"
git tag $tag
git archive --format tar ${tag} --output ../BProbe-${tag}.tar --prefix=BProbe/

cd ../

# remove unnessary directories
tar --delete BProbe/Install/ -f ./BProbe-${tag}.tar
tar --delete BProbe/release_helper/ -f ./BProbe-${tag}.tar

# create offline documenation
mkdir ./BProbe/Documentation
./BProbe/release_helper/md2html.sh ./BProbe.wiki ./BProbe/Documentation

# append offline doc to tar
tar --append ./BProbe/Documentation -f ./BProbe-${tag}.tar

# remove offline documentation
rm -R ./BProbe/Documentation

# compress archive
gzip ./BProbe-${tag}.tar
