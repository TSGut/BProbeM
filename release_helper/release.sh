#!/bin/sh

# TODO: change version in "project.json"

# create a git tag $tag before doing this
tag="v0.1.1"

cd ../
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
