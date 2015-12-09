#!/bin/sh


#
# this script basically converts the github wiki into html files
#
source=`realpath ${1}`
target=`realpath ${2}`
rhelper=`realpath ./BProbe/release_helper/`
echo $rhelper

cd $source

for filename in *.md; do
	basename=$(basename $filename .md)

	echo "Converting ${filename} ..."
	
	title=$basename

	# treat "Home.*" different, i.e. rename to index.* and set a different title
	if [ $basename = "Home" ]
	then
		basename="index"
		title="BProbe Documentation"
	fi

	# add header to each file
	# convert md -> html
	echo "# $title" | cat - $filename | pandoc -f markdown_github -t html5 -o ${target}/${basename}.html -s --self-contained --css="${rhelper}/github-pandoc.css"

	# fix links
	${rhelper}/fix_links.py ${target}/${basename}.html
done
