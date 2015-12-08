#
# this script basically converts the github wiki into html files
#


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
	echo "# $title" | cat - $filename | pandoc -f markdown_github -t html5 -o ${basename}.html -s --self-contained --css=github-pandoc.css

	# fix links
	./fix_links.py ${basename}.html
done
