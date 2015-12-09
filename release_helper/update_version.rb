#!/usr/bin/ruby

require 'json'

json = JSON.load(open("../project.json"))
json["version"] = ARGV[0]

File.open("../project.json", 'w') { |file| file.write(JSON.pretty_generate(json)) }
