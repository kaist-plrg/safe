#!/bin/bash

for v in `grep "@function" heap.shape | grep -o "#[a-zA-Z0-9.]*"`;do
	k=${v:1}.fun;
	if [ -f converted/$k.js ];then
		mv converted/$k.js checked;
	fi
done
for v in `grep "@construct" heap.shape | grep -o "#[a-zA-Z0-9.]*"`;do
	k=${v:1}.con;
	if [ -f converted/$k.js ];then
		mv converted/$k.js checked;
	fi
done

for v in checked/*.fun.js;do
	v=$(basename $v)
	v=${v%.*}
	v=${v%.*}
	name="#$v"
	new="\"$v.fun.js\""
	echo $name to $new
	sed -E "s/\"@function\":[[:space:]]$name,/\"@function\": $new,/g" heap.shape > heap.shape.progress
	mv heap.shape.progress heap.shape
done

for v in checked/*.con.js;do
	v=$(basename $v)
	v=${v%.*}
	v=${v%.*}
	name="#$v"
	new="\"$v.con.js\""
	echo $name to $new
	sed -E "s/\"@construct\":[[:space:]]$name,/\"@construct\": $new,/g" heap.shape > heap.shape.progress
	mv heap.shape.progress heap.shape
done
