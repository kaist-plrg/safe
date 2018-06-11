#!/bin/bash

pushd $SAFE_HOME
mkdir src/main/resources/DOMModels/converted
mkdir src/main/resources/DOMModels/confirmed
rm src/main/resources/DOMModels/converted/*
rm src/main/resources/DOMModels/confirmed/*
cp src/main/resources/DOMModels/heap.shape.new src/main/resources/DOMModels/heap.shape

# extract js models
for v in Array.jsmodel Boolean.jsmodel String.jsmodel built_in.jsmodel;do
	bin/safe convert -ModelConvert:out=src/main/resources/DOMModels/converted src/main/resources/jsModels/built_in/$v
done

pushd src/main/resources/DOMModels
# check the mappings for extracted models.
for v in `grep "@function" heap.shape | grep -o "#[a-zA-Z0-9.]*"`;do
	k=${v:1}.fun;
	if [ -f converted/$k.js ];then
		mv converted/$k.js confirmed;
	fi
done
for v in `grep "@construct" heap.shape | grep -o "#[a-zA-Z0-9.]*"`;do
	k=${v:1}.con;
	if [ -f converted/$k.js ];then
		mv converted/$k.js confirmed;
	fi
done

# replace the existing models to the corresponding extracted models 
for v in confirmed/*.fun.js;do
	v=$(basename $v)
	v=${v%.*}
	v=${v%.*}
	name="#$v"
	new="\"confirmed\\/$v.fun.js\""
	sed -E "s/\"@function\":[[:space:]]$name,/\"@function\": $new,/g" heap.shape > heap.shape.progress
	mv heap.shape.progress heap.shape
done

for v in confirmed/*.con.js;do
	v=$(basename $v)
	v=${v%.*}
	v=${v%.*}
	name="#$v"
	new="\"confirmed\\/$v.con.js\""
	sed -E "s/\"@construct\":[[:space:]]$name,/\"@construct\": $new,/g" heap.shape > heap.shape.progress
	mv heap.shape.progress heap.shape
done

popd
popd
echo "* leftovers"
ls src/main/resources/DOMModels/converted

