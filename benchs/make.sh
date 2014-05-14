#!/bin/sh

COMMIT=$( git log -n 1 | grep commit | cut -d ' ' -f 2 )

echo "target: commit $COMMIT"

TARGET=$(dirname $0)/${COMMIT}/
mkdir -p "${TARGET}"

for i in Axioms Problems bench_run.py utils ; do
    ln -s "../../$i" "${TARGET}/$i"
done

for i in benchs.ini zipperposition.native ; do
    cp "$i" "${TARGET}/$i"
done
