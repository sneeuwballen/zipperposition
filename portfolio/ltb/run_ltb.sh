#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

timeout -k 9 $2 python3 $DIR/../portfolio.sh.sequential.py $1 $2 $3  
