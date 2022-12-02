. .cookie.sh

YEAR=${YEAR:-2022}
DIR=${DIR:-resources}
URL=https://adventofcode.com/"$YEAR"/day/"$1"/input
FILENAME=$(printf "%s/%d/Day%02d.txt" $DIR $YEAR $1)


echo "URL=$URL"
echo "FILENAME=$FILENAME"

curl -S -o "$FILENAME" -b "$AOC_COOKIE"  "$URL"
